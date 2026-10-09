open Std
open Middle
open Dataflow_types

(** {1 Purpose and compiler uses}

    [Dependence_analysis] finds statements that can affect another statement's
    execution or computed values. The Stan compiler uses the dependencies to
    trace effects back to program inputs. Pedantic mode finds control flow that
    depends on a parameter. The factor graph connects each [target +=] term to
    the parameters and data used to compute the term.

    Analysis has two stages. First, map construction records information about
    each statement. Dependency queries then use the records to find dependencies
    and trace the dependencies to program inputs.

    {1 Meaning of a statement dependency}

    A statement dependency represents a possible effect on execution or values.
    The distinction between control and data dependencies determines which
    statements a query follows.

    A {e control dependency} is a statement that controls whether another
    statement executes, such as an enclosing [if] or loop. A {e data dependency}
    is a statement whose write can supply a value read by another statement.
    Data dependencies also include writes that can supply an increment of
    [target].

    Direct dependencies identify the statements with an immediate effect.
    Transitive dependencies include statements reached by following direct
    dependencies through intermediate statements. For example, a condition can
    depend on a parameter through several assignments.

    The following example shows why a data dependency must account for the
    element read. Comparing only variable names introduces a false parameter
    dependency. Suppose [a] is a parameter:
    {@stan[
      theta[1] = a;
      theta[2] = 1;
      if (theta[2] > 0) target += 1;
    ]}
    The [if] condition reads the constant [1] assigned to [theta[2]]. The
    assignment [theta[1] = a] writes a different element and cannot supply the
    value read from [theta[2]]. A comparison by name would connect the condition
    to [theta[1] = a] and then to [a]. The element comparison removes the false
    dependency and avoids an unnecessary pedantic warning.

    {1 Information available for each statement}

    Dependency queries need candidate writes, access paths and control-flow
    information. A {!dep_info_map} stores the information in a {!node_dep_info}
    record for each statement. Integer {e labels} identify the statements and
    let queries look up the records.

    The control-flow information identifies statements that control execution
    and statements that can execute immediately before another statement. Access
    records identify reads, writes and increments, with paths describing the
    accessed elements or tuple fields. Enclosing-loop records let queries
    compare accesses across iterations.

    Candidate writes are recorded as {e reaching definitions}. A reaching
    definition is a write whose value can still be available when a statement
    executes. A program path connects the write to the statement without
    replacing the written value.

    Labels follow source order. Label [1] identifies the analyzed statement,
    such as a model block. Nested statements have separate labels and records.
    Dependency query results use the same labels.

    The reaching-definitions pass also records external definitions at label
    [1], including data defined before the analyzed statement. Queries retain
    external definitions because the map has no access records for external
    definitions.

    {1 Preparing the dependency map}

    Map construction gathers the statement information needed by later queries.
    The builder takes the analyzed statement and the program containing the
    statement. The program supplies variable names for definitions outside the
    analyzed statement.

    {!build_dep_info_map} prepares the map in four steps:

    + The builder assigns statement labels and collects control-flow information
      and enclosing loops.
    + {!Monotone_framework.reaching_definitions_mfp} finds candidate definitions
      by variable name.
    + {!Accesses.of_stmt} collects each statement's accesses. The builder
      gathers all written variable names, including declarations.
    + {!Accesses.classify} uses the written names and enclosing loop variables
      to classify indices. The builder stores the classified accesses with the
      other statement information.

    The reaching-definitions pass treats declarations and assignments as
    definitions. A whole-variable assignment removes earlier definitions of the
    variable. An indexed assignment keeps earlier definitions as candidates.
    Later queries use the access records to distinguish writes to different
    elements.

    {!build_dep_info_map} analyzes function bodies or other individual
    statements. {!log_prob_build_dep_info_map} analyzes the statements used to
    compute the program's log density. Both builders prepare records for
    dependency queries.

    {1 Finding direct dependencies}

    A direct-dependency query uses the map to find possible sources of the
    statement's inputs. Candidate writes are filtered by variable name, access
    kind, element overlap and execution order. The retained data dependencies
    are combined with statements that control execution.

    For each variable read or incremented, the query starts with reaching
    definitions of the variable. In an access pair, the candidate write is the
    source and the read or increment is the sink. When access records are
    available, each pair passes through three checks:

    + {!flows_into} checks the variable names and access kinds.
    + {!Dependence.depends} checks whether the access paths can overlap.
    + {!Dependence.may_exist} checks whether the source can execute before the
      sink.

    A candidate definition remains when a write can supply a read or increment.
    An inconclusive element or execution-order check keeps the candidate.
    Retaining unresolved candidates preserves possible effects on the statement.

    The following example shows how indexing and execution order rule out
    dependencies between statements in a loop. Matching the element is
    insufficient when the assignment executes after the read:
    {@stan[
      for (n in 1:(N - 1)) {
        c[n] = b[n + 1];
        d[n] = b[n];
        b[n] = a;
      }
    ]}
    In iteration [i], [d[n] = b[n]] reads [b[i]] before [b[n] = a] writes
    [b[i]]. The statement [c[n] = b[n + 1]] reads [b[i + 1]] in iteration [i].
    The assignment [b[n] = a] can write [b[i + 1]] only in iteration [i + 1].
    The read of [b[i + 1]] occurs earlier, in iteration [i]. Neither read can
    receive a value from [b[n] = a].

    {!node_immediate_dependencies} returns the retained definition labels
    together with the statement's control dependencies, recorded in [parents].
    The result gives the next statements to visit when tracing dependencies.

    {1 Tracing dependencies to program inputs}

    Transitive queries follow direct dependencies through intermediate
    statements. The visited labels identify statements that can contribute to
    execution or computed values. The names of variables read at the visited
    statements identify possible program inputs.

    {!node_dependencies} starts at a statement and follows all data and control
    dependencies. Pedantic mode queries each [if] and loop, then obtains
    variable names with {!read_variables_at}. Comparing the names with the
    parameter names identifies control flow that depends on a parameter.

    {!node_vars_dependencies} starts with selected variables at a statement.
    Subsequent steps follow all data and control dependencies of each visited
    statement. The optional [blockers] prevent traversal through definitions of
    specified variables. The factor graph uses data and parameters as blockers
    to stop tracing at program inputs.

    A {!dependency_graph} maps each statement label to its dependency labels.
    {!all_node_dependencies} produces the graph for every statement in a map.
    {!log_prob_dependency_graph} builds the map and graph for the program's
    log-density statements. Examples of query results are in
    [test/unit/Dependence_analysis.ml]. *)

(** {1 API reference}

    The types and functions below specify stored information, map construction
    and dependency queries. The contracts identify inputs, returned labels and
    traversal options needed to use the analysis.

    {2 Statement information}

    Statement records and maps hold the inputs to dependency queries. A
    dependency graph holds the query results as sets of labels. *)

(** Dependency information for one statement. *)
type node_dep_info =
  { predecessors: label Set.Poly.t
        (** The statements that can run immediately before the recorded
            statement. *)
  ; parents: label Set.Poly.t
        (** Labels of statements that control whether the recorded statement
            executes. Includes the nearest enclosing [if] or loop, and earlier
            [continue] or [return] statements that can skip the recorded
            statement. For a loop, [parents] also includes [break] or [return]
            statements in the loop body that can end the loop. *)
  ; reaching_defn_entry: reaching_defn Set.Poly.t
        (** Reaching definitions by variable name at statement entry. *)
  ; reaching_defn_exit: reaching_defn Set.Poly.t
        (** Reaching definitions by variable name at statement exit. *)
  ; loops: Dependence.loop list
        (** Enclosing [for] and [while] loops, outermost first. *)
  ; accesses: Accesses.index Accesses.t
        (** Accesses made by the statement itself. Nested statements have
            separate access records. *)
  ; meta: Location_span.t  (** The source location used in pedantic warnings. *)
  }

(** Stores statement patterns and {!node_dep_info} records by label. The map
    includes the analyzed statement and all nested statements. Each statement
    pattern refers to child statements by label. For example, a [for] pattern
    holds the label of the loop body. *)
type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

(** Dependency queries produce a graph mapping each statement label to the
    labels of the statement's direct and transitive dependencies. *)
type dependency_graph = label Set.Poly.t LabelMap.t

(** {2 Map builders}

    The builders select the statements to analyze and prepare their records.
    Each returned map can be passed to the dependency queries. *)

val build_dep_info_map : Program.Typed.t -> Stmt.Located.t -> dep_info_map
(** Builds records for the given statement and all nested statements. The
    program supplies names of variables defined outside the analyzed statement.
    External definitions are recorded at label [1]. *)

val log_prob_build_dep_info_map : Program.Typed.t -> dep_info_map
(** Applies {!build_dep_info_map} to the program's [log_prob] statements:
    parameters, transformed parameters and the model block. *)

(** {2 Candidate checks}

    The helpers look up definitions by name and check access kinds. Candidates
    that pass both checks still require element and execution-order checks. *)

val reaching_defn_lookup :
  reaching_defn Set.Poly.t -> string -> label Set.Poly.t
(** Returns the definition labels for the named variable in the given set.
    Subscripts are not considered. For [{(i, 4); (j, 9)}] and [j], the result is
    [{9}]. *)

val flows_into : 'index Accesses.access -> 'index Accesses.access -> bool
(** Tests whether the access kinds permit a value to flow from the source (first
    argument) to the sink (second argument). {!flows_into} returns false when
    the source and sink name different variables.

    For matching names, a [Write] or [Increment] can supply a [Read]. A [Write]
    can also supply an [Increment]. All other access-kind pairs cause
    {!flows_into} to return false.

    Two increments are treated as a reduction: [target += b] does not depend on
    an earlier [target += a].

    An allowed pair of access kinds still needs an element and execution-order
    check. {!Dependence} performs the element and execution-order checks. *)

(** {2 Dependency queries}

    Queries return direct dependencies, transitive dependencies or a graph for
    every statement. {!read_variables_at} converts sets of labels into variable
    names for checks on program inputs. *)

val node_immediate_dependencies :
  dep_info_map -> ?blockers:string Set.Poly.t -> label -> label Set.Poly.t
(** Returns labels of the statement's direct dependencies. Includes [parents]
    and definitions that can supply values read or incremented by the statement.
    Definitions are filtered by access path and execution order. Reads of
    variables in [blockers] are not followed; the default is an empty set. *)

val node_dependencies : dep_info_map -> label -> label Set.Poly.t
(** Returns dependency labels by following {!node_immediate_dependencies}
    transitively from the given label. The result includes the starting label.
*)

val node_vars_dependencies :
     dep_info_map
  -> ?blockers:string Set.Poly.t
  -> string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Returns dependency labels for the requested variables at the starting
    statement. The first step follows definitions of the requested variables and
    the starting statement's [parents]. If the starting statement does not read
    a requested variable, all reaching definitions of the requested variable are
    kept by name. No read access is available for a subscript comparison.

    From the initial dependencies, the traversal follows all reads, increments
    and [parents] at each visited statement. The starting label is included only
    if the traversal returns to the starting statement.

    Throughout the traversal, definitions of variables in [blockers] are not
    followed. The default is an empty set. *)

val all_node_dependencies : dep_info_map -> dependency_graph
(** Returns transitive dependencies for every statement. Each dependency set
    excludes the statement's own label. {!node_dependencies} includes the
    starting label. *)

val log_prob_dependency_graph : Program.Typed.t -> dependency_graph
(** Applies {!all_node_dependencies} to {!log_prob_build_dep_info_map}. *)

val read_variables_at : dep_info_map -> label Set.Poly.t -> string Set.Poly.t
(** Returns the variables read or incremented at the given labels. A [for]
    contributes reads from the loop bounds. An [if] or [while] contributes reads
    from the condition. Statements in a loop or branch body have separate labels
    and access records. Reads of enclosing loop variables have been removed by
    {!Accesses.classify}. *)

(* TODO: [reaching_definitions_mfp] takes the whole program but needs only the
   names of variables defined before the analyzed statement. *)

(* TODO: Keep external definitions distinct from the root statement.
   [Monotone_framework] already represents an external definition's label as
   [None]. Preserving the [label option] in [reaching_defn] would avoid the
   special case for label [1]. *)

(* TODO: Cache immediate dependencies and reaching-definition lookups per map.
   [node_dependencies_rec] recomputes immediate dependencies on each query;
   [reaching_defn_lookup] scans the whole set for each variable. *)
