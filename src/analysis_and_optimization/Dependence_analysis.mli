open Std
open Middle
open Dataflow_types

(** {1 Purpose and compiler uses}

    Dependency analysis identifies the inputs that affect a statement. The Stan
    compiler uses the identified inputs for pedantic checks and factor-graph
    construction. Pedantic mode checks whether a parameter affects an [if]
    condition or loop. The factor graph connects each [target +=] term to the
    parameters and data used to compute the term.

    Both uses require following values through assignments and conditions.
    Dependence Analysis first gathers possible value sources and the statements
    that control execution. A query then follows the dependencies and collects
    the input names needed by the compiler.

    The examples below use the following statements to follow an [if] condition
    through preparation, dependency checks and the final parameter check. The
    assignments supply values, and the [if] controls whether the target
    increment executes. Analyze a block containing the following statements.
    Assume [a] is a parameter and [theta] is a local vector declared before the
    block:
    {@stan[
      theta[1] = a;
      theta[2] = 1;
      if (theta[2] > 0) target += 1;
    ]}

    {1 Identify value sources and controlling statements}

    A statement can depend on values supplied by assignments and on control flow
    that determines whether the statement executes. Following both relationships
    accounts for computed values and execution conditions.

    A {e data dependency} is a statement whose write can supply a value read by
    another statement. In the example, the condition [theta[2] > 0] has a data
    dependency on [theta[2] = 1] because the condition reads the assigned
    element.

    A {e control dependency} is a statement that controls whether another
    statement executes, such as an enclosing [if] or loop. In the example,
    [target += 1] has a control dependency on the [if] statement because the
    increment executes only when [theta[2] > 0] is true.

    Both relationships identify direct dependencies. Following dependencies of
    the supplying or controlling statements finds transitive dependencies. For
    [target += 1], following the condition's data dependency also reaches
    [theta[2] = 1].

    The resulting relationships are shown below. Each arrow points from the
    dependent statement to the statement on which execution or a read depends:
    {@text[
      if (theta[2] > 0)  --data-->       theta[2] = 1
      target += 1       --control-->    if (theta[2] > 0)
      target += 1       --transitive--> theta[2] = 1
    ]}

    The selected element matters when finding value sources. The assignment
    [theta[1] = a] writes a different element from the condition's read of
    [theta[2]] and cannot supply the read. Comparing variable names alone would
    introduce a false dependency on the parameter [a].

    {1 Prepare statement information}

    Before checking the condition, analysis gathers possible sources of values
    and the information needed to compare them. Each statement receives an
    identifier so a query can locate assignments and follow dependencies.

    Analysis records the statements that can precede each statement and the
    statements that control execution. To trace values through the preceding
    statements, analysis records possible sources called
    {e reaching definitions}. The initial candidates are found by variable name;
    element comparisons happen later.

    Analysis also records each statement's reads, writes and increments,
    including the selected elements and tuple fields. Comparing the recorded
    indices requires knowing which index inputs can change. Enclosing loops
    identify inputs that change between iterations. Writes across the analyzed
    statements identify other inputs that cannot be treated as invariant.

    The prepared information connects candidate sources to the elements written
    and read. In the example, both assignments to [theta] are candidates until
    the query compares their writes with the condition's read.

    The following view of the stored condition record shows the information
    available after preparation. Only candidate definitions of [theta] are
    shown:
    {@text[
      statement:                  if (theta[2] > 0)
      preceding statement:        theta[2] = 1
      controlling statements:     none
      enclosing loops:            none
      candidate sources at entry: theta[1] = a; theta[2] = 1
      candidate sources at exit:  theta[1] = a; theta[2] = 1
      reads:                      theta[2], constant index 2
      writes and increments:      none
    ]}
    The assignment records store each assignment's reads and prepared write. The
    query will compare those writes with the condition's read:
    {@text[
      statement       stored reads   stored write   prepared index
      theta[1] = a    a              theta[1]       constant 1
      theta[2] = 1    none           theta[2]       constant 2
    ]}
    The target increment's record stores the [if] statement as a controlling
    statement and an increment of the log-density accumulator.

    {1 Find direct dependencies}

    Preparation identifies candidates; dependency checks determine which
    candidates can supply a value. A candidate remains only when the analysis
    cannot rule out the required value flow.

    For each variable read, the query compares candidate writes with the read.
    The accesses must permit a value flow, select the same element or field, and
    allow the write to execute before the read. Inside a loop, the write can
    occur in an earlier iteration.

    In the example, the write to [theta[1]] is rejected because the condition
    reads [theta[2]]. The assignment [theta[2] = 1] remains a data dependency.
    Adding the condition as a control dependency of the target increment
    accounts for whether the increment executes.

    The candidate checks produce the following results for the condition:
    {@text[
      candidate       element check    result
      theta[1] = a    1 differs from 2  reject
      theta[2] = 1    2 matches 2       keep
    ]}
    The resulting direct dependencies refer to statements. The following view
    names the statements in each dependency set:
    {@text[
      dependent statement     direct dependencies
      if (theta[2] > 0)       {theta[2] = 1}
      target += 1            {if (theta[2] > 0)}
    ]}

    Some comparisons lack enough information to reject a candidate. Keeping
    uncertain candidates preserves possible dependencies, although the result
    can include assignments that never supply a read.

    {1 Follow dependencies to program inputs}

    Direct dependencies identify supplying assignments and controlling
    statements. The compiler follows those dependencies until the visited
    statements reveal the program inputs used in the computation.

    For the original condition, traversal visits the condition and its supplying
    assignment. Collecting reads from those statements and comparing the read
    names with the parameter names produces:
    {@text[
      visited statements:   if (theta[2] > 0); theta[2] = 1
      collected read names: {theta}
      parameter names:      {a}
      parameters found:     {}
    ]}
    The supplying assignment reads no parameter, so the condition has no
    dependency on [a]. Changing only the condition to [theta[1] > 0] makes
    traversal follow [theta[1] = a] to the external input [a]. The resulting
    names now reveal a parameter dependency:
    {@text[
      collected read names: {theta, a}
      parameter names:      {a}
      parameters found:     {a}
    ]}

    Pedantic mode compares the collected input names with parameter names. The
    factor graph starts from the reads in a density term and follows the same
    process. Traversal stops at data and parameters so the graph connects the
    term to program inputs. For [target += 1], the term itself reads no inputs,
    but traversal follows the controlling condition. The original condition
    yields no connection to [a]; the changed condition yields a connection from
    the term to [a]. *)

(** {1 API reference}

    The declarations below specify how statement information is stored and how
    callers build records, select candidates and query dependencies.

    {2 Statement information}

    Statement records hold the facts needed to identify dependencies. A map
    locates records by statement label; a graph stores dependency sets. *)

(** The facts used by dependency queries for one statement. {!dep_info_map}
    stores the facts with the statement pattern under the statement's label.

    The following excerpt shows the fields used to check a condition reading
    [theta[2]]. Assume [theta[1] = a] and [theta[2] = 1] have labels [2] and
    [3], and the condition has label [4]. Only definitions of [theta] are shown:
    {@text[
      node_dep_info for statement 4
        predecessors:        {3}      (theta[2] = 1)
        parents:             {}       (no enclosing if or loop)
        loops:               []       (no enclosing loops)
        reaching_defn_entry: {("theta", Some 2), ("theta", Some 3)}
        reaching_defn_exit:  {("theta", Some 2), ("theta", Some 3)}
        accesses:            Read theta[2], with constant index 2
    ]}
    A query uses the candidate labels to locate the assignments, then compares
    their writes with the read in [accesses]. The field contracts below describe
    the remaining control-flow, definition and source-location information. *)
type node_dep_info =
  { predecessors: label Set.Poly.t
        (** Labels of statements that can execute immediately before the
            recorded statement. *)
  ; parents: label Set.Poly.t
        (** Labels of statements that control whether the recorded statement
            executes. Includes the nearest enclosing [if] or loop, and earlier
            [continue] or [return] statements that can skip the recorded
            statement. For a loop, [parents] also includes [break] or [return]
            statements in the loop body that can end the loop. *)
  ; reaching_defn_entry: reaching_defn Set.Poly.t
        (** Candidate definitions at statement entry, stored as pairs of
            variable names and optional definition labels. [None] identifies an
            external definition; [Some label] identifies a statement in the map.
            Subscripts are not considered. *)
  ; reaching_defn_exit: reaching_defn Set.Poly.t
        (** Candidate definitions at statement exit, in the same form as
            [reaching_defn_entry]. *)
  ; loops: Dependence.loop list
        (** Enclosing [for] and [while] loops, outermost first. *)
  ; accesses: Accesses.index Accesses.t
        (** Accesses made by the statement itself. Nested statements have
            separate access records. *)
  ; meta: Location_span.t  (** The source location used in pedantic warnings. *)
  }

(** Stores statement patterns and {!node_dep_info} records by label. Includes
    the analyzed statement and all nested statements. Patterns refer to child
    statements by label; for example, a [for] pattern holds the body's label.

    The following diagram shows how map keys locate statements and records. The
    enclosing block contains two assignments and an [if] with a target
    increment:
    {@text[
      map key   statement pattern                node_dep_info
      1         block containing 2, 3, 4         info1
      2         theta[1] = a                     info2
      3         theta[2] = 1                     info3
      4         if (theta[2] > 0) then 5          info4
      5         target += 1                      info5
    ]}
    Key [4] locates the condition's pattern and [info4] record. The condition's
    pattern refers to the target increment at key [5]. [info5.parents] contains
    [4] because the condition controls whether the increment executes. *)
type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

(** Dependency queries produce a graph mapping each statement label to the
    labels of the statement's direct and transitive dependencies. *)
type dependency_graph = label Set.Poly.t LabelMap.t

(** {2 Map builders}

    Builders select the statements to analyze and prepare records for queries.
    The analyzed region can be one statement or the program's log density. *)

val build_dep_info_map : Program.Typed.t -> Stmt.Located.t -> dep_info_map
(** Builds records for the given statement and all nested statements. The
    program's input and output variable names initialize definitions from
    outside the analyzed statement. External definitions have label [None];
    statements in the map have labels [Some label].

    Numbering starts at [1] for the given statement, then visits nested
    statements in source order. Blocks receive labels because a block is also a
    statement. Labels join the facts collected by the following stages:

    + Build the labeled statement patterns and source locations.
    + Compute control flow and reaching definitions, then find enclosing loops.
      Control flow supplies [predecessors] and [parents]. Definitions supply
      [reaching_defn_entry] and [reaching_defn_exit]; enclosing loops supply
      [loops].
    + Collect each statement's accesses with {!Accesses.of_stmt}. Combine
      written variable names across all access lists, including declarations and
      increments.
    + Prepare each access list with {!Accesses.classify}, using the combined
      write set and the statement's enclosing loop variables.
    + Store the pattern and completed {!node_dep_info} under each label.

    Declarations and writes introduce reaching definitions. A whole-variable
    assignment replaces earlier definitions of that variable. Indexed
    assignments keep earlier definitions as candidates because other elements
    can retain their values. The builder records candidates by name; queries
    perform element and execution-order checks. For the running example, the
    resulting map has the five entries shown in {!dep_info_map}; the condition's
    stored fields are shown in {!node_dep_info}. *)

val log_prob_build_dep_info_map : Program.Typed.t -> dep_info_map
(** Applies {!build_dep_info_map} to the program's [log_prob] statements:
    parameters, transformed parameters and the model block. *)

(** {2 Candidate checks}

    Candidate selection starts with variable names. Access-kind checks identify
    write/read pairs eligible for element and execution-order comparisons. *)

val reaching_defn_lookup :
  reaching_defn Set.Poly.t -> string -> label option Set.Poly.t
(** Returns optional definition labels for the named variable. [None] identifies
    an external definition. Lookup selects definitions by variable name, so
    subscripts do not affect the result. For example, a lookup for [theta] in
    [{("theta", Some 2); ("theta", Some 3); ("a", None)}] returns
    [{Some 2; Some 3}]. *)

val flows_into : 'index Accesses.access -> 'index Accesses.access -> bool
(** Tests whether the access kinds permit a value to flow from the source (first
    argument) to the sink (second argument). {!flows_into} returns false when
    the source and sink name different variables.

    For matching names, a [Write] or [Increment] can supply a [Read]. A [Write]
    can also supply an [Increment]. All other access-kind pairs cause
    {!flows_into} to return false.

    Two increments are treated as a reduction: [target += b] does not depend on
    an earlier [target += a].

    For example, comparing [W theta[1]] with [R theta[2]] returns true because
    the names and access kinds permit a flow. Comparing two [Increment] accesses
    to [target] returns false.

    An allowed pair of access kinds still needs an element and execution-order
    check. {!Dependence} performs the element and execution-order checks. *)

(** {2 Dependency queries}

    Queries return dependency labels for a statement or selected variables.
    Graph queries collect results for every statement. Read-name collection
    supplies the inputs needed by the parameter check or factor graph. *)

val node_immediate_dependencies :
  dep_info_map -> ?blockers:string Set.Poly.t -> label -> label Set.Poly.t
(** Returns labels of the statement's direct dependencies. Includes [parents]
    and definitions that can supply values read or incremented by the statement.
    Reads of variables in [blockers] are not followed; the default is an empty
    set.

    {!reaching_defn_lookup} selects candidates by variable name. External
    definitions remain possible sources and contribute the root label [1]. A
    candidate without a recorded write or increment is also kept by name; a
    [for] defining its loop variable is one such candidate.

    Candidates with recorded writes or increments undergo access comparisons.
    {!flows_into} checks names and kinds, {!Dependence.compare_paths} checks
    elements, and {!Dependence.may_exist} checks execution order. A candidate
    survives if any access pair passes all three checks.

    For the map shown in {!dep_info_map}, querying label [4] returns [{3}], the
    assignment to [theta[2]]. Querying label [5] returns [{4}], the controlling
    [if] statement. *)

val node_dependencies : dep_info_map -> label -> label Set.Poly.t
(** Returns dependency labels by following {!node_immediate_dependencies}
    transitively from the given label. The result includes the starting label.

    The following calls show how to obtain input names for the condition in the
    {!dep_info_map} example. [program] supplies the compiler's program
    representation, and [block] contains the statements shown in the map:
    {[
    let statement_map = build_dep_info_map program block
    let dependency_labels = node_dependencies statement_map 4
    let read_names = read_variables_at statement_map dependency_labels
    ]}
    The result is [dependency_labels = {3; 4}] and [read_names = {"theta"}].
    Comparing [read_names] with the parameter names [{"a"}] gives an empty set,
    so the condition has no parameter dependency. *)

val node_vars_dependencies :
     dep_info_map
  -> ?blockers:string Set.Poly.t
  -> string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Returns dependency labels for the requested variables at the starting
    statement. The first step follows definitions of the requested variables and
    the statement's [parents]. If the statement does not read a requested
    variable, all reaching definitions of that variable are kept by name.

    From the initial dependencies, traversal follows all reads, increments and
    [parents] at each visited statement. The starting label is included only if
    traversal returns to the starting statement. Definitions of variables in
    [blockers] are not followed at any step; the default is an empty set.

    The factor graph passes data and parameter names as [blockers] to stop
    traversal at program inputs. The caller combines names read at the returned
    labels with the density term's own read names because the starting statement
    can be absent from the result. Combining both sets preserves the term's
    inputs. *)

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

(* TODO: Cache immediate dependencies and reaching-definition lookups per map.
   [node_dependencies_rec] recomputes immediate dependencies on each query;
   [reaching_defn_lookup] scans the whole set for each variable. *)
