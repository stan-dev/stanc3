open Std
open Middle
open Dataflow_types

(** {1 Purpose}

    Dependency analysis finds the earlier statements that each statement depends
    on. A statement can depend on an earlier statement in two ways.

    - A {e data dependency} exists when the earlier statement writes a value
      that this statement reads.
    - A {e control dependency} exists when the earlier statement decides whether
      this statement runs. The earlier statement is an [if] or loop around this
      statement (an {e enclosing} [if] or loop), or a [break], [continue] or
      [return] that can skip this statement (a {e jump}).

    The enclosing ifs and loops and the jumps are the {e controls} of a
    statement ({!Dataflow_types.control}). A statement has a direct control
    dependency only on the nearest enclosing [if] or loop, and on the jumps. The
    other enclosing ifs and loops are transitive dependencies (dependencies of a
    dependency). The direct ones are the {e control parents} of the statement
    ({!Dataflow_utils.control_parents}).

    Pedantic mode uses dependencies to warn when a parameter decides an [if] or
    a loop. The factor graph ({!Factor_graph}) uses them to connect each
    [target +=] term to the parameters and data that the term uses. A planned
    loop optimization will use them to decide which loops the optimization can
    vectorize (see {{!section-distances}Loop distances}). Optimizations do not
    use them yet.

    {1 Running example}

    The analysis runs on one statement, the {e analyzed statement}, and on the
    statements nested in the analyzed statement. The analysis gives each
    statement a {e label}, an integer that identifies the statement. A test in
    [test/unit/Dependence_analysis.ml] prints the labels and dependencies that
    this file quotes.
    {@stan[
      data { int N; vector[N] x; }
      parameters { real a; }
      model {
        matrix[N, 2] theta;
        for (n in 1:N) {
          theta[n, 1] = a;
          theta[n, 2] = x[n];
          if (theta[n, 2] > 0) target += 1;
        }
      }
    ]}
    In the examples, the analyzed statement is the {e log density}, the sum that
    [target] accumulates. The statements of the log density are in the
    parameters, transformed parameters and model blocks. The analyzed statement
    gets label [1], and the nested statements, blocks included, get the next
    labels in source order. The analysis keeps the statements in a map from each
    label to a {e statement pattern}. A statement pattern is the statement with
    each nested statement replaced by the label of the nested statement. For
    example, the [if] at key [10] refers to the body by key [11].
    {@text[
      map key   statement pattern
      1         block of 2, 3 (the whole log density)
      2         real a;
      3         block of 4, 5, 6 (the model block)
      4         FnValidateSize__("theta", "N", N);
      5         matrix[N, 2] theta;
      6         for (n in 1:N) 7
      7         block of 8, 9, 10
      8         theta[n, 1] = a;
      9         theta[n, 2] = x[n];
      10        if (theta[n, 2] > 0) 11
      11        target += 1;
    ]}
    Label [4] is [FnValidateSize__], a size check. The compiler adds this check
    before a sized declaration, such as [matrix[N, 2] theta].

    The table below lists the dependencies of the [if] and of [target += 1].
    {@text[
      if (theta[n, 2] > 0)  --data-->       matrix[N, 2] theta;
      if (theta[n, 2] > 0)  --data-->       theta[n, 2] = x[n]
      if (theta[n, 2] > 0)  --control-->    for (n in 1:N)
      target += 1           --control-->    if (theta[n, 2] > 0)
      target += 1           --transitive--> matrix[N, 2] theta;
      target += 1           --transitive--> theta[n, 2] = x[n]
      target += 1           --transitive--> for (n in 1:N)
    ]}
    The table leaves out the data [N] and [x], because the values of [N] and [x]
    come from outside the model block. Step 3 of {e Finding data dependencies}
    explains why the declaration of [theta] is a data dependency.

    {1 Finding data dependencies}

    A data dependency connects a read to each write that can supply the value
    read. The analysis finds these writes in three steps.

    {ol
     {- Step 1 of 3, record the elements: An {e access} is one read, write or
        {e increment} of a variable. An increment is an addition to [target], as
        in [target +=] or a call to an [_lp] function. {!Accesses} records the
        accesses of each statement, with the element that each access selects.
        Step 2 and step 3 need these elements to decide whether a write can
        supply a value to a read.

        A write or an increment can pass a value to a read, and a write can pass
        a value to an increment. A read of [target], as in [target()], depends
        on each earlier increment, because the read sees their sum. Two
        increments of [target] do not depend on each other, because the
        additions can occur in any order.
     }
     {- Step 2 of 3, find the writes that reach each read: Step 2 records each
        write as a {e definition}. A definition holds the name of the written
        variable and the label of the statement that writes the variable. A
        write {e reaches} a read if the program can go from the write to the
        read, and no other write overwrites the same element between them. The
        standard {e reaching definitions} analysis
        ({!Monotone_framework.reaching_definitions_mfp}) finds the definitions
        that reach the start of each statement (the {e entry} set) and the end
        of each statement (the {e exit} set).

        In the running example, the read is [theta[n, 2]] in the condition
        [if (theta[n, 2] > 0)]. Three writes reach the read. These writes are
        the declaration of [theta], [theta[n, 1] = a], and [theta[n, 2] = x[n]].
        Each of these writes is a {e candidate source} for the read, and the
        read is the {e sink}.

        The analysis removes a write when a later write always overwrites the
        same element. The later write {e kills} the earlier definition. For
        example, [theta[1] = 1] kills an earlier [theta[1] = a]. In the running
        example, [theta[n, 1] = a] writes column 1, and [theta[n, 2] = x[n]]
        writes column 2. Because the two writes change different elements,
        neither write overwrites the other. Thus both assignments stay candidate
        sources for the read.

        Step 2 never asks whether a write selects the element that the read
        selects. Step 2 only removes writes that a later write overwrites. So
        [theta[n, 1] = a] stays a candidate source, although that write never
        writes column 2. Step 3 handles this case.
     }
     {- Step 3 of 3, compare elements and order: {!Dependence} asks two
        questions about each candidate source and the read.
        - Question 1: Can the write select the element that the read selects?
          {!Dependence} compares the indices of the write with the indices of
          the read. If the numbers of indices differ, the comparison cannot
          decide, and the answer is yes.
        - Question 2: Can the write run before the read? Inside one loop, two
          accesses in the same iteration pass question 2 when the write comes
          first in the loop body.

        The analysis rejects the candidate source if the answer to a question is
        no. A rejection means the analysis has proved that the write can never
        supply the value the read uses, so the analysis drops that write from
        the data dependencies of the read. In the running example, the [if] no
        longer depends on [theta[n, 1] = a], and therefore not on the parameter
        [a]. The declaration writes the whole of [theta] with no indices. The
        read [theta[n, 2]] has two indices, so question 1 cannot decide.
        {@text[
          candidate source     question 1 (element)   question 2 (order)  result
          matrix[N, 2] theta;  cannot decide, so yes  yes, runs before    keep
                                                      the loop
          theta[n, 1] = a      no, column 1 is        not needed          reject
                               never column 2
          theta[n, 2] = x[n]   yes, same element      yes, same           keep
                                                      iteration, and the
                                                      write runs first
        ]}
     }
    }

    If the analysis kept [theta[n, 1] = a], pedantic mode would falsely warn
    that [a] decides the [if]. Question 1 and question 2 of step 3 reject a
    candidate source only when they prove that the source never supplies the
    value. Some cases are too hard for question 1 and question 2, and
    {!Dependence} lists these cases. In these cases the candidate source stays,
    so the result can contain extra dependencies. But the result never misses a
    dependency.

    {1:distances Loop distances}

    Question 2 of step 3 needs the execution order of a write and a read in a
    loop. {!Dependence} answers question 2 with a {e distance} for each
    dependency. The distance is the number of iterations from the write to the
    read that uses the written value. The words "same iteration" in the step 3
    table mean distance [0]. In the running example, each distance is [0]. Then
    no value passes from one iteration to another. Thus the iterations can run
    in any order, and a loop optimization can vectorize them. In
    [for (n in 2:N) b[n] = b[n - 1] + a;] the distance is [1], so the iterations
    must run in order. *)

(** {1 API reference}

    {!build_dep_info_map} builds a map with a {!node_dep_info} for each
    statement. The {e queries}, such as {!node_immediate_dependencies} and
    {!node_dependencies}, take this map and a label, and return the labels of
    the dependencies of the statement at that label. A statement {e uses} a
    variable when the statement reads or increments the variable. Some queries
    take a set of variables whose definitions the query follows, as [vars] in
    {!node_immediate_dependencies} or as an argument of
    {!node_vars_dependencies}. By default, [vars] holds each variable that the
    statement uses. The factor graph and pedantic mode pass
    {!node_vars_dependencies} the names in a factor or an expression. Some
    queries also take [blockers], the names whose definitions the query does not
    follow. For example, the factor graph passes data and parameter names as
    [blockers]. By default, [blockers] is empty. *)

(** The data that a dependency query needs about one statement. The fields hold
    the statements that can run immediately before this statement, the controls
    of the statement, the definitions that reach the statement, and the accesses
    in the expressions of the statement.

    The example record below is for the condition (label [10]) of the running
    example. The record shows only the entry definitions of [theta].
    [reaching_definitions.entry] contains [Some 8] ([theta[n, 1] = a]), because
    step 2 of {e Finding data dependencies} keeps that write. Step 3 rejects
    that write later, when a query runs in {!node_immediate_dependencies}.
    {@text[
      predecessors:        {9}         (theta[n, 2] = x[n])
      controls:            [Enclosing_loop (For {id = 6; var = "n"})]
      reaching_definitions.entry:
                           {("theta", Some 5), ("theta", Some 8),
                            ("theta", Some 9)}
      accesses:            R theta[n, 2]
    ]} *)
type node_dep_info =
  { predecessors: label Set.Poly.t
        (** The labels of the statements that can run immediately before this
            statement. *)
  ; controls: control list
        (** The controls of this statement ({!Dataflow_types.control}). The list
            holds the enclosing ifs and loops, outermost first, and then the
            jumps. A jump is an earlier [continue] or [return] that can skip
            this statement, or, for a loop, a [break] or [return] in the loop
            body that can end the loop. A loop statement does not list itself.
            {!Dataflow_utils.control_parents} and {!Dataflow_utils.loops_around}
            give the two views that queries use. *)
  ; reaching_definitions:
      reaching_defn Set.Poly.t Monotone_framework_sigs.entry_exit
        (** The result of the reaching definitions analysis for this statement.
            [entry] holds the definitions that can reach the start of the
            statement, and [exit] holds those that can reach the end. Each
            definition is a (variable name, label) pair. [Some label] is a
            statement in the map. [None] is a definition from outside the
            analyzed statement, such as data. *)
  ; accesses: Accesses.index Accesses.t
        (** The reads, writes and increments in the expressions of this
            statement. Each nested statement has a separate record. *)
  ; meta: Location_span.t  (** The source location for pedantic warnings. *) }

(** Maps the label of each statement to the statement pattern and the
    {!node_dep_info} of the statement. {e Running example} shows the keys and
    statement patterns of the map that {!log_prob_build_dep_info_map} builds.
    Label [1] is the analyzed statement, the root of the map. Queries also use
    label [1] to stand for definitions from outside the analyzed statement, such
    as data. *)
type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

(** Maps the label of each statement to the labels of all the statements that
    the statement depends on, directly or transitively. *)
type dependency_graph = label Set.Poly.t LabelMap.t

val build_dep_info_map : Program.Typed.t -> Stmt.Located.t -> dep_info_map
(** Builds the map for the statement (label [1]) and the statements nested in
    the statement. The function has five steps. *)

val log_prob_build_dep_info_map : Program.Typed.t -> dep_info_map
(** Builds the map for the statements of the log density. These statements are
    in the parameters, transformed parameters and model blocks. *)

val reaching_defn_lookup :
  reaching_defn Set.Poly.t -> string -> label option Set.Poly.t
(** Returns the labels in the definitions of the named variable. The lookup
    compares names only and ignores which element each definition writes. *)

val flows_into : 'index Accesses.access -> 'index Accesses.access -> bool
(** Whether the first access (the source) can pass a value to the second (the
    sink) by the rules in step 1, comparing only variable names and kinds. *)

val node_immediate_dependencies :
     dep_info_map
  -> ?blockers:string Set.Poly.t
  -> ?vars:string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Returns the control parents of the statement at the label, and each
    statement whose write can supply the value of [vars] used there. *)

val node_dependencies : dep_info_map -> label -> label Set.Poly.t
(** Returns the labels of the direct and transitive dependencies of the
    statement at the label. The result also includes the label. *)

val node_vars_dependencies :
     dep_info_map
  -> ?blockers:string Set.Poly.t
  -> string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Like {!node_dependencies}, but the first step follows only the control
    parents and the definitions of the variables in the given set. *)

val all_node_dependencies : dep_info_map -> dependency_graph
(** Returns the direct and transitive dependencies of each statement. Unlike in
    {!node_dependencies}, the set of a statement leaves out that label. *)

val log_prob_dependency_graph : Program.Typed.t -> dependency_graph
(** Returns {!all_node_dependencies} for the statements of the log density. *)

val read_variables_at : dep_info_map -> label Set.Poly.t -> string Set.Poly.t
(** Returns the variables that the statements at the labels use, without loop
    variables. For a loop or [if], only the bounds or condition count. *)

(* TODO: [reaching_definitions_mfp] takes the whole program but needs only the
   names of variables defined before the analyzed statement. *)

(* TODO: Cache the results of [node_immediate_dependencies] and
   [reaching_defn_lookup] per map. Each query computes them again. *)
