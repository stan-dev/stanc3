open Std
open Middle
open Dataflow_types

(** {1 Purpose}

    Dependency analysis finds the earlier statements that each statement depends
    on. A statement can depend on an earlier statement in two ways.

    - A {e data dependency} exists when the earlier statement writes a value
      that this statement reads.
    - A {e control dependency} exists when the earlier statement decides whether
      this statement runs. The earlier statement is an enclosing [if] or loop,
      or a [break], [continue] or [return] that can skip this statement.

    Pedantic mode uses dependencies to warn when a parameter decides an [if] or
    a loop. The factor graph uses them to connect each [target +=] term to its
    parameters and data. A planned loop optimization will use them to decide
    which loops it can vectorize (see {{!section-distances}Loop distances}). No
    optimization uses them yet.

    {1 Running example}

    The analysis gives each statement a {e label}, an integer that identifies
    it. A test in [test/unit/Dependence_analysis.ml] prints the labels and
    dependencies that this file quotes.
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
    The table below lists the dependencies of the [if] and of [target += 1]. A
    transitive dependency is a dependency of a dependency.
    {@text[
      if (theta[n, 2] > 0)  --data-->       matrix[N, 2] theta;
      if (theta[n, 2] > 0)  --data-->       theta[n, 2] = x[n]
      if (theta[n, 2] > 0)  --control-->    for (n in 1:N)
      target += 1           --control-->    if (theta[n, 2] > 0)
      target += 1           --transitive--> matrix[N, 2] theta;
      target += 1           --transitive--> theta[n, 2] = x[n]
      target += 1           --transitive--> for (n in 1:N)
    ]}
    The table leaves out the definitions of the data [N] and [x], because they
    come from outside the model block. Step 3 of {e Finding data dependencies}
    explains why the declaration of [theta] is a data dependency.

    {1 Finding data dependencies}

    A data dependency connects a read to each write that can supply the value
    that it reads. The analysis finds these writes in three steps.

    {ol
     {- Step 1 of 3, record the elements: {!Accesses} records the element that
        each write and read selects. Step 2 and step 3 need these elements to
        decide whether a write can supply a value to a read.
     }
     {- Step 2 of 3, reaching definitions: The reaching definitions analysis
        records each write as a {e definition}. A definition holds the name of
        the written variable and the label of the statement that writes it. The
        analysis finds the writes that {e reach} each read. A write reaches a
        read if the program can go from the write to the read, and no other
        write overwrites the same element between them.

        In the running example, the read is [theta[n, 2]] in the condition
        [if (theta[n, 2] > 0)]. Three writes reach the read. These writes are
        the declaration of [theta], [theta[n, 1] = a], and [theta[n, 2] = x[n]].
        They are the candidate sources for the read.

        The analysis removes a write when a later write always overwrites the
        same element. For example, [theta[1] = 1] removes an earlier
        [theta[1] = a]. In the running example, [theta[n, 1] = a] writes column
        1, and [theta[n, 2] = x[n]] writes column 2. Because the two writes
        change different elements, neither write overwrites the other. Thus both
        assignments stay candidate sources for the read.

        Step 2 never asks whether a write selects the element that the read
        selects. It only removes writes that a later write overwrites. So
        [theta[n, 1] = a] stays a candidate source, although it never writes
        column 2. Step 3 exists to close this gap.
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
        no. "Reject" means the analysis has proved that the write can never
        supply the value the read uses, so it drops that write from the read's
        data dependencies. i.e. in our example the [if] no longer depends on
        [theta[n, 1] = a], and therefore not on the parameter [a]. The
        declaration writes the whole of [theta] with no indices. The read
        [theta[n, 2]] has two indices, so question 1 cannot decide.
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
    candidate source only when they prove that it never supplies the value. Some
    cases are too hard for question 1 and question 2, and {!Dependence} lists
    these cases. In these cases the candidate source stays, so the result can
    contain extra dependencies. But the result never misses a dependency.

    {1:distances Loop distances}

    Question 2 of step 3 needs the execution order of a write and a read in a
    loop. {!Dependence} answers question 2 with a {e distance} for each
    dependency. The distance is the number of iterations from the write to the
    read that uses its value. The words "same iteration" in the step 3 table
    mean distance [0]. In the running example, each distance is [0]. Then no
    value passes from one iteration to another. Thus the iterations can run in
    any order, and a loop optimization can vectorize them. In
    [for (n in 2:N) b[n] = b[n - 1] + a;] the distance is [1], so the iterations
    must run in order. *)

(** {1 API reference} *)

(** The data that a dependency query needs about one statement. The fields hold
    the statements that run immediately before this statement, and the
    statements that control it. These are its enclosing ifs and loops, and the
    jumps that can skip it. They also hold the definitions that reach this
    statement and the accesses of its expressions.

    The example record below is for the condition (label [10]) of the running
    example. It shows only the entry definitions of [theta].
    [reaching_definitions.entry] contains [Some 8] ([theta[n, 1] = a]), because
    step 2 of {e Finding data dependencies} keeps that write. Step 3 rejects it
    later, when a query runs in {!node_immediate_dependencies}.
    {@text[
      predecessors:        {9}         (theta[n, 2] = x[n])
      controls:            [Enclosing_loop {id = 6; var = Some "n"}]
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
        (** The statements that control this statement
            ({!Dataflow_types.control}). The list holds the enclosing ifs and
            loops, outermost first, and then the jumps. A jump is an earlier
            [continue] or [return] that can skip this statement, or, for a loop,
            a [break] or [return] in its body that can end the loop. A loop
            statement does not list itself. {!Dataflow_utils.control_parents}
            and {!Dataflow_utils.loops_around} give the two views that queries
            use. *)
  ; reaching_definitions:
      reaching_defn Set.Poly.t Monotone_framework_sigs.entry_exit
        (** The result of the reaching definitions analysis for this statement.
            [entry] holds the definitions that can reach the start of the
            statement, and [exit] holds those that can reach its end. Each
            definition is a (variable name, label) pair. [Some label] is a
            statement in the map. [None] is a definition from outside the
            analyzed statement, such as data. *)
  ; accesses: Accesses.index Accesses.t
        (** The reads, writes and increments in the expressions of this
            statement. Each nested statement has its own record. *)
  ; meta: Location_span.t  (** The source location for pedantic warnings. *) }

(** Maps the label of each statement to its statement pattern and its
    {!node_dep_info}. A {e statement pattern} is the statement with each child
    statement replaced by the label of the child. For example, the [if] at key
    [10] refers to its body by key [11].

    The {e log density} is the sum that [target] accumulates. Its statements are
    in the parameters, transformed parameters and model blocks.
    {!log_prob_build_dep_info_map} builds this map from the statements of the
    log density in the running example.
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
    - Label [1] is the root statement of the map. Queries also use label [1] to
      stand for definitions from outside the analyzed statement, such as data
      (see {!node_immediate_dependencies} and {!node_dependencies}).
    - Label [4] is [FnValidateSize__], a size check. The compiler adds this
      check before a sized declaration, such as [matrix[N, 2] theta]. *)
type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

(** Maps the label of each statement to the labels of all the statements that it
    depends on, directly or transitively. *)
type dependency_graph = label Set.Poly.t LabelMap.t

val build_dep_info_map : Program.Typed.t -> Stmt.Located.t -> dep_info_map
(** Builds the map for the given statement and each statement nested in it. The
    given statement gets label [1]. The nested statements, blocks included, get
    the next labels in source order. The function has five steps. *)

val log_prob_build_dep_info_map : Program.Typed.t -> dep_info_map
(** Builds the map for the statements of the log density. These statements are
    in the parameters, transformed parameters and model blocks. *)

val reaching_defn_lookup :
  reaching_defn Set.Poly.t -> string -> label option Set.Poly.t
(** Returns the labels of the definitions of the named variable. The lookup
    compares names only. For example, [theta] in
    [{("theta", Some 5); ("theta", Some 8); ("a", None)}] gives
    [{Some 5; Some 8}]. *)

val flows_into : 'index Accesses.access -> 'index Accesses.access -> bool
(** Whether the first access (the source) can pass a value to the second access
    (the sink). The check uses only the variable names and the access kinds. The
    names must be equal. An [Increment] is an addition to [target], as in
    [target +=] or a call to an [_lp] function. A [Write] or an [Increment] can
    pass a value to a [Read], and a [Write] can pass a value to an [Increment].
    A read of [target], as in [target()], depends on each earlier increment,
    because the read sees their sum. Two increments of [target] do not depend on
    each other, because the additions can occur in any order. {!Dependence}
    compares the elements later. *)

val node_immediate_dependencies :
     dep_info_map
  -> ?blockers:string Set.Poly.t
  -> ?vars:string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Returns the labels of the statements that this statement depends on
    directly. These are its control dependencies
    ({!Dataflow_utils.control_parents}), and each statement whose write can
    supply a value of [vars] that this statement reads or increments. By
    default, [vars] holds every variable that the statement reads or increments.
    [blockers] are names whose definitions the query does not follow. For
    example, the factor graph passes data and parameter names as [blockers]. By
    default, [blockers] is empty. *)

val node_dependencies : dep_info_map -> label -> label Set.Poly.t
(** Returns the labels of the direct and transitive dependencies of the given
    statement. The result also includes the given label.*)

val node_vars_dependencies :
     dep_info_map
  -> ?blockers:string Set.Poly.t
  -> string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Like {!node_dependencies}, but from the given statement it follows only its
    control dependencies and the definitions of the given variables. The factor
    graph and pedantic mode pass the names of a factor or an expression. *)

val all_node_dependencies : dep_info_map -> dependency_graph
(** Returns the direct and transitive dependencies of each statement. Unlike in
    {!node_dependencies}, a set does not contain its own label. *)

val log_prob_dependency_graph : Program.Typed.t -> dependency_graph
(** Returns {!all_node_dependencies} for the statements of the log density. *)

val read_variables_at : dep_info_map -> label Set.Poly.t -> string Set.Poly.t
(** Returns the names of the variables that the statements at the given labels
    read or increment. For a [for], [if] or [while], these are the reads in its
    bounds or condition, because the body has its own labels. Enclosing loop
    variables are not included, because {!Accesses.classify} removes them. *)

(* TODO: [reaching_definitions_mfp] takes the whole program but needs only the
   names of variables defined before the analyzed statement. *)

(* TODO: Cache the results of [node_immediate_dependencies] and
   [reaching_defn_lookup] per map. Each query computes them again. *)
