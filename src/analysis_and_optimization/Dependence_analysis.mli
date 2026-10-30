open Std
open Middle
open Dataflow_types

(** {1 Purpose}

    Dependence analysis finds the earlier statements that each statement depends
    on. A statement can depend on an earlier statement in two ways.

    - A {e data dependence} exists when the earlier statement writes a value
      that this statement reads.
    - A {e control dependence} exists when the earlier statement decides whether
      this statement runs. The earlier statement is an [if] or loop around this
      statement (an {e enclosing} [if] or loop), or a [break], [continue] or
      [return] that can skip this statement (a {e jump}).

    The enclosing ifs and loops and the jumps are the {e controls} of a
    statement ({!Dataflow_types.controls}). A statement has a direct control
    dependence only on the nearest enclosing [if] or loop, and on the jumps. The
    other enclosing ifs and loops are transitive dependences (dependences of a
    dependence). The direct control dependencies are the {e control parents} of
    the statement ({!Dataflow_utils.control_parent_ids}).

    Pedantic mode uses dependences to warn when a parameter decides an [if] or a
    loop. The factor graph ({!Factor_graph}) uses them to connect each
    [target +=] term to the parameters and data that the term uses. A planned
    loop optimization will use them to decide which loops the optimization can
    vectorize (see {{!section-distances}Loop distances}). Optimizations do not
    use them yet.

    {1 Running example}

    The analysis runs on one statement, the {e analyzed statement}, and on the
    statements nested in the analyzed statement. The analysis gives each
    statement a {e label}, an integer that identifies the statement. A test in
    [test/unit/Dependence_analysis.ml] prints the labels and dependences that
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

    The table below lists the dependences of the [if] and of [target += 1].
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
    come from outside the model block. Step 3 of {e Finding data dependences}
    explains why the declaration of [theta] is a data dependence.

    {1 Finding data dependences}

    A data dependence connects a read to each write that can supply the value
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
        the data dependences of the read. In the running example, the [if] no
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
    so the result can contain extra dependences. But the result never misses a
    dependence.

    {1:distances Loop distances}

    Question 2 of step 3 needs the execution order of a write and a read in a
    loop. {!Dependence} answers question 2 with a {e distance} for each
    dependence. The distance is the number of iterations from the write to the
    read that uses the written value. The words "same iteration" in the step 3
    table mean distance [0]. In the running example, each distance is [0]. Then
    no value passes from one iteration to another. Thus the iterations can run
    in any order, and a loop optimization can vectorize them. In
    [for (n in 2:N) b[n] = b[n - 1] + a;] the distance is [1], so the iterations
    must run in order. *)

(** {1 API reference}

    {!build_dependence_info} builds a {!dependence_info}. The {e queries}, such
    as {!direct_dependence_ids} and {!transitive_dependence_ids}, take a
    {!dependence_info} and a label, and return the labels of the dependences of
    the statement at that label. A statement {e uses} a variable when the
    statement reads or increments the variable, and {e defines} a variable when
    the statement writes or increments the variable. Some queries take a set of
    variables whose definitions the query follows, as [vars] in
    {!direct_dependence_ids} or as an argument of {!var_dependence_ids}. By
    default, [vars] holds each variable that the statement uses. The factor
    graph and pedantic mode pass {!var_dependence_ids} the names in a factor or
    an expression. Some queries also take [blockers], the names whose
    definitions the query does not follow. For example, the factor graph passes
    data and parameter names as [blockers]. By default, [blockers] is empty. *)

(** What a dependence query needs to know about one statement, apart from the
    reaching definitions.

    The example record below is for the condition (label [10]) of the running
    example.
    {@text[
      pattern:   if (theta[n, 2] > 0) 11
      controls:  {enclosing = [{id = 6; side = None}]; jump_ids = {}}
      accesses:  R theta[n, 2]
    ]} *)
type statement =
  { pattern: (Expr.Typed.t, label) Stmt.Pattern.t
        (** The statement, with each nested statement replaced by the label of
            the nested statement. *)
  ; meta: Location_span.t  (** The source location for pedantic warnings. *)
  ; controls: controls
        (** The enclosing ifs and loops of this statement, outermost first, and
            the jumps that can skip the statement ({!Dataflow_types.controls}).
            A loop statement does not list itself.
            {!Dataflow_utils.control_parent_ids} and {!Dataflow_utils.loop_ids}
            give the two views that queries use. *)
  ; accesses: Accesses.access list
        (** The reads, writes and increments in the expressions of this
            statement. Each nested statement has a separate record. *) }

(** The statements of the analyzed statement and the definitions that reach each
    one. Both maps have the same labels. {e Running example} shows the labels
    and statement patterns that {!build_log_prob_dependence_info} gives. Label
    [1] ({!Dataflow_utils.root_statement_id}) is the analyzed statement, the
    root of the map. The queries return statement labels only, so a definition
    from outside the analyzed statement, such as data, never appears in a
    result.

    In the running example, the definitions of [theta] that reach the condition
    (label [10]) are [("theta", Some 5)], [("theta", Some 8)] and
    [("theta", Some 9)]. The set contains [Some 8] ([theta[n, 1] = a]), because
    step 2 of {e Finding data dependences} keeps that write. Step 3 rejects that
    write later, when a query runs in {!direct_dependence_ids}. *)
type dependence_info =
  { statements: statement LabelMap.t
  ; reaching_definitions: reaching_definition Set.Poly.t LabelMap.t
        (** The definitions that can reach the start of each statement. Each
            definition is a (variable name, label) pair. [Some label] is a
            statement in the map. [None] is a definition from outside the
            analyzed statement, such as data. *) }

val build_dependence_info : Program.Typed.t -> Stmt.Located.t -> dependence_info
(** Builds the statement records and the reaching definitions for the statement
    (label [1]) and the statements nested in the statement. *)

val build_log_prob_dependence_info : Program.Typed.t -> dependence_info
(** Builds the {!dependence_info} for the statements of the log density. These
    statements are in the parameters, transformed parameters and model blocks.
*)

val definition_ids :
  reaching_definition Set.Poly.t -> string -> label option Set.Poly.t
(** Returns the labels in the definitions of the named variable. The lookup
    compares names only and ignores which element each definition writes. *)

val is_flow_kind : Accesses.access -> Accesses.access -> bool
(** Whether the first access (the source) can pass a value to the second (the
    sink) by the rules in step 1, comparing only variable names and kinds. *)

val common_loop_ids : statement LabelMap.t -> label -> label -> label list
(** Returns the labels of the {e common loops} of two statements, the loops that
    contain both statements, outermost first. A loop statement counts as inside
    its own loop. *)

val dependence_between :
     dependence_info
  -> label * Accesses.access
  -> label * Accesses.access
  -> Dependence.t
(** Compares the elements that the first access (the source) and the second
    access (the sink) select, over the common loops of both statements. Each
    access comes paired with the label of the statement that makes the access.
*)

val is_flow_dependence :
  dependence_info -> label * Accesses.access -> label * Accesses.access -> bool
(** Whether a flow dependence can exist from the first access (the source) to
    the second (the sink), which means that the source can supply the value that
    the sink uses: {!is_flow_kind}, then
    {!Dependence.is_lexicographically_positive} on {!dependence_between}. *)

val direct_dependence_ids :
     dependence_info
  -> ?blockers:string Set.Poly.t
  -> ?vars:string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Returns the control parents of the statement at the label, and each
    statement whose write can supply the value of [vars] used there. *)

val transitive_dependence_ids : dependence_info -> label -> label Set.Poly.t
(** Returns the labels of the direct and transitive dependences of the statement
    at the label. The result also includes the label. *)

val var_dependence_ids :
     dependence_info
  -> ?blockers:string Set.Poly.t
  -> string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Like {!transitive_dependence_ids}, but the first step follows only the
    control parents and the definitions of the variables in the given set, and
    the result leaves out the label. *)

val dependence_graph : dependence_info -> label Set.Poly.t LabelMap.t
(** Returns the direct and transitive dependences of each statement. Unlike in
    {!transitive_dependence_ids}, the set of a statement leaves out that label.
*)

val used_vars_at : dependence_info -> label Set.Poly.t -> string Set.Poly.t
(** Returns the variables that the statements at the labels use, without loop
    variables. For a loop or [if], only the bounds or condition count. *)

(* TODO: [reaching_definitions_mfp] takes the whole program but needs only the
   names of variables defined before the analyzed statement. *)

(* TODO: Cache the results of [direct_dependence_ids] and [definition_ids] per
   map. Each query computes them again. *)
