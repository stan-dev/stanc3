open Std
open Middle
open Dataflow_types

(** {1 Purpose}

    Dependence analysis finds the statements that each statement of a program
    depends on. A statement depends on another statement through the values that
    the two statements read and write, or through the statements that decide
    whether the statement runs. Knowing these dependencies allows for nice
    language specific correctness checks on a program and optimizations that
    reorder statements.

    In the stanc compiler, dependency analysis has two consumers. Pedantic mode
    ({!Pedantic_analysis}) uses dependency analysis to warn when the condition
    of an [if] or the bounds of a loop depend on a parameter. The factor graph
    ({!Factor_graph}) uses the same analysis to connect each [target +=] term to
    the parameters and data that the term uses.

    This documentation describes how dependency analysis works in stanc. Each
    section uses the running example below.

    {1 Running example}
    {@text[
       1  data {
       2    int N;
       3    vector[N] x;
       4  }
       5  parameters {
       6    real a;
       7  }
       8  model {
       9    matrix[N, 2] theta;
      10    for (n in 1:N) {
      11      theta[n, 1] = a;
      12      theta[n, 2] = x[n];
      13      if (theta[n, 2] > 0) {
      14        target += 1;
      15      }
      16    }
      17  }
    ]}

    The idea of dependency analysis is pretty simple on its own. Given we are
    looking at some statement in a program which statements does this statement
    depend on either via reads/writes or conditional execution. We call these
    dependence forms {e data dependence} or {e control dependence}.

    - A {e data dependence} exists when a write in the other statement can
      supply a value that this statement reads. The write runs before the read
      when the program runs.

    {@text[
      12      theta[n, 2] = x[n];       writes theta[n, 2]
      13      if (theta[n, 2] > 0) {    reads theta[n, 2]: data dependence on line 12
    ]}

    - A statement can also depend on another statement without reading a value
      from the other statement. A {e control dependence} exists when the other
      statement decides whether this statement runs. The other statement is
      control dependence if it is an [if] or a loop around this statement, or a
      [break], [continue], or [return] that can skip this statement.

    {@text[
      10    for (n in 1:N) {            decides whether lines 11 to 13 run
      11      theta[n, 1] = a;          control dependence on line 10
      12      theta[n, 2] = x[n];       control dependence on line 10
      13      if (theta[n, 2] > 0) {    control dependence on line 10; decides whether line 14 runs
      14        target += 1;            control dependence on line 13
    ]}

    A statement has a direct control dependence only on the innermost [if] or
    loop around the statement, and on the jumps that can skip the statement.
    These statements are the {e control parents} of the statement
    ({!Dataflow_utils.control_parent_ids}). The outer [if]s and loops around a
    statement are dependencies of the control parent. They are therefore
    transitive control dependencies of the statement.
    {@text[
      13      if (theta[n, 2] > 0) {    control parent of line 14
      14        target += 1;            direct: line 13   transitive: line 10, through line 13
    ]}

    The rest of this section shows how the analysis finds these dependencies for
    the running example. Dependence analysis reads a large group of statements
    at once, the {e analyzed code}, and then finds the dependencies between the
    statements in that group. For the log density, the analyzed code is the
    parameters, transformed parameters, and model blocks, which are lines 5 to
    17 in the running example.

    The analysis puts the analyzed code under one {e root}, which gets label
    [1], and gives each statement inside the root an integer {e label} that
    offsets it from root. The dependencies between the labeled statements form a
    graph whose nodes are the labels ({!dependence_graph}). Statements outside
    the analyzed code, such as the data block on lines 1 to 4, do not get a
    label.

    {@text[
     5  parameters {
     6    real a;
     7  }
     8  model {
     9    matrix[N, 2] theta;
    10    for (n in 1:N) {
    11      theta[n, 1] = a;
    12      theta[n, 2] = x[n];
    13      if (theta[n, 2] > 0) {
    14        target += 1;
    15      }
    16    }
    17  }
    ]}

    To build this graph, the analysis first records the statement at each label.
    The [statements] field of {!dependence_info} maps each label to the
    statement at that label. A statement that holds other statements refers to
    each nested statement by its label. For example, the [if] at label [13]
    holds the label [11] in place of its body [target += 1].

    The table below shows what the analysis builds for the running example. The
    analysis builds the table by walking the root depth first. The root gets
    label [1]. Each statement that the walk visits gets the next label, and the
    walk labels the statements nested inside a statement before it moves on to
    the next statement at the same level. For example, the loop on line 10 gets
    label [6], the braces of its body get label [7], and the three statements in
    the body get labels [8], [9], and [10]. So after running the initial
    labeling pass we can see that line 6 covers the for loop which holds the
    statements at labels 8, 9, and 10.

    The Stan lines column gives the lines of the running example that each
    statement comes from. The two dependency columns give the direct
    dependencies that {!direct_dependence_ids} returns: the [Data] labels, then
    the [Control] labels.
    {@text[
      label  Stan lines  statement                           data dependencies  control dependencies
      1      5-17        root { #2 #3 }                      -                 -
      2      6           real a;                             -                 -
      3      8-17        model { #4 #5 #6 }                  -                 -
      4      9           FnValidateSize__("theta", "N", N);  -                 -
      5      9           matrix[N, 2] theta;                 -                 -
      6      10-16       for (n in 1:N) #7                   -                 -
      7      10-16       Block { #8 #9 #10 }                 -                 6
      8      11          theta[n, 1] = a;                    2                 6
      9      12          theta[n, 2] = x[n];                 -                 6
      10     13-15       if (theta[n, 2] > 0) #11            5, 9              6
      11     13-15       Block { #12 }                       -                 10
      12     14          target += 1;                        -                 10
    ]}

    The [if] statement at label [10] has three direct dependencies.
    - Labels [5] and [9] are data dependencies. The condition [theta[n, 2] > 0]
      reads [theta[n, 2]]. The declaration [matrix[N, 2] theta] at label [5] and
      the assignment [theta[n, 2] = x[n]] at label [9] write values that this
      read can use.
    - Label [6] is a control dependence. The loop at label [6] is the innermost
      loop around the [if], and the loop decides whether the [if] runs.

    {1 Using the dependencies}

    The factor graph and pedantic mode both start at one statement and follow
    the dependencies in the table, then the dependencies of those statements.
    The tree below shows this walk from [target += 1] at label [12]. The right
    column lists the variables that each statement reads, leaving out loop
    variables. Note the numbers in the below are the {e label} and not the
    original stan program line numbers.
    {@text[
      label: 12  target += 1;
      `-- label: 10  if (theta[n, 2] > 0)     control
          |-- label: 9  theta[n, 2] = x[n];   data
          |   `-- label: 6  for (n in 1:N)    control
          |-- label: 5  matrix[N, 2] theta;   data
          `-- label: 6  for (n in 1:N)        control
          label: 8  theta[n, 1] = a;          reads a (not reached)
    ]}

    The factor graph ({!Factor_graph}) starts the walk at each factor, a term
    that changes the log density, such as [target += 1]. It records the data and
    parameters that the walk reads, here [N] and [x]. The walk never reaches
    label [8], so pedantic mode warns that the parameter [a] is not used in the
    density.

    Pedantic mode ({!Pedantic_analysis}) starts the same walk at each [if] and
    loop, and warns if the walk reads a parameter. From the [if] at label [10],
    the walk does not read [a]. If line 13 read [theta[n, 1]], the walk would
    reach label [8], and pedantic mode would warn that the [if] depends on [a].
*)

(** What a dependence query needs to know about one statement, apart from the
    reaching definitions.

    The example record below is for the condition (label [10]) of the running
    example.
    {@text[
      pattern:   if (theta[n, 2] > 0) 11
      controls:  {enclosing = [{id = 6; side = None}]; jump_ids = {}}
      accesses:  R theta[n, 2]
    ]} *)
type stmt_info =
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

(** The statements of the analyzed code and the definitions that reach each one.
    Both maps have the same labels. {e Running example} shows the labels and
    statement patterns that {!build_log_prob_dependence_info} gives. Label [1]
    ({!Dataflow_utils.root_statement_id}) is the root. The queries return
    statement labels only, so a definition from outside the analyzed code, such
    as data, never appears in a result. *)
type dependence_info =
  { statements: stmt_info LabelMap.t
  ; reaching_definitions: reaching_definition Set.Poly.t LabelMap.t
        (** The definitions that can reach the start of each statement. Each
            definition is a (variable name, label) pair. [Some label] is a
            statement in the map. [None] is a definition from outside the
            analyzed code, such as data. *) }

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

val common_loop_ids : stmt_info LabelMap.t -> label -> label -> label list
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

(** One direct dependence of a statement ({!direct_dependence_ids}). The label
    is the statement depended on. For the [if] at label [10] of the running
    example the set is [{Data 5; Data 9; Control 6}]. *)
type kind = Data of label | Control of label

val direct_dependence_ids :
     dependence_info
  -> ?blockers:string Set.Poly.t
  -> ?vars:string Set.Poly.t
  -> label
  -> kind Set.Poly.t
(** Returns the direct dependences of the statement at the label: [Control] for
    each control parent, and [Data] for each statement whose write can supply
    the value of [vars] used there. *)

val transitive_dependence_ids : dependence_info -> label -> label Set.Poly.t
(** Returns the labels of the direct and transitive dependencies of the
    statement at the label. The result also includes the label. *)

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
(** Returns the direct and transitive dependencies of each statement. Unlike in
    {!transitive_dependence_ids}, the set of a statement leaves out that label.
*)

val used_vars_at : dependence_info -> label Set.Poly.t -> string Set.Poly.t
(** Returns the variables that the statements at the labels use, without loop
    variables. For a loop or [if], only the bounds or condition count. *)
