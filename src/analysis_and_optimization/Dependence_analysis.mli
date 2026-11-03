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

    {1 The statement map}

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
    {!stmt_info} record of that statement. A statement that holds other
    statements refers to each nested statement by label. For example, the [if]
    at label [10] holds the label [11] in place of the braces of the body.

    The table below shows what the analysis builds for the running example. The
    analysis builds the table by walking the root depth first. The root gets
    label [1]. Each statement that the walk visits gets the next label, and the
    walk labels the statements nested inside a statement before it moves on to
    the next statement at the same level. For example, the loop on line 10 gets
    label [6], the braces of the loop body get label [7], and the three
    statements in the body get labels [8], [9], and [10]. So after the labeling
    pass, label [6] is the for loop, which holds the statements at labels [8],
    [9], and [10].

    The table has one row per label and one column per field of {!stmt_info}.
    The Stan lines column gives the lines of the running example that each
    statement comes from. The pattern column is the statement with each nested
    statement replaced by a label. The controls column lists the [if]s and loops
    around the statement, outermost first. The accesses column lists what the
    expressions of the statement read ([R]), write ([W]) and increment ([+=]),
    with the element when the access has a subscript. Reads of the loop variable
    [n] are dropped, so label [8] shows [R a] and not [R n], and the loop at
    label [6] reads only the bound [N].
    {@text[
    label  Stan lines  pattern                             controls              accesses
    1      5-17        root { #2 #3 }                      []                    []
    2      6           real a;                             []                    W a
    3      8-17        Block { #4 #5 #6 }                  []                    []
    4      9           FnValidateSize__("theta", "N", N);  []                    R N
    5      9           matrix[N, 2] theta;                 []                    R N, W theta
    6      10-16       for (n in 1:N) #7                   []                    R N
    7      10-16       Block { #8 #9 #10 }                 [loop 6]              []
    8      11          theta[n, 1] = a;                    [loop 6]              R a, W theta[n, 1]
    9      12          theta[n, 2] = x[n];                 [loop 6]              R x[n], W theta[n, 2]
    10     13-15       if (theta[n, 2] > 0) #11            [loop 6]              R theta[n, 2]
    11     13-15       Block { #12 }                       [loop 6; if 10 then]  []
    12     14          target += 1;                        [loop 6; if 10 then]  += target
    ]}

    Each row describes one statement by itself. The pattern comes from the text
    of the statement, the controls from the statements around the statement, and
    the accesses from the expressions of the statement. No row says which other
    statement wrote a value that the row reads. That question needs the control
    flow of the whole analyzed code, and the next section answers that question.

    {1 Reaching definitions}

    A {e reaching definition} of a variable at a statement is a write of that
    variable that can run before the statement, with no write in between that
    overwrites the value. The writes that reach a statement are the only places
    the value the statement reads can come from, so every data dependence starts
    as a reaching definition. The [reaching_definitions] field of
    {!dependence_info} maps each label to the definitions that reach the node of
    the statement in the flowgraph. Each definition is a
    {!Dataflow_types.reaching_definition}, the variable paired with a
    [label option]. [Some label] is the statement that writes the variable, and
    the write is a [W] or [+=] access of that label in the table above. [None]
    is a write from outside the analyzed code. The analysis starts with a [None]
    definition for every data variable and every parameter, since both hold a
    value before the analyzed code runs. A declaration, an assignment, a
    [target +=] and a loop variable all define the variable they name.

    The table leaves out the four block labels. The node of a block comes after
    the statements of the block, so the set of a block holds what reaches the
    end of the block, which no query asks for.
    {@text[
    label  pattern                             reaching definitions
    2      real a;                             (N, None) (a, None) (x, None)
    4      FnValidateSize__("theta", "N", N);  (N, None) (a, Some 2) (x, None)
    5      matrix[N, 2] theta;                 (N, None) (a, Some 2) (x, None)
    6      for (n in 1:N) #7                   (N, None) (a, Some 2) (n, Some 6) (target, Some 12) (theta, Some 5) (theta, Some 8) (theta, Some 9) (x, None)
    8      theta[n, 1] = a;                    same as 6
    9      theta[n, 2] = x[n];                 same as 6
    10     if (theta[n, 2] > 0) #11            same as 6
    12     target += 1;                        same as 6
    ]}

    Three things in the table are worth reading closely.
    - The data [N] and [x] keep the [None] definition at every label, because
      the data block is outside the analyzed code and nothing inside writes
      them. The parameter [a] starts as [(a, None)] too, but the declaration at
      label [2] is a write of [a] inside the analyzed code, so from label [4] on
      the only definition of [a] is [(a, Some 2)].
    - Inside the loop, at labels [8] to [12], all three writes of [theta] reach
      every statement. Label [5] reaches from the first iteration, and labels
      [8] and [9] reach from an earlier iteration through the back edge of the
      loop. None of the three kills another: [theta[n, 1]] and [theta[n, 2]] are
      different elements, and neither overwrites the whole matrix that label [5]
      declares.
    - The loop header at label [6] holds the writes of the loop body as well,
      because the body flows back to the header.

    {1 Direct dependencies}

    {!direct_dependence_ids} joins the two maps. The control dependencies of a
    statement come from the controls column of the statement map: the innermost
    [if] or loop, and the jumps. The data dependencies come from the two maps in
    turn. The accesses of the statement say which variables the statement reads.
    The reaching definitions of the statement name the candidate writers of each
    such variable. The accesses of each candidate then say which element the
    candidate writes, and a candidate that can never write the element that is
    read is dropped ({!is_flow_dependence}). A [None] definition never becomes a
    dependence, because the queries return labels only. An increment, such as
    [target += 1], does not use the value of an earlier increment, so two
    increments of the same variable are never dependent.

    The table shows the result for each label with a dependence; labels [1] to
    [6] have none. The last column gives the derivation.
    {@text[
    label  pattern                   Data  Control  from
    7      Block { #8 #9 #10 }       -     6        the innermost control is loop 6
    8      theta[n, 1] = a;          2     6        reads a; the reaching definitions give (a, Some 2)
    9      theta[n, 2] = x[n];       -     6        reads x[n]; the reaching definitions give (x, None), and a write from outside is never a dependence
    10     if (theta[n, 2] > 0) #11  5, 9  6        reads theta[n, 2]; the reaching definitions give 5, 8, 9; label 8 writes theta[n, 1], another element, so 8 is dropped
    11     Block { #12 }             -     10       the innermost control is if 10
    12     target += 1;              -     10       increments target; the reaching (target, Some 12) is this same increment, and an increment does not use an earlier increment
    ]}

    Label [10] shows the join in full. The statement map says that the [if]
    reads [theta[n, 2]] and that the innermost control is the loop at label [6].
    The reaching definitions of label [10] give three candidate writers of
    [theta]: labels [5], [8] and [9]. Back in the statement map, label [8]
    writes [theta[n, 1]], never the element [theta[n, 2]], so label [8] is
    dropped. The result is [{Data 5; Data 9; Control 6}]. The statement map is
    read for the sink and for every candidate, and the reaching map once, for
    the sink.

    {1 Using the dependencies}

    The factor graph and pedantic mode both start at one statement and follow
    the direct dependencies in the table above, then the dependencies of those
    statements. The tree below shows this walk from [target += 1] at label [12].
    The right column lists the variables that each statement reads, leaving out
    loop variables. Note the numbers in the below are the {e label} and not the
    original stan program line numbers.
    {@text[
      label: 12  target += 1;
      `-- label: 10  if (theta[n, 2] > 0)     control
          |-- label: 9  theta[n, 2] = x[n];   data
          |   `-- label: 6  for (n in 1:N)    control
          |-- label: 5  matrix[N, 2] theta;   data
          `-- label: 6  for (n in 1:N)        control
      label: 8  theta[n, 1] = a;              reads a (not reached)
    ]}

    One goal of the pedantic analysis is to warn users about parameters that are
    declared but not used. Do check for unused parameters, the analysis starts
    the walk at each statement that changes the log density, such as
    [target += 1]. At each statement it records the data and control
    dependencies that the walk goes through. We can see that the walk never
    reaches label [8] which assigns [a] to [theta], so pedantic mode warns that
    the parameter [a] is not used in the density. In the real pedantic analysis
    we would sweep through every statement that changes the log density and
    store all parameters. Then at the end we would report which parameters go
    unused. *)

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
    Both maps have the same labels, and the sections {e The statement map} and
    {e Reaching definitions} above show both for the running example. The two
    maps are read differently. Every query, and the kill rule inside the
    reaching-definitions dataflow, reads [statements]. Only the data half of
    {!direct_dependence_ids} reads [reaching_definitions], once per sink, to
    find the candidate writers of a variable before the accesses of each writer
    decide the element. Label [1] ({!Dataflow_utils.root_statement_id}) is the
    root. The queries return statement labels only, so a definition from outside
    the analyzed code, such as data, never appears in a result. *)
type dependence_info =
  { statements: stmt_info LabelMap.t
        (** What each statement does by itself: the pattern, the enclosing
            controls and the accesses ({!stmt_info}). *)
  ; reaching_definitions: reaching_definition Set.Poly.t LabelMap.t
        (** The writes that can reach the node of each statement in the
            flowgraph with no overwrite in between, one (variable name, label)
            pair per write. [Some label] is a statement in the map. [None] is a
            write from outside the analyzed code, such as data. The node of a
            block comes after the statements of the block, so the set of a block
            holds what reaches the end of the block. How a writer changes the
            variable is in the accesses of the writer. *) }

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
