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
      10    real s = 0;
      11    for (n in 1:N) {
      12      theta[n, 1] = a;
      13      theta[n, 2] = x[n];
      14      s = s + theta[n, 2];
      15      if (s > 0) {
      16        target += 1;
      17      }
      18    }
      19  }
    ]}

    The idea of dependency analysis is pretty simple on its own. Given we are
    looking at some statement in a program which statements does this statement
    depend on either via reads/writes or conditional execution. We call these
    dependence forms {e data dependence} or {e control dependence}.

    - A {e data dependence} exists when a write in the other statement can
      supply a value that this statement reads. The write runs before the read
      when the program runs.

    {@text[
      14      s = s + theta[n, 2];    writes s
      15      if (s > 0) {            reads s: data dependence on line 14
    ]}

    - A statement can also depend on another statement without reading a value
      from the other statement. A {e control dependence} exists when the other
      statement decides whether this statement runs. The other statement is
      control dependence if it is an [if] or a loop around this statement, or a
      [break], [continue], or [return] that can skip this statement.

    {@text[
      11    for (n in 1:N) {          decides whether lines 12 to 15 run
      12      theta[n, 1] = a;        control dependence on line 11
      13      theta[n, 2] = x[n];     control dependence on line 11
      14      s = s + theta[n, 2];    control dependence on line 11
      15      if (s > 0) {            control dependence on line 11; decides whether line 16 runs
      16        target += 1;          control dependence on line 15
    ]}

    A statement has a direct control dependence only on the innermost [if] or
    loop around the statement, and on the jumps that can skip the statement.
    These statements are the {e control parents} of the statement
    ({!Dataflow_utils.control_parent_ids}). The outer [if]s and loops around a
    statement are dependencies of the control parent. They are therefore
    transitive control dependencies of the statement.
    {@text[
      15      if (s > 0) {            control parent of line 16
      16        target += 1;          direct: line 15   transitive: line 11, through line 15
    ]}

    The rest of this section shows how the analysis finds these dependencies for
    the running example. Dependence analysis reads a large group of statements
    at once, the {e analyzed code}, and then finds the dependencies between the
    statements in that group. For the log density, the analyzed code is the
    parameters, transformed parameters, and model blocks, which are lines 5 to
    19 in the running example.

    {1 The statement map}

    The analysis puts the analyzed code under one {e root}, which gets label
    [1], and gives each statement inside the root an integer {e label} that
    offsets it from root. The dependencies between the labeled statements form a
    graph whose nodes are the labels. Statements outside the analyzed code, such
    as the data block on lines 1 to 4, do not get a label.

    {@text[
     5  parameters {
     6    real a;
     7  }
     8  model {
     9    matrix[N, 2] theta;
    10    real s = 0;
    11    for (n in 1:N) {
    12      theta[n, 1] = a;
    13      theta[n, 2] = x[n];
    14      s = s + theta[n, 2];
    15      if (s > 0) {
    16        target += 1;
    17      }
    18    }
    19  }
    ]}

    To build this graph, the analysis first records the statement at each label.
    The result of the analysis maps each label to the {!stmt_info} record of
    that statement. A statement that holds other statements refers to each
    nested statement by label. For example, the [if] at label [13] holds the
    label [14] in place of the braces of the body.

    The table below shows what the analysis builds for the running example. The
    analysis builds the table by walking the root depth first. The root gets
    label [1]. Each statement that the walk visits gets the next label, and the
    walk labels the statements nested inside a statement before it moves on to
    the next statement at the same level. For example, the loop on line 11 gets
    label [8], the braces of the loop body get label [9], and the four
    statements in the body get labels [10] to [13]. So after the labeling pass,
    label [8] is the for loop, which holds the statements at labels [10] to
    [13]. A declaration with an initializer, [real s = 0] on line 10, is two
    statements in the MIR: the declaration at label [6] and the assignment at
    label [7], where the integer literal prints as [promote(0, real, var)].

    The table has one row per label and one column per field of {!stmt_info}.
    The Stan lines column is the location recorded for the statement; the root
    has none, and a block records the line of the first statement in the block.
    The pattern column is the statement with each nested statement replaced by
    [#] and the label, and a block shows the constructor name, [Block] or
    [SList], before the labels of the statements in the block. The controls
    column lists the [if]s and loops around the statement, outermost first. The
    accesses column lists what the expressions of the statement read ([R]),
    write ([W]) and increment ([+=]), with the element when the access has a
    subscript. Reads of the loop variable [n] are dropped, so label [10] shows
    [R a] and not [R n], and the loop at label [8] reads only the bound [N]. The
    unit test "Running example: the statement map" prints this table.
    {@text[
    label  Stan lines  pattern                             controls                  accesses
    1      -           SList { #2 #3 }                     []
    2      6           real a;                             []                        W a
    3      9           Block { #4 #5 #6 #7 #8 }            []
    4      9           FnValidateSize__("theta", "N", N);  []                        R N
    5      9           matrix[N, 2] theta;                 []                        R N, W theta
    6      10          real s;                             []                        W s
    7      10          s = promote(0, real, var);          []                        W s
    8      11-18       for(n in 1:N) #9                    []                        R N
    9      11-18       Block { #10 #11 #12 #13 }           [loop 8 (n)]
    10     12          theta[n, 1] = a;                    [loop 8 (n)]              R a, W theta[n, 1]
    11     13          theta[n, 2] = x[n];                 [loop 8 (n)]              R x[n], W theta[n, 2]
    12     14          s = (s + theta[n, 2]);              [loop 8 (n)]              R s, R theta[n, 2], W s
    13     15-17       if((s > 0)) #14                     [loop 8 (n)]              R s
    14     15-17       Block { #15 }                       [loop 8 (n); if 13 then]
    15     16          target += 1;                        [loop 8 (n); if 13 then]  += target
    ]}

    Each row describes one statement by itself. The pattern comes from the text
    of the statement, the controls from the statements around the statement, and
    the accesses from the expressions of the statement. No row says which other
    statement wrote a value that the row reads. That question needs the control
    flow of the whole analyzed code, and the next section answers that question.

    {1 Reaching definitions}

    A {e reaching definition} of a variable at a statement is a write of that
    variable that is the last write of the variable on at least one path to the
    statement. The set at a statement holds one entry per such write, so a
    statement that two paths reach, such as a loop header, can hold two writes
    of one variable. The writes that reach a statement are the only places the
    value the statement reads can come from, so every data dependence starts as
    a reaching definition.

    A write of a whole variable {e kills} every earlier definition of that
    variable, and a write of one element kills an earlier definition only when
    the earlier write must have written the same element. The access of a
    whole-variable write has an empty path, and an empty path overwrites every
    path. A killed definition leaves the set, so the set is not cumulative.

    Each entry in the table is one definition: the variable and the path of the
    write, then the label of the writing statement, as [theta[n, 1] 10]. Several
    labels in brackets, as [s [7, 12]], are definitions with the same access at
    different statements. The table leaves out the writes from outside the
    analyzed code, whose label is [None]; a row with only those prints [None].
    The reaching definitions show, for each variable, where the value could have
    been written before the statement runs. In a for loop, previous iterations
    of the loop can contribute a write in the current iteration. For label [10]
    you can see that [s] has reaching definitions at both [7] and [12]. This is
    because the analysis follows the flowgraph of the program, so the write at
    label [12] in the previous iteration needs to be accounted for in the
    reaching definitions. The table leaves out the four block labels. The unit
    test "Running example: the reaching definitions" prints this table, with the
    full patterns of labels [4] and [7] in place of the abbreviations.
    {@text[
    label  pattern                reaching definitions
    2      real a;                None
    4      size_check("theta",N); a 2
    5      matrix[N, 2] theta;    a 2
    6      real s;                a 2, theta 5
    7      s = assign(0);         a 2, s 6, theta 5
    8      for(n in 1:N) #9       a 2, s [7, 12], target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    10     theta[n, 1] = a;       a 2, s [7, 12], target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    11     theta[n, 2] = x[n];    a 2, s [7, 12], target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    12     s = (s + theta[n, 2]); a 2, s [7, 12], target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    13     if((s > 0)) #14        a 2, s 12, target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    15     target += 1;           a 2, s 12, target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    ]}

    A few things in the table are worth reading closely.
    - The entries for [s] show a set that grows and then shrinks. At label [7]
      the only definition is the declaration at [6]. At label [8], the loop
      header, we see [s] with reaching definitions for [7] and [12]. Both labels
      stay in the entry through label [12]. The assignment at [12] writes the
      whole of [s], so the write at [7] ends and only [s 12] reaches the [if] at
      label [13]. The data [N] and [x] keep the [None] definition at every
      label, because nothing in the analyzed code writes them. The parameter [a]
      starts with the [None] definition too, but the declaration at label [2]
      writes the whole of [a], so from label [4] on the entry is [a 2].
    - Inside the loop, all three writes of [theta] reach every statement. Label
      [5] reaches from the first iteration, and labels [10] and [11] reach from
      an earlier iteration through the back edge. None of the three kills
      another: [theta[n, 1]] and [theta[n, 2]] are different elements, and
      neither must overwrite the whole matrix that label [5] declares.
    - The loop header at label [8] holds the writes of the loop body as well,
      because the body flows back to the header.

    {1 Direct dependencies}

    Both halves of a direct dependence are stored on the record of the
    statement. The control dependencies come from the controls column, through
    {!Dataflow_utils.control_parent_ids}: the innermost [if] or loop, and the
    jumps. The data dependencies, the [writers] field, are computed when the
    record is built, from the accesses and the reaching definitions in turn. The
    accesses say which variables the statement reads. The reaching definitions
    of the statement name the candidate writers of each such variable. The
    access in each definition says which element the candidate writes, and a
    candidate that can never write the element that is read is dropped
    ({!Dependence.test_dependence}). A [None] definition never becomes a
    dependence, because the queries return labels only. An increment, such as
    [target += 1], does not use the value of an earlier increment, so two
    increments of the same variable are never dependent.

    The table shows the two halves for each label with a dependence; labels [1]
    to [8] have none. The two columns are the writer labels and the control
    parents, and [-] is an empty column. The unit test "Running example: the
    direct dependencies" prints this table.
    {@text[
    label  pattern                    data dependencies  control dependencies
    9      Block { #10 #11 #12 #13 }  -                  8
    10     theta[n, 1] = a;           2                  8
    11     theta[n, 2] = x[n];        -                  8
    12     s = (s + theta[n, 2]);     5 7 11 12          8
    13     if((s > 0)) #14            12                 8
    14     Block { #15 }              -                  13
    15     target += 1;               -                  13
    ]}

    Label [12] shows the join in full. The statement map says that the
    assignment reads [s] and [theta[n, 2]], and that the innermost control is
    the loop at label [8]. The reaching definitions of label [12] give two
    candidate writers of [s], labels [7] and [12], and three of [theta], labels
    [5], [10] and [11]. The access in each definition says what the candidate
    writes: both writers of [s] write the whole variable, so both stay, and
    label [12] depends on the value of [s] from the previous iteration. Label
    [10] writes [theta[n, 1]], never the element [theta[n, 2]], so label [10] is
    dropped. The writers are labels [5], [7], [11] and [12], and the control
    parent is label [8]. The subscript tests run once, when the record is built;
    every definition brings the write, the label and the loops of the writer
    along, so no other record is read.

    Label [13] shows a kill in the result. The [if] reads [s], and the only
    reaching definition of [s] at label [13] is label [12], so the initializer
    at label [7] is never a candidate and the only writer is label [12].

    {1 Using the dependencies}

    The factor graph and pedantic mode both start at one statement and follow
    the direct dependencies in the table above, then the dependencies of those
    statements. The tree below shows this walk from [target += 1] at label [15].
    The right column gives the kind of each edge. A label that the walk has
    already visited, such as the loop at label [8] and label [12] from the
    previous iteration, is not expanded again. Note the numbers in the below are
    the {e label} and not the original stan program line numbers. The unit test
    "Running example: the walk from target += 1" prints the labels that the walk
    visits, [5 7 8 11 12 13 15], and the variables they read,
    [N s target theta x].
    {@text[
      label: 15  target += 1;
      `-- label: 13  if((s > 0)) #14                  control
          |-- label: 12  s = (s + theta[n, 2]);       data
          |   |-- label: 5  matrix[N, 2] theta;       data
          |   |-- label: 7  s = promote(0, real, var);  data
          |   |-- label: 11  theta[n, 2] = x[n];      data
          |   |   `-- label: 8  for(n in 1:N) #9      control
          |   |-- label: 12  (visited)                data
          |   `-- label: 8  (visited)                 control
          `-- label: 8  (visited)                     control
      label: 10  theta[n, 1] = a;                     reads a (not reached)
    ]}

    One goal of the pedantic analysis is to warn users about parameters that are
    declared but not used. Do check for unused parameters, the analysis starts
    the walk at each statement that changes the log density, such as
    [target += 1]. At each statement it records the data and control
    dependencies that the walk goes through. We can see that the walk never
    reaches label [10] which assigns [a] to [theta], so pedantic mode warns that
    the parameter [a] is not used in the density. In the real pedantic analysis
    we would sweep through every statement that changes the log density and
    store all parameters. Then at the end we would report which parameters go
    unused. *)

(** One write that can still hold the value of a variable when a statement runs:
    the access ({!Accesses.access}, a [Write] or an [Increment]), the label of
    the statement that makes the access and the loops around that statement,
    outermost first. A [None] label is a write from outside the analyzed code,
    such as data or a parameter; the access of such a write is the whole
    variable and the loop list is empty.

    The set at label [12] of the running example holds eight definitions. Two of
    them are
    {@text[
      {access = {var = "theta"; path = [n, 1]; kind = Write}; statement_id = Some 10; loop_ids = [8]}
      {access = {var = "N"; path = []; kind = Write}; statement_id = None; loop_ids = []}
    ]} *)
type reaching_definition =
  {access: Accesses.access; statement_id: label option; loop_ids: label list}

(** What a dependence query needs to know about one statement. Every query reads
    the record of the statement the query starts at; the transitive queries then
    follow labels to other records.

    The example record below is for the condition (label [13]) of the running
    example.
    {@text[
      id:                    13
      pattern:               if((s > 0)) #14
      controls:              {enclosing = [{id = 8; side = None}]; jump_ids = {}}
      loop_ids:              [8]
      accesses:              R s
      reaching_definitions:  a 2, s 12, target 15, theta 5, theta[n, 1] 10, theta[n, 2] 11
    ]} *)
type stmt_info =
  { id: label  (** The label of this statement. *)
  ; pattern: (Expr.Typed.t, label) Stmt.Pattern.t
        (** The statement, with each nested statement replaced by the label of
            the nested statement. *)
  ; meta: Location_span.t  (** The source location for pedantic warnings. *)
  ; controls: controls
        (** The enclosing ifs and loops of this statement, outermost first, and
            the jumps that can skip the statement ({!Dataflow_types.controls}).
            A loop statement does not list itself.
            {!Dataflow_utils.control_parent_ids} gives the controls that decide
            whether the statement runs. *)
  ; loop_ids: label list
        (** The loops around this statement, outermost first
            ({!Dataflow_utils.enclosing_loop_ids}). A loop statement counts as
            inside the loop that the statement starts. *)
  ; accesses: Accesses.t
        (** The reads, writes and increments in the expressions of this
            statement. Each nested statement has a separate record. *)
  ; reaching_definitions: reaching_definition Set.Poly.t
        (** The writes that can reach the node of this statement in the
            flowgraph with no overwrite in between, one definition per write.
            The node of a block comes after the statements of the block, so the
            set of a block holds what reaches the end of the block. *)
  ; writers: (string * label) Set.Poly.t
        (** The data dependencies of this statement: each variable the statement
            reads, paired with the label of a statement whose write of that
            variable can supply the value read. The reaching definitions name
            the candidates, and the subscript test of {!Dependence} keeps the
            writes that can select the element read. For label [12] of the
            running example the set is
            [{(s, 7); (s, 12); (theta, 5); (theta, 11)}]. *) }

val build_dependence_info :
  Program.Typed.t -> Stmt.Located.t -> stmt_info LabelMap.t
(** Builds the statement records, with the reaching definitions and the writers,
    for the statement (label [1]) and the statements nested in the statement. *)

val build_log_prob_dependence_info : Program.Typed.t -> stmt_info LabelMap.t
(** Builds the statement records for the statements of the log density. These
    statements are in the parameters, transformed parameters and model blocks.
*)

val transitive_dependence_ids :
  stmt_info LabelMap.t -> label -> label Set.Poly.t
(** Returns the labels of the direct and transitive dependencies of the
    statement at the label. The result also includes the label. *)

val var_dependence_ids :
     stmt_info LabelMap.t
  -> ?blockers:string Set.Poly.t
  -> string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Like {!transitive_dependence_ids}, but the first step follows only the
    control parents and the definitions of the variables in the given set, and
    the result leaves out the label. *)

val used_vars_at : stmt_info LabelMap.t -> label Set.Poly.t -> string Set.Poly.t
(** Returns the variables that the statements at the labels use, without loop
    variables. For a loop or [if], only the bounds or condition count. *)

(** {2 The loop dependence graph of one [For]} *)

(** The leaves of one [For]'s body and the flow, anti, output and effects edges
    between them; the implementation documents the fields. *)
type loop_graph

val subtree_accesses : stmt_info LabelMap.t -> label -> Accesses.t
(** The accesses of the statement at the label and of every statement below. *)

val statement_at : stmt_info LabelMap.t -> label -> Stmt.Located.t
(** The statement at the label, children rebuilt from the map. *)

val has_effects : stmt_info LabelMap.t -> label -> bool
(** Whether the statement at the label, or one below, prints, rejects, draws
    random numbers or calls a user function. *)

val build_loop_graph : stmt_info LabelMap.t -> loop:label -> loop_graph
(** The flow, anti, output and effects edges between the leaves of the [For] at
    [loop], at that loop's level; the implementation documents the rules. *)

val pi_blocks : loop_graph -> label list list
(** The strongly connected components (Tarjan 1972) in emission order (Allen and
    Kennedy 1987, section 5.2): a topological order of the condensation, ties to
    the earliest leaf. *)

val is_cyclic : loop_graph -> label list -> bool
(** Whether a pi-block must stay a sequential loop; a write-only scatter's self
    output dependence does not count, since indexed assignment stores in order.
*)

val edge_between : loop_graph -> from:label list -> into:label list -> bool
(** Whether some edge leaves a leaf of [from] for a leaf of [into]. *)

val pp_graph :
     stmt_info LabelMap.t
  -> (label * string) list
  -> Format.formatter
  -> loop_graph
  -> unit
(** One indented line per leaf, with the leaf's outcome after it when the list
    has one, then the edges and the pi-blocks in emission order. *)
