(** Control flow information for the statements of a statement map.

    A {e statement map} ({!build_statement_map}) gives each statement in a tree
    of statements an integer {e label}. The map takes each label to the
    statement, with each nested statement replaced by the label of the nested
    statement. One traversal of a statement map computes three items.
    {!build_predecessor_graph} returns the first two, and {!build_controls} the
    third.
    - The {e predecessor graph} maps each label to the labels of the statements
      that can run immediately before that statement.
    - The {e exit set} holds the labels of the statements that can run last.
    - The {e controls} of a statement ({!Dataflow_types.controls}) are the
      statements that decide whether, or how often, the statement runs. They are
      the ifs and loops around the statement, outermost first, called the
      {e enclosing} ifs and loops, and the {e jumps}, which are the [break],
      [continue] and [return] statements that can skip the statement. A loop
      statement does not list itself.

    The {e control parents} of a statement are the controls that directly decide
    whether the statement runs. These are the nearest enclosing [if] or loop,
    and each jump.

    {!enclosing_loop_ids} counts a loop statement as inside the loop that the
    statement forms. The loop checks the condition, or the upper bound, again
    after each iteration, so writes in the body can reach the loop statement.
    LLVM also puts the loop header inside the loop.

    A loop is identified by the label of the loop statement. The statement at
    that label holds the loop variable.

    The running example is [for (n in 1:N) if (c) x = 1;], where the [for]
    statement has label [6] and the [if] has label [8]. The controls of [x = 1]
    are [{id = 6; side = None}] for the loop and [{id = 8; side = Some Then}]
    for the [if], and the control parents of [x = 1] hold only [8]. For both
    [x = 1] and the [for] statement, {!enclosing_loop_ids} gives [[6]]. *)

open Std
open Middle
open Dataflow_types

val root_statement_id : label
(** The label that {!build_statement_map} gives the statement that the map is
    built from. *)

val build_controls :
     ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t
  -> controls LabelMap.t
(** Returns the controls of each label of the statement map. *)

val control_parent_ids : controls -> label Set.Poly.t
(** Returns the labels of the control parents of a statement, given the controls
    of the statement. *)

val enclosing_loop_ids :
  label -> ('e, 's) Stmt.Pattern.t -> controls -> label list
(** Returns the labels of the loops around a statement, outermost first: the
    loops in the controls of the statement, without the ifs, and for a loop
    statement the label of the statement itself last, since the loop header runs
    in every iteration of the loop. In the running example of
    {!Dependence_analysis}, label [15] has the controls [loop 8; if 13] and the
    loops [[8]], and the loop at label [8] has the loops [[8]]. The common
    prefix of two such lists is the loops around both statements, and the
    dependence distance vector has one entry per such loop. *)

val build_cf_graph :
     ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t
  -> label Set.Poly.t LabelMap.t
(** Building the controlflow graph requires a traversal with state that includes
    continues, breaks, returns and the controlflow graph accumulator. The
    traversal should be a branching traversal with set unions rather than a
    forward traversal because continue and return statements shouldn't affect
    other branches of execution. *)

val build_predecessor_graph :
     ?flatten_loops:bool
  -> ?blocks_after_body:bool
  -> ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t
  -> label Set.Poly.t * label Set.Poly.t LabelMap.t
(** Building the predecessor graph requires a traversal with state that includes
    the current previous nodes and the predecessor graph accumulator. Special
    cases are made for loops, because they should include the body exits as
    predecessors to the body, and they should include loop predecessors in their
    exit sets. I'm not sure if the single re-traversal of the loop body is
    sufficient or this requires finding a fixed-point. *)

val build_recursive_statement :
     (('e, 's) Stmt.Pattern.t -> 'm -> 's)
  -> (('e, label) Stmt.Pattern.t * 'm) LabelMap.t
  -> label
  -> 's
(** Build a fixed-point data type representation of a statement given a
    label-map representation. *)

val is_ctrl_flow : ('a, 'b) Stmt.Pattern.t -> bool
(** Check if the statement controls the execution of its substatements. *)

val merge_set_maps :
  (module M : Map.S)
  -> 'b Set.Poly.t M.t -> 'b Set.Poly.t M.t -> 'b Set.Poly.t M.t
(** Merge two maps whose values are sets, and union the sets when there's a
    collision. *)

val build_statement_map :
     Stmt.Located.t
  -> ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t
(** Builds the statement map of a statement. The walk visits the statement and
    the statements nested in the statement depth first, gives each one the next
    label, and records each statement's pattern with every nested statement
    replaced by the nested statement's label, paired with the location of the
    statement. *)
