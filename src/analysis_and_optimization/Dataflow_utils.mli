open Std
open Middle
open Dataflow_types

val build_cf_graphs :
     ?flatten_loops:bool
  -> ?blocks_after_body:bool
  -> ((Expr.Typed.t, label) Stmt.Pattern.t * 'm) LabelMap.t
  -> label Set.Poly.t * label Set.Poly.t LabelMap.t * controls LabelMap.t
(** Simultaneously builds control information, the predecessor graph and the
    exit set of a statement. It's advantageous to build them together because
    they both rely on some of the same Break, Continue and Return bookkeeping.

    Takes a statement map and returns the triple: (exit set, predecessor graph,
    control information) where
    - (exit set, predecessor graph) is the return value of
      build_predecessor_graph
    - (control information) is the return value of build_controls *)

val build_controls :
  ((Expr.Typed.t, label) Stmt.Pattern.t * 'm) LabelMap.t -> controls LabelMap.t
(** Returns the controls of each label of the statement map. *)

val control_parent_ids : controls -> label Set.Poly.t
(** [control_parent_ids controls] returns the labels of the statements that
    directly control whether a given statement runs. It takes the [break],
    [continue], and [return] labels recorded in [controls.jump_ids] and adds the
    label of the nearest enclosing [if], [for], or [while], if there is one.

    For example, in [for (i in 1:N) { if (skip) continue; print(i); }], the
    result for [print(i)] contains the labels of the [for] and the [continue].
    The loop starts the iteration, and the [continue] can skip the print. *)

val enclosing_loop_ids :
  label -> ('e, 's) Stmt.Pattern.t -> controls -> label list
(** Loop labels from outermost to innermost. For a [for] or [while] header, the
    last label is the header itself. *)

val build_cf_graph :
     ((Expr.Typed.t, label) Stmt.Pattern.t * 'm) LabelMap.t
  -> label Set.Poly.t LabelMap.t
(** Building the controlflow graph requires a traversal with state that includes
    continues, breaks, returns and the controlflow graph accumulator. The
    traversal should be a branching traversal with set unions rather than a
    forward traversal because continue and return statements shouldn't affect
    other branches of execution. *)

val build_predecessor_graph :
     ?flatten_loops:bool
  -> ?blocks_after_body:bool
  -> ((Expr.Typed.t, label) Stmt.Pattern.t * 'm) LabelMap.t
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
     ('s -> ('e, 's) Stmt.Pattern.t)
  -> ('s -> 'm)
  -> 's
  -> (('e, label) Stmt.Pattern.t * 'm) LabelMap.t
(** The statement map is built by traversing substatements recursively to
    replace substatements with their labels while building up the substatements'
    statement maps. Then, the result is the union of the substatement maps with
    this statement's singleton pair, which is expressed in terms of the new
    label-containing statement. *)
