open Std
open Middle
open Dataflow_types

val build_cf_graphs :
     ?flatten_loops:bool
  -> ?blocks_after_body:bool
  -> ((Expr.Typed.t, label) Stmt.Pattern.t * 'm) LabelMap.t
  -> label Set.Poly.t * label Set.Poly.t LabelMap.t * control list LabelMap.t
(** Builds the exit set, the predecessor graph and the controls of each
    statement in one traversal. They share the bookkeeping of [break],
    [continue] and [return].

    The result is a triple. The exit set and the predecessor graph are the
    result of {!build_predecessor_graph}. The third part maps each label to its
    controls. A control is an enclosing [if], an enclosing loop, or a jump that
    can skip the statement ({!Dataflow_types.control}). The enclosing ifs and
    loops come first, outermost first. A loop statement does not list itself. *)

val loop_of_statement : label -> ('e, 's) Stmt.Pattern.t -> loop option
(** Returns the loop that a [for] or [while] statement at the label forms. Any
    other statement gives [None]. [for (n in 1:N) ...] at label [6] gives
    [Some {id = 6; var = Some "n"}]. *)

val control_parents : control list -> label Set.Poly.t
(** Returns the statements that directly decide whether a statement runs. These
    are the nearest enclosing [if] or loop, and each jump. In
    [for (n in 1:N) if (c) x = 1;], the controls of [x = 1] are the loop and the
    [if], and the result holds only the [if]. *)

val loops_around :
  statement_id:label -> ('e, 's) Stmt.Pattern.t -> control list -> loop list
(** Returns the [for] and [while] loops around a statement, outermost first.
    [controls] are the controls of the statement. A loop statement also counts
    as inside its own loop, so its list ends with the loop itself. The reason is
    that the loop checks its condition, or its upper bound, again after each
    iteration, so writes in the body can reach it. LLVM also puts the loop
    header inside the loop. In [for (n in 1:N) if (c) x = 1;], [x = 1] and the
    [for] statement both give the loop over [n]. *)

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
