open Std
open Std.Sexp_conv

(***********************************)
(* Basic datatypes                 *)
(***********************************)

(** A label is a unique identifier for a node in the dataflow/dependency graph,
    and often corresponds to one node in the Mir. *)
type label = int [@@deriving sexp_of]

(** The side of an [if] that holds a statement. *)
type side = Then | Else

(** An [if] or a loop around a statement. [id] is the label of the [if] or the
    loop. [side] is the side of the [if] that holds the statement, and [None]
    for a loop. A loop is identified by the label of the loop statement, and the
    statement at that label holds the loop variable. *)
type enclosing = {id: label; side: side option}

(** The {e controls} of a statement are the other statements that decide
    whether, or how often, the statement runs. {!Dataflow_utils.build_controls}
    finds the controls of each statement. *)
type controls =
  { enclosing: enclosing list
        (** The index of ifs and loops around the statement, outermost first. *)
  ; jump_ids: label Set.Poly.t
        (** The labels of the [break], [continue] and [return] statements that
            can skip the statement. For a loop statement, also each [break] or
            [return] in the loop body that can end the loop. *) }

module LabelMap = struct
  include Int.Map

  let sexp_of_t f t = sexp_of_list (sexp_of_pair sexp_of_int f) (to_list t)
end

module ExprSet = Set.Make (Middle.Expr.Typed)
module ExprMap = Map.Make (Middle.Expr.Typed)
