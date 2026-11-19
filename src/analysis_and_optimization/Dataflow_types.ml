open Std
open Std.Sexp_conv

(***********************************)
(* Basic datatypes                 *)
(***********************************)

(** A label is a unique identifier for a node in the dataflow/dependency graph,
    and often corresponds to one node in the Mir. *)
type label = int [@@deriving sexp_of]

(** Representation of an expression that can be assigned to. This should also be
    able to represent indexed variables, but we don't support that yet. *)
type vexpr = VVar of string [@@deriving sexp_of]

(** The most recently nested control flow (block start, if/then, or loop)

    This isn't included in the traversal_state because it only flows downward
    through the tree, not across and up like everything else *)
type cf_state = label

(** The side of an [if] that holds a statement. *)
type branch_side = Then | Else

type enclosing_control = {id: label; side: branch_side option}

type controls =
  { enclosing: enclosing_control list
        (** The index of ifs and loops around the statement, outermost first. *)
  ; jump_ids: label Set.Poly.t
        (** Labels of [break], [continue], and [return] statements that can
            affect whether this statement executes. A [continue] can skip a
            later statement in the loop body, and a [break] can prevent the loop
            header from running again. *) }

module LabelMap = struct
  include Int.Map

  let sexp_of_t f t = sexp_of_list (sexp_of_pair sexp_of_int f) (to_list t)
end

module ExprSet = Set.Make (Middle.Expr.Typed)
module ExprMap = Map.Make (Middle.Expr.Typed)
