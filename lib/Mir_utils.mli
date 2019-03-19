open Core_kernel
open Mir
open Dataflow_types

val fwd_traverse_statement : ((expr_typed_located, 'a) statement) ->
    init:'f -> f:('f -> 'a -> 'f * 'c) -> 'f * (expr_typed_located, 'c) statement
(**
   A traversal that simultaneously accumulates a state (type 'f) and replaces the
   substatement values from ('a to 'c). Traversal is done in-order but ignores branching,
   e.g., and if's then block is followed by the else block rather than branching.
*)

val vexpr_of_expr_exn : expr_typed_located -> vexpr
(**
   Take a LHS expression from a general expression, throwing an exception if it can't be a
   LHS expression.
*)

val expr_var_set : expr_typed_located -> vexpr Set.Poly.t
(**
   The set of variables in an expression, including inside an index.

   For use in RHS sets, not LHS assignment sets, except in a target term.
*)

val index_var_set : expr_typed_located index -> vexpr Set.Poly.t
(**
   The set of variables in an index.

   For use in RHS sets, not LHS assignment sets, except in a target term
*)

val expr_assigned_var : expr_typed_located -> vexpr
(**
   The variable being assigned to when the expression is the LHS
*)

val summation_terms : expr_typed_located -> expr_typed_located list
(** The list of terms in expression separated by a + *)

val stmt_of_block : stmt_loc list -> stmt_loc
(** Represent a list of statements as a single statement *)

val subst_expr :
     (string, expr_typed_located) Map.Poly.t
  -> expr_typed_located
  -> expr_typed_located
(** Substitute variables in an expression according to the provided Map. *)

val subst_stmt_base :
     (string, expr_typed_located) Map.Poly.t
  -> (expr_typed_located, 'a) statement
  -> (expr_typed_located, 'a) statement
(** Substitute variables occurring at the top level in statements according to the provided Map. *)

val subst_stmt :
  (string, expr_typed_located) Map.Poly.t -> stmt_loc -> stmt_loc
(** Substitute variables occurring anywhere in a statement according to the provided Map. *)
