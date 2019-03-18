open Core_kernel
open Mir
open Dataflow_types

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
