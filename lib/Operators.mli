(* -- Helpers for treatment of operators -- *)
open Ast

val operator_return_type_from_string :
  string -> typed_expression list -> returntype option
(** Get an optional return type for an operator, given its name and argument types. *)

val operator_name : Ast.operator -> string

val operator_return_type :
  Ast.operator -> typed_expression list -> returntype option

val pretty_print_all_operator_signatures : string -> string
(** Print all the signatures of a stan math operator, for the purposes of error messages. *)
