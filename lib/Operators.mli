(* -- Helpers for treatment of operators -- *)
open Ast

val get_operator_return_type_opt :
  string -> (originblock * unsizedtype) list -> returntype option
(** Get an optional return type for an operator, given its name and argument types. *)

val pretty_print_all_operator_signatures : string -> string
(** Print all the signatures of a stan math operator, for the purposes of error messages. *)
