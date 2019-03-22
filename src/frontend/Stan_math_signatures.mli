(** The signatures of the Stan Math library, which are used for type checking *)

open Ast

val get_stan_math_function_return_type_opt :
  string -> typed_expression list -> returntype option
(** Get an optional return type for a Stan Math library function, given its name and argument types. *)

val is_stan_math_function_name : string -> bool
(** Check whether a string is the name of a Stan Math library function. *)

val pretty_print_all_stan_math_function_signatures : string -> string
(** Print all the signatures of a stan math function, for the purposes of error messages. *)
