(** The signatures of the Stan Math library, which are used for type checking *)
open Ast

val try_get_stan_math_function_return_type :
  string -> (originblock * unsizedtype) list -> returntype option
(** Get an optional return type for a Stan Math library function, given its name and argument types. *)

val is_stan_math_function_name : string -> bool
(** Check whether a string is the name of a Stan Math library function. *)

val try_get_operator_return_type :
  string -> (originblock * unsizedtype) list -> returntype option
(** Get an optional return type for an operator, given its name and argument types. *)
