(** The signatures of the Stan Math library, which are used for type checking *)

val stan_math_returntype :
     string
  -> (Middle.autodifftype * Middle.unsizedtype) list
  -> Middle.returntype option
(** Get an optional return type for a Stan Math library function, given its name and argument types. *)

val assignmentoperator_stan_math_return_type :
     Middle.operator
  -> (Middle.autodifftype * Middle.unsizedtype) list
  -> Middle.returntype option

val operator_stan_math_return_type :
     Middle.operator
  -> (Middle.autodifftype * Middle.unsizedtype) list
  -> Middle.returntype option

val is_stan_math_function_name : string -> bool
(** Check whether a string is the name of a Stan Math library function. *)

val pretty_print_all_math_lib_fn_sigs : string -> string
(** Print all the signatures of a math library function, for the purposes of error messages. *)

val pretty_print_math_lib_operator_sigs : Middle.operator -> string list

val pretty_print_math_lib_assignmentoperator_sigs :
  Middle.operator -> string option
