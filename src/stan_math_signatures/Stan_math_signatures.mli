(** This module stores a table of all signatures from the Stan math C++ library
    which are exposed to Stan, and some helper functions for dealing with those
    signatures. *)

open Middle

val is_stan_math_function_name : string -> bool
(** Check if a string names a Stan Math library function *)

val lookup_stan_math_function : string -> UnsizedType.signature list
(** Look up the signature of a Stan Math library function. If it is not found,
    this returns [[]] *)

val get_stan_math_signatures_alist :
  unit -> (string * UnsizedType.signature list) list
(** Get all the signatures in the Stan Math library *)

val is_stan_math_variadic_function_name : string -> bool
(** Test if a string names a built-in variadic function

    Note that these function names cannot be overloaded, and usually require
    customized code-gen in the backend. *)

val lookup_stan_math_variadic_function :
  string -> UnsizedType.variadic_signature option
(** Look up the signature of a built-in variadic function *)

(** Pretty printers *)

val pretty_print_all_math_sigs : unit Fmt.t
val pretty_print_all_math_distributions : unit Fmt.t

val distributions : (string * string list) list
(** The distribution {e families} exposed by the math library *)

(** Helpers for dealing with operators as signatures *)

val operator_to_stan_math_fns : Operator.t -> string list
val string_operator_to_stan_math_fns : string -> string
val pretty_print_math_lib_operator_sigs : Operator.t -> string list

val make_assignmentoperator_stan_math_signatures :
  Operator.t -> UnsizedType.signature list

(** Special functions for the variadic signatures exposed *)

(* reduce_sum helpers *)
val is_reduce_sum_fn : string -> bool
val reduce_sum_slice_types : UnsizedType.t list
val is_embedded_laplace_fn : string -> bool
val laplace_helper_param_types : string -> UnsizedType.argumentlist
val laplace_tolerance_argument_types : UnsizedType.argumentlist

val lacks_higher_order_autodiff : string -> bool
(** Check if a function is in the Stan Math library and does not have higher
    order autodiff *)

val is_special_function_name : string -> bool
(** Check if a string names a special function in the Stan Math library. This
    combines several of the above checks, e.g. [is_reduce_sum_fn] *)
