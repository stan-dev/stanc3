(** This module stores a table of all signatures from the Stan
    math C++ library which are exposed to Stan, and some helper
    functions for dealing with those signatures.
*)

open Middle
open Frontend.Std_library_utils
include Library

val pretty_print_all_math_sigs : unit Fmt.t
val pretty_print_all_math_distributions : unit Fmt.t

(* TODO: We should think of a better encapsulization for these,
   this doesn't scale well.
*)

(* reduce_sum helpers *)
val is_reduce_sum_fn : string -> bool
val reduce_sum_slice_types : UnsizedType.t list

(* variadic ODE helpers *)
val is_variadic_ode_fn : string -> bool
val is_variadic_ode_nonadjoint_tol_fn : string -> bool
val ode_tolerances_suffix : string
val variadic_ode_adjoint_fn : string
val variadic_ode_mandatory_arg_types : fun_arg list
val variadic_ode_mandatory_fun_args : fun_arg list
val variadic_ode_tol_arg_types : fun_arg list
val variadic_ode_adjoint_ctl_tol_arg_types : fun_arg list
val variadic_ode_fun_return_type : UnsizedType.t
val variadic_ode_return_type : UnsizedType.t

(* variadic DAE helpers *)
val is_variadic_dae_fn : string -> bool
val is_variadic_dae_tol_fn : string -> bool
val dae_tolerances_suffix : string
val variadic_dae_mandatory_arg_types : fun_arg list
val variadic_dae_mandatory_fun_args : fun_arg list
val variadic_dae_tol_arg_types : fun_arg list
val variadic_dae_fun_return_type : UnsizedType.t
val variadic_dae_return_type : UnsizedType.t
