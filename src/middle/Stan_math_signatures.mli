(** This module stores a table of all signatures from the Stan
    math C++ library which are exposed to Stan, and some helper
    functions for dealing with those signatures.
*)

open Core_kernel

(** Function arguments are represented by their type an autodiff
   type. This is [AutoDiffable] for everything except arguments
   marked with the data keyword *)
type fun_arg = UnsizedType.autodifftype * UnsizedType.t

(** Signatures consist of a return type, a list of arguments, and a flag
    for whether or not those arguments can be Struct of Arrays objects *)
type signature =
  UnsizedType.returntype * fun_arg list * Common.Helpers.mem_pattern

val stan_math_signatures : (string, signature list) Hashtbl.t
(** Mapping from names to signature(s) of functions *)

val is_stan_math_function_name : string -> bool
(** Equivalent to [Hashtbl.mem stan_math_signatures s]*)

(** Pretty printers *)

val pp_math_sig : signature Fmt.t
val pretty_print_all_math_sigs : unit Fmt.t
val pretty_print_all_math_distributions : unit Fmt.t

type dimensionality

type fkind = Lpmf | Lpdf | Log | Rng | Cdf | Ccdf | UnaryVectorized
[@@deriving show {with_path= false}]

val distributions :
  (fkind list * string * dimensionality list * Common.Helpers.mem_pattern) list
(** The distribution {e families} exposed by the math library *)

val dist_name_suffix : (string * 'a) list -> string -> string

(** Helpers for dealing with operators as signatures *)

val operator_to_stan_math_fns : Operator.t -> string list
val string_operator_to_stan_math_fns : string -> string
val pretty_print_math_lib_operator_sigs : Operator.t -> string list
val make_assignmentoperator_stan_math_signatures : Operator.t -> signature list

(** Special functions for the variadic signatures exposed *)

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
