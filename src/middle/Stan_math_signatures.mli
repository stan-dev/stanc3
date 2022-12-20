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
type signature = UnsizedType.returntype * fun_arg list * Mem_pattern.t

val stan_math_signatures : (string, signature list) Hashtbl.t
(** Mapping from names to signature(s) of functions *)

val is_stan_math_function_name : string -> bool
(** Equivalent to [Hashtbl.mem stan_math_signatures s]*)

type variadic_signature =
  { return_type: UnsizedType.t
  ; control_args: fun_arg list
  ; required_fn_rt: UnsizedType.t
  ; required_fn_args: fun_arg list }

val stan_math_variadic_signatures : (string, variadic_signature) Hashtbl.t
(** Mapping from names to description of a variadic function.

  Note that these function names cannot be overloaded, and usually require
  customized code-gen in the backend.
*)

val is_stan_math_variadic_function_name : string -> bool
(** Equivalent to [Hashtbl.mem stan_math_variadic_signatures s]*)

(** Pretty printers *)

val pp_math_sig : signature Fmt.t
val pretty_print_all_math_sigs : unit Fmt.t
val pretty_print_all_math_distributions : unit Fmt.t

type dimensionality
type return_behavior

type fkind = private
  | Lpmf
  | Lpdf
  | Log
  | Rng
  | Cdf
  | Ccdf
  | UnaryVectorized of return_behavior
[@@deriving show {with_path= false}]

val distributions :
  (fkind list * string * dimensionality list * Mem_pattern.t) list
(** The distribution {e families} exposed by the math library *)

val dist_name_suffix : (string * 'a) list -> string -> string

(** Helpers for dealing with operators as signatures *)

val operator_to_stan_math_fns : Operator.t -> string list
val string_operator_to_stan_math_fns : string -> string
val pretty_print_math_lib_operator_sigs : Operator.t -> string list
val make_assignmentoperator_stan_math_signatures : Operator.t -> signature list

(** Special functions for the variadic signatures exposed *)

(* reduce_sum helpers *)
val is_reduce_sum_fn : string -> bool
val reduce_sum_slice_types : UnsizedType.t list
