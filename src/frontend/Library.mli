(** This module is used as a parameter for many functors which
    rely on information about a backend-specific Stan library. *)

open Middle
open Core_kernel
open Std_library_utils

val function_signatures : (string, signature list) Hashtbl.t
(** Mapping from names to signature(s) of functions *)

val distribution_families : string list

val is_stdlib_function_name : string -> bool
(** Equivalent to [Hashtbl.mem function_signatures s]*)

val get_signatures : string -> signature list
(** Equivalent to [Hashtbl.find_multi function_signatures s]*)

val get_operator_signatures : Operator.t -> signature list
val get_assignment_operator_signatures : Operator.t -> signature list
val is_not_overloadable : string -> bool
val is_variadic_function_name : string -> bool
val variadic_function_returntype : string -> UnsizedType.returntype option

val check_variadic_fn :
     Ast.identifier
  -> is_cond_dist:bool
  -> Location_span.t
  -> Environment.originblock
  -> Environment.t
  -> Ast.typed_expression list
  -> Ast.typed_expression
(** This function is responsible for typechecking varadic function
      calls. It needs to live in the Library since this is usually
      bespoke per-function. *)

val operator_to_function_names : Operator.t -> string list
val string_operator_to_function_name : string -> string
val deprecated_distributions : deprecation_info String.Map.t
val deprecated_functions : deprecation_info String.Map.t
