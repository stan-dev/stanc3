(** Utilities for emitting deprecation warnings and
  finding proper replacements for deprecated features
*)

open Core_kernel
open Ast

val find_udf_log_suffix :
  typed_statement -> (string * Middle.UnsizedType.t) option

val update_suffix : string -> Middle.UnsizedType.t -> string

val collect_userdef_distributions :
  typed_program -> Middle.UnsizedType.t String.Map.t

val distribution_suffix : string -> bool
val without_suffix : string list -> string -> string
val is_deprecated_distribution : string -> bool
val deprecated_distributions : (string * string) String.Map.t
val deprecated_functions : (string * string) String.Map.t
val rename_deprecated : (string * string) String.Map.t -> string -> string
val userdef_distributions : untyped_statement block option -> string list
val collect_warnings : typed_program -> Warnings.t list
