open Core_kernel
open Ast

type t = Middle.Warnings.t

val find_udf_log_suffix :
  typed_statement -> (string * Middle.UnsizedType.t) option

val update_suffix : string -> Middle.UnsizedType.t -> string

val collect_userdef_distributions :
  typed_program -> Middle.UnsizedType.t String.Map.t

val distribution_suffix : string -> bool
val without_suffix : string sexp_list -> string -> string
val is_deprecated_distribution : string -> bool
val deprecated_distributions : string String.Map.t
val deprecated_functions : string String.Map.t
val rename_deprecated : string String.Map.t -> string -> string
val userdef_distributions : untyped_statement list option -> string list
val collect_warnings : typed_program -> t list
