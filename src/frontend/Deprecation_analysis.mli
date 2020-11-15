open Core_kernel
open Ast

val find_suffixes : typed_statement -> (string * Middle.UnsizedType.t) option
val update_suffix : string -> Middle.UnsizedType.t -> string
val analyze_udfs : typed_program -> Middle.UnsizedType.t String.Map.t
val distribution_suffix : string -> bool
val without_suffix : string sexp_list -> string -> string
val is_distribution : string -> bool
val rename_distribution : string -> string
val rename_function : string -> string
val userdef_distributions : untyped_statement list option -> string list
val emit_warnings : typed_program -> unit
