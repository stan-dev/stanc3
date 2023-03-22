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

val without_suffix : string list -> string -> string
val is_deprecated_distribution : string -> bool
val deprecated_distributions : (string * string) String.Map.t
val deprecated_functions : (string * string) String.Map.t
val rename_deprecated : (string * string) String.Map.t -> string -> string

val userdef_functions :
     ('a, 'b, 'c, 'd) statement_with program
  -> (string * Middle.UnsizedType.argumentlist) list

val is_redundant_forwarddecl :
     (string * Middle.UnsizedType.argumentlist) list
  -> identifier
  -> (Middle.UnsizedType.autodifftype * Middle.UnsizedType.t * 'a) list
  -> bool

val userdef_distributions : untyped_statement block option -> string list
val collect_warnings : typed_program -> Warnings.t list
val remove_unneeded_forward_decls : typed_program -> typed_program
