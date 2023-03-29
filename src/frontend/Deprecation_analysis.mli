(** Utilities for emitting deprecation warnings and
  finding proper replacements for deprecated features
*)

open Core_kernel
open Ast

module type DEPRECATION_ANALYZER = sig
  val find_udf_log_suffix :
    typed_statement -> (string * Middle.UnsizedType.t) option

  val update_suffix : string -> Middle.UnsizedType.t -> string

  val collect_userdef_distributions :
    typed_program -> Middle.UnsizedType.t String.Map.t

  val without_suffix : string list -> string -> string
  val is_deprecated_distribution : string -> bool
  val rename_deprecated_distribution : string -> string
  val rename_deprecated_function : string -> string
  val userdef_distributions : untyped_statement block option -> string list
  val collect_warnings : typed_program -> Warnings.t list
end

val remove_unneeded_forward_decls : typed_program -> typed_program

module Make (StdLibrary : Std_library_utils.Library) : DEPRECATION_ANALYZER
