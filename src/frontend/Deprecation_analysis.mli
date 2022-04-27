(** Utilities for emitting deprecation warnings and
  finding proper replacements for deprecated features
*)

open Core_kernel
open Ast

module type Deprecation_analizer = sig
  val find_udf_log_suffix :
    typed_statement -> (string * Middle.UnsizedType.t) option

  val update_suffix : string -> Middle.UnsizedType.t -> string

  val collect_userdef_distributions :
    typed_program -> Middle.UnsizedType.t String.Map.t

  val distribution_suffix : string -> bool
  val without_suffix : string list -> string -> string
  val is_deprecated_distribution : string -> bool
  val rename_deprecated_distribution : string -> string
  val rename_deprecated_function : string -> string
  val userdef_distributions : untyped_statement block option -> string list
  val collect_warnings : typed_program -> Warnings.t list
end

module Make (StdLib : Std_library_utils.Library) : Deprecation_analizer
