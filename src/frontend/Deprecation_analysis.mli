(** Utilities for emitting deprecation warnings and
  finding proper replacements for deprecated features
*)

open Core
open Ast

val expired : int * int -> bool
val deprecated_functions : (string * (int * int)) String.Map.t
val rename_deprecated : (string * (int * int)) String.Map.t -> string -> string
val stan_lib_deprecations : (string * (int * int)) String.Map.t
val collect_warnings : typed_program -> Warnings.t list
val remove_unneeded_forward_decls : typed_program -> typed_program
