(** The [Generated_signatures] module is produced by the [Generate.ml]
   executable in this folder.

   These values should only be used by the [Stan_math_signatures] module. *)

open Core
open Middle

val stan_math_signatures : UnsizedType.signature list String.Table.t

val stan_math_variadic_signatures :
  UnsizedType.variadic_signature String.Table.t

val distributions : (string * string list) list
