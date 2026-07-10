(** The [Generated_signatures] module is produced by the [Generate.ml]
    executable in this folder.

    These values should only be used by the [Stan_math_signatures] module. *)

open Base
open Middle

val stan_math_signatures : (string, UnsizedType.signature list) Hashtbl.t Lazy.t

val stan_math_variadic_signatures :
  (string, UnsizedType.variadic_signature) Hashtbl.t

val distributions : (string * string list) list
