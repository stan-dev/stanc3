(** Re-write ASTs to remove deprecated features and clean up
    extra parenthesis, etc *)

open Ast

(** Flags to enable or disable certain kinds of canonicalization.
    NB: [inline_includes] is controlled by the [--canonicalize] argument
    to stanc, but it consumed by the pretty-printer, {i not} this module.
*)
type canonicalizer_settings =
  {deprecations: bool; parentheses: bool; braces: bool; inline_includes: bool}

val all : canonicalizer_settings
val none : canonicalizer_settings

val repair_syntax : untyped_program -> canonicalizer_settings -> untyped_program
(** When deprecation canonicalization is enabled, this runs before typechecking
    and removes suffixes from ~ statements, which are otherwise forbidden by the typechecker *)

val canonicalize_program :
  typed_program -> canonicalizer_settings -> typed_program
(** "Canonicalize" the program by removing deprecations, adding or removing parenthesis
    and braces, etc. *)
