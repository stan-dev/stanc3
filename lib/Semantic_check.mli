(** Semantic validation of AST*)

open Ast

val semantic_check_program : untyped_program -> typed_program
(** Performs semantic check on AST and returns original AST embellished with type decorations *)

val check_that_all_functions_have_definition : bool ref
(** A switch to determine whether we check that all functions have a definition *)

val model_name : string ref
