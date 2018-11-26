(** Semantic validation of AST*)

open Ast

(* Performs semantic check on AST and returns original AST embellished with type decorations *)
val semantic_check_program : untyped_program -> typed_program
