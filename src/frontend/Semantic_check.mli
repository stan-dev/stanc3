(** Semantic validation of AST*)

open Core_kernel

val semantic_check_program :
  Ast.untyped_program -> (Ast.typed_program, Semantic_error.t list) result
(** Performs semantic check on AST and returns original AST embellished with type decorations *)

val check_that_all_functions_have_definition : bool ref
(** A switch to determine whether we check that all functions have a definition *)

val model_name : string ref
(** A reference to hold the model name. Relevant for checking variable
    clashes and used in code generation. *)
