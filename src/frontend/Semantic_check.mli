(** Semantic validation of AST*)

open Core_kernel

val inferred_unsizedtype_of_indexed_exn :
     loc:Middle.location_span
  -> Middle.unsizedtype
  -> Ast.typed_expr_meta Ast.expr_with Ast.index list
  -> Middle.unsizedtype
(** Infers unsized type of an `Indexed` expression  *)

val semantic_check_binop_exn :
     Middle.location_span
  -> Middle.operator
  -> Ast.typed_expr_meta Ast.expr_with * Ast.typed_expr_meta Ast.expr_with
  -> Ast.typed_expr_meta Ast.expr_with

val semantic_check_program :
  Ast.untyped_program -> (Ast.typed_program, Semantic_error.t list) result
(** Performs semantic check on AST and returns original AST embellished with type decorations *)

val check_that_all_functions_have_definition : bool ref
(** A switch to determine whether we check that all functions have a definition *)

val model_name : string ref
(** A reference to hold the model name. Relevant for checking variable
    clashes and used in code generation. *)
