(** Semantic validation of AST*)

open Core_kernel

val semantic_check_program : Ast.untyped_program -> Ast.typed_program
(** Performs semantic check on AST and returns original AST embellished with type decorations *)

val check_that_all_functions_have_definition : bool ref
(** A switch to determine whether we check that all functions have a definition *)

val model_name : string ref
(** A reference to hold the model name. Relevant for checking variable
    clashes and used in code generation. *)

val inferred_unsizedtype_of_indexed :
     Mir.location_span
  -> Mir.unsizedtype
  -> (Ast.typed_expression Ast.index * Mir.unsizedtype) sexp_list
  -> Mir.unsizedtype
(** [inferred_unsizedtype_of_indexed loc ut typed_idxs] is responsible for figuring
    out what the return (unsized) type of an indexing operation into an unsized
    type is.*)

val operator_return_type :
  Mir.operator -> Ast.typed_expression list -> Mir.returntype option
