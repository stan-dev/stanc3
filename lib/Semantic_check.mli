(** Semantic validation of AST*)

open Core_kernel
open Ast

val semantic_check_program : untyped_program -> typed_program
(** Performs semantic check on AST and returns original AST embellished with type decorations *)

val check_that_all_functions_have_definition : bool ref
(** A switch to determine whether we check that all functions have a definition *)

val model_name : string ref
(** A reference to hold the model name. Relevant for checking variable
    clashes and used in code generation. *)

val inferred_unsizedtype_of_indexed :
     location_span
  -> unsizedtype
  -> (typed_expression index * unsizedtype) sexp_list
  -> unsizedtype
(** [inferred_unsizedtype_of_indexed loc ut typed_idxs] is responsible for figuring
    out what the return (unsized) type of an indexing operation into an unsized
    type is.*)
