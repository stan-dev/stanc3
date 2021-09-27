open Ast

val check_program_exn : untyped_program -> typed_program
(** Can raise Errors.SemanticError *)

val check_program : untyped_program -> (typed_program, Semantic_error.t) result

val model_name : string ref
(** A reference to hold the model name. Relevant for checking variable
    clashes and used in code generation. *)

val check_that_all_functions_have_definition : bool ref
(** A switch to determine whether we check that all functions have a definition *)
