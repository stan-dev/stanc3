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
     Mir.location_span
  -> Mir.unsizedtype
  -> (typed_expression index * Mir.unsizedtype) sexp_list
  -> Mir.unsizedtype
(** [inferred_unsizedtype_of_indexed loc ut typed_idxs] is responsible for figuring
    out what the return (unsized) type of an indexing operation into an unsized
    type is.*)

val operator_return_type_from_string :
  string -> typed_expression list -> Mir.returntype option
(** Get an optional return type for an operator, given its name and argument types. *)

val string_of_operator : Ast.operator -> string
val operator_of_string : string -> Ast.operator option

val operator_return_type :
  Ast.operator -> typed_expression list -> Mir.returntype option

val pretty_print_all_operator_signatures : string -> string
(** Print all the signatures of a stan math operator, for the purposes of error messages. *)

(* The name of the TernaryIf operator *)
val ternary_if : string
