(** a type/semantic checker for Stan ASTs

  Functions which begin with "check_" return a typed version of their input
  Functions which begin with "verify_" return unit if a check succeeds, or else
    throw an [Errors.SemanticError] exception.
  Other functions which begin with "infer"/"calculate" vary. Usually they return
    a value, but a few do have error conditions.

  All [Error.SemanticError] excpetions are caught by check_program
  which turns the ast or exception into a [Result.t] for external usage

  A type environment {!val:Environment.t} is used to hold variables and functions, including
  Stan math functions. This is a functional map, meaning it is handled immutably.
*)

open Ast

val check_program_exn : untyped_program -> typed_program * Warnings.t list
(**
    Type check a full Stan program.
    Can raise [Errors.SemanticError]
*)

val check_program :
  untyped_program -> (typed_program * Warnings.t list, Semantic_error.t) result
(**
    The safe version of [check_program_exn]. This catches
    all [Errors.SemanticError] exceptions and converts them
    into a [Result.t]
*)

val operator_stan_math_return_type :
     Middle.Operator.t
  -> (Middle.UnsizedType.autodifftype * Middle.UnsizedType.t) list
  -> (Middle.UnsizedType.returntype * Promotion.t list) option

val stan_math_return_type :
     string
  -> (Middle.UnsizedType.autodifftype * Middle.UnsizedType.t) list
  -> Middle.UnsizedType.returntype option

val model_name : string ref
(** A reference to hold the model name. Relevant for checking variable
    clashes and used in code generation. *)

val check_that_all_functions_have_definition : bool ref
(** A switch to determine whether we check that all functions have a definition *)
