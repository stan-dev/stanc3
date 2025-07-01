(** a type/semantic checker for Stan ASTs

  Functions which begin with "check_" return a typed version of their input
  Functions which begin with "verify_" return unit if a check succeeds, or else
    throw an {!exception:Frontend.Errors.SemanticError} exception.
  Other functions which begin with "infer"/"calculate" vary. Usually they return
    a value, but a few do have error conditions.

  All [Error.SemanticError] excepetions are caught by check_program
  which turns the ast or exception into a [Result.t] for external usage

  A type environment {!type:Frontend.Environment.t} is used to hold variables
  and functions, including Stan math functions.
  This is a functional map, meaning it is handled immutably.
*)

open Ast

val check_program :
     ?allow_undefined_functions:bool
  -> untyped_program
  -> (typed_program * Warnings.t list, Semantic_error.t) result
(**
    Type check a Stan program, returning a typed program and warnings or
    an error.
    When [allow_undefined_functions] is set to [true], the typechecker
    will not check that all functions have a definition.
*)

val operator_stan_math_return_type :
     Middle.Operator.t
  -> Middle.UnsizedType.argumentlist
  -> (Middle.UnsizedType.returntype * Promotion.t list) option

val stan_math_return_type :
     string
  -> Middle.UnsizedType.argumentlist
  -> Middle.UnsizedType.returntype option

val model_name : string ref
(** A reference to hold the model name. Relevant for checking variable
    clashes and used in code generation. *)
