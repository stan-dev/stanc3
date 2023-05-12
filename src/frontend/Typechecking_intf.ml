open Ast

(** Signature for a Stan typechecker *)
module type TYPECHECKER = sig
  val check_program_exn : untyped_program -> typed_program * Warnings.t list
  (**
        Type check a full Stan program.
        Can raise [Errors.SemanticError]
    *)

  val check_program :
       untyped_program
    -> (typed_program * Warnings.t list, Semantic_error.t) result
  (**
        The safe version of [check_program_exn]. This catches
        all [Errors.SemanticError] exceptions and converts them
        into a [Result.t]
    *)

  val operator_return_type :
       Middle.Operator.t
    -> (Middle.UnsizedType.autodifftype * Middle.UnsizedType.t) list
    -> (Middle.UnsizedType.returntype * Promotion.t list) option

  val library_function_return_type :
       string
    -> (Middle.UnsizedType.autodifftype * Middle.UnsizedType.t) list
    -> Middle.UnsizedType.returntype option
end
