open Core_kernel
open Middle

type type_mismatch = private
  | DataOnlyError
  | TypeMismatch of UnsizedType.t * UnsizedType.t * details option

and details = private
  | SuffixMismatch of unit Fun_kind.suffix * unit Fun_kind.suffix
  | ReturnTypeMismatch of UnsizedType.returntype * UnsizedType.returntype
  | InputMismatch of function_mismatch

and function_mismatch = private
  | ArgError of int * type_mismatch
  | ArgNumMismatch of int * int
[@@deriving sexp]

type signature_error =
  (UnsizedType.returntype * (UnsizedType.autodifftype * UnsizedType.t) list)
  * function_mismatch

type ('unique, 'error) generic_match_result =
  | UniqueMatch of 'unique
  | AmbiguousMatch of
      (UnsizedType.returntype * (UnsizedType.autodifftype * UnsizedType.t) list)
      list
  | SignatureErrors of 'error

(** The match result for general (non-variadic) functions *)
type match_result =
  ( UnsizedType.returntype
    * (bool Middle.Fun_kind.suffix -> Ast.fun_kind)
    * Promotion.t list
  , signature_error list * bool )
  generic_match_result

val check_of_same_type_mod_conv :
  UnsizedType.t -> UnsizedType.t -> (Promotion.t, type_mismatch) result

val check_compatible_arguments_mod_conv :
     (UnsizedType.autodifftype * UnsizedType.t) list
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> (Promotion.t list, function_mismatch) result

val unique_minimum_promotion :
  ('a * Promotion.t list) list -> ('a * Promotion.t list, 'a list option) result

val matching_function :
     Environment.t
  -> string
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> match_result
(** Searches for a function of the given name which can
    support the required argument types.
    Requires a unique minimum option under type promotion
*)

val check_variadic_args :
     bool
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> UnsizedType.t
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> ( UnsizedType.t * Promotion.t list
     , (UnsizedType.autodifftype * UnsizedType.t) list * function_mismatch )
     result
(** Check variadic function arguments.
      If a match is found, returns [Ok] of the function type and a list of promotions (see [promote])
      If none is found, returns [Error] of the list of args and a function_mismatch.
     *)

val pp_signature_mismatch :
     Format.formatter
  -> string
     * UnsizedType.t list
     * ( ( ( UnsizedType.returntype
           * (UnsizedType.autodifftype * UnsizedType.t) list )
         * function_mismatch )
         list
       * bool )
  -> unit

val compare_errors : function_mismatch -> function_mismatch -> int
