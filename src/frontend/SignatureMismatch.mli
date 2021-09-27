open Core_kernel
open Middle

type type_mismatch =
  | DataOnlyError
  | TypeMismatch of UnsizedType.t * UnsizedType.t * details option

and details =
  | SuffixMismatch of unit Fun_kind.suffix * unit Fun_kind.suffix
  | ReturnTypeMismatch of UnsizedType.returntype * UnsizedType.returntype
  | InputMismatch of function_mismatch

and function_mismatch =
  | ArgError of int * type_mismatch
  | ArgNumMismatch of int * int
[@@deriving sexp]

type signature_error =
  (UnsizedType.returntype * (UnsizedType.autodifftype * UnsizedType.t) list)
  * function_mismatch

val check_compatible_arguments_mod_conv :
     (UnsizedType.autodifftype * UnsizedType.t) list
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> function_mismatch option

val returntype :
     Environment.t
  -> string
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> ( UnsizedType.returntype * (bool Middle.Fun_kind.suffix -> Ast.fun_kind)
     , signature_error list * bool )
     result

val check_variadic_args :
     bool
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> UnsizedType.t
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> ((UnsizedType.autodifftype * UnsizedType.t) list * function_mismatch)
     option

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
