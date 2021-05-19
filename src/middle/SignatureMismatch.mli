open Core_kernel

type type_mismatch =
  | DataOnlyError
  | TypesMismatch of UnsizedType.t * UnsizedType.t
  | FuncTypeMismatch of UnsizedType.t * UnsizedType.t * function_mismatch

and function_mismatch =
  | SuffixMismatch of unit Fun_kind.suffix * unit Fun_kind.suffix
  | ReturnTypeMismatch of UnsizedType.returntype * UnsizedType.returntype
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

val stan_math_returntype :
     string
  -> (UnsizedType.autodifftype * UnsizedType.t) list
  -> (UnsizedType.returntype, signature_error list * bool) result

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
