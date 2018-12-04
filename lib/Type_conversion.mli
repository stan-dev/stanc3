(** Some functions for checking whether conversions between types are allowed *)
open Ast

(** Check that the rhs list of function argument types can be converted to the
    lhs *)
val check_compatible_arguments_mod_conv :
     string
  -> (originblock * unsizedtype) list
  -> (originblock * unsizedtype) list
  -> bool

(** Check that the rhs type can be converted to the lhs, where we allow
    conversion inside an array constructor *)
val check_of_same_type_mod_array_conv :
  string -> unsizedtype -> unsizedtype -> bool

(** Check that the rhs function body statement_returntype is compatible with the
    lhs returntype *)
val check_of_compatible_return_type :
  returntype -> statement_returntype -> bool
