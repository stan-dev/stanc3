(** Some functions for checking whether conversions between types are allowed *)
open Ast

val check_compatible_arguments_mod_conv :
     string
  -> (originblock * unsizedtype) list
  -> (originblock * unsizedtype) list
  -> bool

val check_of_same_type_mod_array_conv :
  string -> unsizedtype -> unsizedtype -> bool

val check_of_compatible_return_type :
  returntype -> statement_returntype -> bool
