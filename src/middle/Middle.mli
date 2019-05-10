include module type of Mir
open Core_kernel

val string_of_location : location -> string
val string_of_location_span : location_span -> string
val operator_of_string : string -> operator option
val string_of_operator : operator -> string
val string_of_internal_fn : internal_fn -> string
val internal_fn_of_string : string -> internal_fn option
val no_loc : location
val no_span : location_span
val merge_spans : location_span -> location_span -> location_span
val internal_meta : mtype_loc_ad
val loop_bottom : mtype_loc_ad with_expr
val remove_size : 'a sizedtype -> unsizedtype

val pp_typed_prog :
  Format.formatter -> ('a with_expr, ('b, 'c) stmt_with) prog -> unit

val sexp_of_expr_typed_located : 'a with_expr -> Sexp.t
