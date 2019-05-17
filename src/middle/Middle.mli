open Core_kernel
module Mir : Mir_intf.Mir
module Validation : Validation_intf.Validation

module type Frontend = Frontend_intf.Frontend

val string_of_location : Mir.location -> string
val string_of_location_span : Mir.location_span -> string
val operator_of_string : string -> Mir.operator option
val string_of_operator : Mir.operator -> string
val string_of_internal_fn : Mir.internal_fn -> string
val internal_fn_of_string : string -> Mir.internal_fn option

val internal_funapp :
  Mir.internal_fn -> 'a Mir.with_expr list -> 'a -> 'a Mir.with_expr

val no_loc : Mir.location
val no_span : Mir.location_span
val merge_spans : Mir.location_span -> Mir.location_span -> Mir.location_span
val internal_meta : Mir.mtype_loc_ad
val loop_bottom : Mir.mtype_loc_ad Mir.with_expr
val remove_size : 'a Mir.sizedtype -> Mir.unsizedtype
val zero : Mir.mtype_loc_ad Mir.with_expr

val pp_indexed :
  'a Fmt.t -> Format.formatter -> string * 'a Mir.index list -> unit

val pp_expr_typed_located :
  Format.formatter -> Mir.mtype_loc_ad Mir.with_expr -> unit

val pp_operator : Format.formatter -> Mir.operator -> unit
val pp_unsizedtype : Format.formatter -> Mir.unsizedtype -> unit
val pp_returntype : Format.formatter -> Mir.returntype -> unit

val pp_typed_prog :
     Format.formatter
  -> ('a Mir.with_expr, ('b, 'c) Mir.stmt_with) Mir.prog
  -> unit

val sexp_of_expr_typed_located : 'a Mir.with_expr -> Sexp.t
val sexp_of_stmt_loc : ('a, 'b) Mir.stmt_with -> Sexp.t
val gensym : unit -> string
val gensym_enter : unit -> string * (unit -> unit)
