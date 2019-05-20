include module type of Mir
open Core_kernel

val string_of_location : location -> string
val string_of_location_span : location_span -> string
val operator_of_string : string -> operator option
val string_of_operator : operator -> string
val string_of_internal_fn : internal_fn -> string
val internal_fn_of_string : string -> internal_fn option
val internal_funapp : internal_fn -> 'a with_expr list -> 'a -> 'a with_expr
val no_loc : location
val no_span : location_span
val merge_spans : location_span -> location_span -> location_span
val internal_meta : mtype_loc_ad
val loop_bottom : mtype_loc_ad with_expr
val zero : mtype_loc_ad with_expr
val pp_indexed : 'a Fmt.t -> Format.formatter -> string * 'a index list -> unit
val pp_expr_typed_located : Format.formatter -> mtype_loc_ad with_expr -> unit
val remove_size : 'a sizedtype -> unsizedtype
val remove_possible_size : 'a possiblysizedtype -> unsizedtype

val pp_typed_prog :
  Format.formatter -> ('a with_expr, ('b, 'c) stmt_with) prog -> unit

val sexp_of_expr_typed_located : 'a with_expr -> Sexp.t

val map_rec_expr :
     (expr_typed_located expr -> expr_typed_located expr)
  -> expr_typed_located
  -> expr_typed_located

val map_rec_expr_state :
     ('s -> expr_typed_located expr -> expr_typed_located expr * 's)
  -> 's
  -> expr_typed_located
  -> expr_typed_located * 's

val map_rec_stmt_loc :
     (   (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement)
  -> stmt_loc
  -> stmt_loc

val map_rec_state_stmt_loc :
     (   's
      -> (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement * 's)
  -> 's
  -> stmt_loc
  -> stmt_loc * 's

val map_rec_stmt_loc_num :
     (int, stmt_loc_num) Map.Poly.t
  -> (   int
      -> (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement)
  -> stmt_loc_num
  -> stmt_loc

val map_rec_state_stmt_loc_num :
     (int, stmt_loc_num) Map.Poly.t
  -> (   int
      -> 's
      -> (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement * 's)
  -> 's
  -> stmt_loc_num
  -> stmt_loc * 's

val stmt_loc_of_stmt_loc_num :
  (int, stmt_loc_num) Map.Poly.t -> stmt_loc_num -> stmt_loc

val statement_stmt_loc_of_statement_stmt_loc_num :
     (int, stmt_loc_num) Map.Poly.t
  -> (mtype_loc_ad with_expr, int) statement
  -> ( mtype_loc_ad with_expr
     , (mtype_loc_ad, location_span) stmt_with )
     statement

val unnumbered_prog_of_numbered_prog :
     (int, stmt_loc_num) Map.Poly.t
  -> ('a -> 'b)
  -> (stmt_loc_num, 'a) prog
  -> (stmt_loc, 'b) prog

val gensym : unit -> string
val gensym_enter : unit -> string * (unit -> unit)
val gensym_reset_danger_use_cautiously : unit -> unit
