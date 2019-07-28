open Middle
open Core_kernel

type loc_t
type stmt_num = (mtype_loc_ad, (loc_t sexp_opaque[@compare.ignore])) stmt_with
type typed_prog_num = (mtype_loc_ad with_expr, stmt_num) prog
type state_t

val prepare_prog : Program.Typed.t -> typed_prog_num * state_t
val pp_globals : Format.formatter -> state_t -> unit
val pp_smeta : Format.formatter -> loc_t -> unit
val no_span_num : loc_t
