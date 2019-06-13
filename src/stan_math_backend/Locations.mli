open Middle

type loc_t
type stmt_num
type typed_prog_num
type state_t

val prepare_prog : typed_prog -> typed_prog_num * state_t
val pp_globals : Format.formatter -> typed_prog_num * state_t -> unit
val pp_smeta : Format.formatter -> loc_t -> unit
