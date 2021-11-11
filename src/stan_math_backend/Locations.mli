(** Create messages used in the C++ for providing locations in the Stan
code when an error occurs *)

open Middle

type state_t

val prepare_prog : Program.Typed.t -> Program.Numbered.t * state_t
val pp_globals : Format.formatter -> state_t -> unit
val pp_smeta : Format.formatter -> Stmt.Numbered.Meta.t -> unit
val no_span_num : Stmt.Numbered.Meta.t
