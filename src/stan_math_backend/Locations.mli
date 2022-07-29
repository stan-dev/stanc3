(** Create messages used in the C++ for providing locations in the Stan
code when an error occurs *)

open Middle

type state_t

val prepare_prog : Program.Typed.t -> Program.Numbered.t * state_t
val no_span_num : Stmt.Numbered.Meta.t
val gen_globals : state_t -> Cpp.defn list
val create_loc_assignment : Stmt.Numbered.Meta.t -> Cpp.stmt list
