(**
This module translates the locations of statements into
an array of messages used in the C++ for providing locations in the Stan
code when an error occurs, and replaces the locations in the MIR tree
with indices into said array.

It also numbers calls to [map_rect] for registration.

*)

open Middle

type state_t
type map_rect_registration_t

val prepare_prog :
  Program.Typed.t -> Program.Numbered.t * state_t * map_rect_registration_t

val no_span_num : Stmt.Numbered.Meta.t
val gen_globals : ?printed_filename:string -> state_t -> Cpp.defn list
val assign_loc : Stmt.Numbered.Meta.t -> Cpp.stmt list

val register_map_rect_functors :
  string -> map_rect_registration_t -> Cpp.defn list
(** Register functiors used for map_rect. *)
