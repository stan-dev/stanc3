(** Generate fresh (never before used) symbols. *)

module type Gensym = sig
  val next : ?infix:string -> unit -> string
  val enter : unit -> string * (unit -> unit)
end

val make : prefix:string -> (module Gensym)
val reset_danger_use_cautiously : unit -> unit
