(** Generate fresh (never before used) symbols. *)

val next : prefix:string -> ?extra:string -> unit -> string
val enter : prefix:string -> string * (unit -> unit)
val reset_danger_use_cautiously : unit -> unit
