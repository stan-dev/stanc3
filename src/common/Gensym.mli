(** Generate fresh (never before used) symbols.

Uses an internal (mutable) counter
*)

val generate : ?prefix:string -> unit -> string
val enter : unit -> string * (unit -> unit)
val reset_danger_use_cautiously : unit -> unit
