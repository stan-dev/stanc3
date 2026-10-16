(** Generate fresh (never before used) symbols.

    Uses an internal (mutable) counter per prefix, so symbols generated with one
    prefix do not shift the numbering of symbols with another prefix. *)

val generate : ?prefix:string -> unit -> string

val next : int ref -> prefix:string -> string
(** Increment [counter] and return a symbol with [prefix] numbered by the new
    value, so a pass can number symbols with a counter local to one run. *)

val enter : prefix:string -> unit -> string * (unit -> unit)
(** Generate a symbol with [prefix] and return a function that restores the
    counter for [prefix] to its value before the call. *)

val reset_danger_use_cautiously : unit -> unit
