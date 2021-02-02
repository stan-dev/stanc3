type t = Location_span.t * string

val init : unit -> unit
(** As something of a hack, Warnings keeps track of which warnings the lexer has
    emitted as a form of hidden state, which must be initialized and [collect]ed.*)

val collect : unit -> t list
(** Returns all of the warnings issued since [init] was called. *)

val pp : ?printed_filename:string -> t Fmt.t
val pp_warnings : ?printed_filename:string -> t list Fmt.t

val deprecated : string -> Lexing.position * string -> unit
(** Register that a deprecated language construct has been found. *)
