type t = Lexing.position * string

val init : unit -> unit
(** As something of a hack, Warnings keeps track of which warnings the lexer has
    emitted as a form of hidden state, which must be initialized and [collect]ed.*)

val collect : unit -> t list
(** Returns all of the warnings issued since [init] was called. *)

val pp : (Lexing.position * string) Fmt.t

val deprecated : t -> unit
(** Register that a deprecated language construct has been found. *)
