(** A module to contain the mutable state used to track warnings from the parser
    and lexer. *)

val init : unit -> unit
(** As something of a hack, Input_warnings keeps track of which warnings the
    lexer has emitted as a form of hidden state, which must be initialized and
    [collect]ed.*)

val collect : unit -> Warnings.t list
(** Returns all of the warnings issued since [init] was called. *)

val empty : unit -> unit
(** Register that an empty file is being parsed *)
