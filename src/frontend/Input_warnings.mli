(** A module to contain the mutable state used to track warnings from the
    parser and lexer.
*)

val init : unit -> unit
(** As something of a hack, Input_warnings keeps track of which warnings the lexer has
    emitted as a form of hidden state, which must be initialized and [collect]ed.*)

val collect : unit -> Warnings.t list
(** Returns all of the warnings issued since [init] was called. *)

val add_warning : Middle.Location_span.t -> string -> unit
(** Add a generic warning string to the current list *)

val deprecated : string -> Lexing.position * string -> unit
(** Register that a deprecated language construct has been found. *)

val empty : string -> unit
(** Register that an empty file is being parsed *)

val array_syntax : ?unsized:bool -> Lexing.position * Lexing.position -> unit
(** Warn on the old [real x\[3\]] syntax and suggest the [array] keyword*)

val future_keyword :
  string -> string -> Lexing.position * Lexing.position -> unit
(** Warn on a keyword which will be reserved in the future*)

val drop_array_future : unit -> unit
(** Hack: Remove the most recent warning about array as a future keyword.
    Needed due to the {e other} hack of how we currently parse arrays.
*)
