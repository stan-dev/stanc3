(** Generate fresh symbols.

    There is no global counter: each pass or translation stage creates its own
    [int ref] and passes it down, so symbols from one stage never shift the
    numbering of another. *)

val next : int ref -> prefix:string -> string
(** Increment [count] and return a symbol with [prefix] numbered by the new
    value. *)

val enter : int ref -> prefix:string -> string * (unit -> unit)
(** Generate a symbol like [next] and return a function that restores [count] to
    its value before the call. *)
