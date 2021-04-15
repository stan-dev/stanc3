(** Symbol table interface to implement var map *)

type 'value state

val initialize : unit -> 'value state
(** Creates a new symbol table *)

val enter : 'value state -> string -> 'value -> unit
(** Enters a specified identifier with its specified type (or other) information
    into a symbol table  *)

val look : 'value state -> string -> 'value option
(** Looks for an identifier in a symbol table and returns its information if found and None otherwise  *)

val begin_scope : 'value state -> unit
(** Used to start a new local scope which symbols added from now will end up in *)

val end_scope : 'value state -> unit
(** Used to end a local scope, purging the symbol table of all symbols added in that scope *)

val set_read_only : 'value state -> string -> unit
(** Used to add a read only label to an identifier *)

val get_read_only : 'value state -> string -> bool
(** Used to check for a read only label for an identifier *)

val set_is_assigned : 'value state -> string -> unit
(** Label an identifier as having been assigned to *)

val set_is_unassigned : 'value state -> string -> unit
(** Label an identifier as not having been assigned to *)

val check_is_unassigned : 'value state -> string -> bool
(** Check whether an identifier is labelled as unassigned *)

val check_some_id_is_unassigned : 'value state -> bool
(** Used to check whether some identifier is labelled as unassigned *)

val is_global : 'value state -> string -> bool
(** Used to check whether an identifier was declared in global scope *)

val unsafe_clear_symbol_table : 'value state -> unit
(** Used to clear the whole symbol table *)
