(** Symbol table interface to implement var map *)

type 'a t

val empty : 'a t
(** The empty symbol table *)

val enter : 'a t -> string -> 'a -> 'a t
(** Enters a specified identifier with its specified type (or other) information
    into a symbol table  *)

val look : 'a t -> string -> 'a option
(** Looks for an identifier in a symbol table and returns its information if found and None otherwise  *)

val begin_scope : 'a t -> 'a t
(** Used to start a new local scope which symbols added from now will end up in *)

val end_scope : 'a t -> 'a t
(** Used to end a local scope, purging the symbol table of all symbols added in that scope *)

val set_read_only : 'a t -> string -> 'a t
(** Used to add a read only label to an identifier *)

val get_read_only : 'a t -> string -> bool
(** Used to check for a read only label for an identifier *)

val set_is_assigned : 'a t -> string -> 'a t
(** Label an identifier as having been assigned to *)

val set_is_unassigned : 'a t -> string -> 'a t
(** Label an identifier as not having been assigned to *)

val check_is_unassigned : 'a t -> string -> bool
(** Check whether an identifier is labelled as unassigned *)

val check_some_id_is_unassigned : 'a t -> bool
(** Used to check whether some identifier is labelled as unassigned *)

val is_global : 'a t -> string -> bool
(** Used to check whether an identifier was declared in global scope *)

val debug : 'a t -> unit
