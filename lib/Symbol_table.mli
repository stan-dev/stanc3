(** Symbol table interface to implement var map *)

type 'a state

val initialize : unit -> 'a state
(** Creates a new symbol table *)

val enter : 'a state -> string -> 'a -> unit
(** Enters a specified identifier with its specified type (or other) information
    into a symbol table  *)

val look : 'a state -> string -> 'a option
(** Looks for an identifier in a symbol table and returns its information if found and None otherwise  *)

val begin_scope : 'a state -> unit
(** Used to start a new local scope which symbols added from now will end up in *)

val end_scope : 'a state -> unit
(** Used to end a local scope, purging the symbol table of all symbols added in that scope *)

val set_read_only : 'a state -> string -> unit
(** Used to add a read only label to an identifier *)

val get_read_only : 'a state -> string -> bool
(** Used to check for a read only label for an identifier *)

val add_is_missing_fun_def : 'a state -> string -> unit
(** Used to add a label to an identifier to say that it represents a declared identifier of function type which is missing a definition *)

val remove_is_missing_fun_def : 'a state -> string -> unit
(** Used to remove a label to an identifier to say that it represents a declared identifier of function type which is missing a definition *)

val is_missing_fun_def : 'a state -> string -> bool
(** Used to check a label to an identifier to say that it represents a declared identifier of function type which is missing a definition *)

val some_fun_is_missing_def : 'a state -> bool
(** Used to check that some identifier of function type is missing a definition *)

val is_global : 'a state -> string -> bool
(** Used to check whether an identifier was declared in global scope *)

val unsafe_clear_symbol_table : 'a state -> unit
(** Used to clear the whole symbol table *)

(* TODO: the following is very ugly, but we seem to need something like it to
 reproduce the (strange) behaviour in the current Stan that local variables
 have a block level that is determined by what has been assigned to them
 rather than by where they were declared. I'm not sure that behaviour makes
 sense unless we use static analysis as well to make sure these assignments
 actually get evaluated in that phase. *)

val unsafe_replace : 'a state -> string -> 'a -> unit
(** Used to replace the information recorded for an identifier *)
