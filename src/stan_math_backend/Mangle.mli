(** Mangle variables which are C++ reserved words into
    valid C++ identifiers.

    This is done in Transform_Mir. When one of these
    names is emitted as a string, we use remove_prefix
    such that this mangling is opaque to the user -
    e.g., a cmdstan output file would still have a column
    called "public", even if internally we called this
    "_stan_public"

    NB: the use of a leading _ is essential, because
    the lexer won't allow this in a user-created variable.
*)

val remove_prefix : string -> string
(** Used in code generation so that this
   mangling is opaque to the interfaces (e.g. cmdstan)
*)

val add_prefix_to_kwrds : string -> string
