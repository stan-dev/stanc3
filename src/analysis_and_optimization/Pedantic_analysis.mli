open Middle

val sprint_warn_pedantic : Program.Typed.t -> string
(**
   Collect all pedantic mode warnings and print to a string.
*)

val sprint_warn_uninitialized : Program.Typed.t -> string
(**
   Collect warnings about each variable which is used before being initialized and print to a string.
*)
