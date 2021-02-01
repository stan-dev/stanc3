open Middle

type warning_span = Location_span.t * string

val warn_pedantic : Program.Typed.t -> warning_span list
(**
   Collect all pedantic mode warnings and print to a string.
*)

val warn_uninitialized : Program.Typed.t -> warning_span list
(**
   Collect warnings about each variable which is used before being initialized and print to a string.
*)
