open Middle

type warning_span = Location_span.t * string

val pp_warning_span : ?printed_filename:string -> warning_span Fmt.t
val pp_warnings : ?printed_filename:string -> warning_span list Fmt.t

val warn_pedantic : Program.Typed.t -> warning_span list
(**
   Collect all pedantic mode warnings and print to a string.
*)

val warn_uninitialized : Program.Typed.t -> warning_span list
(**
   Collect warnings about each variable which is used before being initialized and print to a string.
*)
