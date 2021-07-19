type t = Location_span.t * string

val pp : ?printed_filename:string -> t Fmt.t
val pp_warnings : ?printed_filename:string -> t list Fmt.t
