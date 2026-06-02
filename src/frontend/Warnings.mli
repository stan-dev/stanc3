(** Used for user-facing warning messages *)

type t = Middle.Location_span.t * string

val pp : ?printed_filename:string -> ?code:string -> t Fmt.t
val pp_warnings : ?printed_filename:string -> ?code:string -> t list Fmt.t

val to_grace :
  ?printed_filename:string -> ?code:string -> t -> 'a Grace.Diagnostic.t
