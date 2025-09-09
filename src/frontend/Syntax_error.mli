type t

val location : t -> Middle.Location_span.t
val kind : t -> string
val pp : Format.formatter -> t -> unit

(** Exception-based control flow is useful during parsing/lexing.
    These helpers are used to try to keep all that logic in one place*)

val unexpected_eof : Middle.Location_span.t -> 'a
val unexpected_character : Middle.Location_span.t -> 'a
val include_error : string -> Middle.Location_span.t -> 'a
val parse_error : string -> Middle.Location_span.t -> 'a
val try_with : (unit -> 'a) -> ('a, t) result
