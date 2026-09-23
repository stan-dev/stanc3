type t

(** This is the type of format strings with no %-placeholders. Why? Because we
    can still include other formatting indicators, including semantic tags used
    for things like ["@{<green>Colored text@}"]. These strings usually come from
    the [parser.messages] text file. *)
type styled_text = (unit, Format.formatter, unit) format

val to_grace :
  ?printed_filename:string -> ?code:string -> t -> 'a Grace.Diagnostic.t

(** Exception-based control flow is useful during parsing/lexing. These helpers
    are used to try to keep all that logic in one place *)

val unexpected_eof : Middle.Location_span.t -> 'a
val unexpected_character : Middle.Location_span.t -> 'a
val include_error : ?note:string -> string -> Middle.Location_span.t -> 'a
val parse_error : styled_text -> Middle.Location_span.t -> 'a
val try_with : (unit -> 'a) -> ('a, t) result
