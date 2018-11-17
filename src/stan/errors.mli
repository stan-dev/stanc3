(** Source code locations. *)
type location =
  | Location of Lexing.position * Lexing.position  (** delimited location *)
  | Nowhere  (** no location *)

val location_of_lex : Lexing.lexbuf -> location
(** Convert a [Lexing.lexbuf] location to a [location] *)

val make_location : Lexing.position -> Lexing.position -> location
(** [make_location p1 p2] creates a location which starts at [p1] and ends at [p2]. *)

(** Exception [Error (loc, err, msg)] indicates an error of type [err] with error message
    [msg], occurring at location [loc]. *)
exception Error of (location * string * string)

(** Our two kinds of syntax errors *)
type parse_error =
  | Lexing of string * Lexing.position
  | Parsing of string option * Lexing.position * Lexing.position

exception SyntaxError of parse_error

val report_error : Lexing.lexbuf -> parse_error -> unit
(** A pretty printer for syntax errors *)

val fatal_error : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** A fatal error reported by the toplevel. *)

val syntax_error :
  ?loc:location -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** A syntax error reported by the toplevel *)

val semantic_error : ?loc:location -> string -> 'a
(** A semantic error reported by the toplevel *)

val print_error : location * string * string -> unit
(** Print the caught error *)
