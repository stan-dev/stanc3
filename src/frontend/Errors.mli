(** Some plumbing for our compiler errors *)

(** Our type of syntax error information *)
type syntax_error =
  | Lexing of Middle.Location.t
  | UnexpectedEOF of Middle.Location.t
  | Include of string * Middle.Location.t
  | Parsing of string * Middle.Location_span.t

(** Exception for Syntax Errors *)
exception SyntaxError of syntax_error

type t =
  | FileNotFound of string
  | Syntax_error of syntax_error
  | Semantic_error of Semantic_error.t
  | DebugDataError of (Middle.Location_span.t * string)

val pp : ?printed_filename:string -> ?code:string -> t Fmt.t
(** Pretty-printer for error type [t]. Replace occurances of
  filename from locations with [printed_filename], if supplied.
  If [code] is supplied, read context from that string. Otherwise,
  it will attempt to open the original file.
 *)
