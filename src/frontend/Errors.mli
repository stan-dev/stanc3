(** Some plumbing for our compiler errors *)

(** Our type of syntax error information *)
type syntax_error =
  | Lexing of Middle.Location.t
  | UnexpectedEOF of Middle.Location.t
  | Include of string * Middle.Location.t
  | Parsing of string * Middle.Location_span.t

(** Exception for Syntax Errors *)
exception SyntaxError of syntax_error

(** Exception [SemanticError (loc, msg)] indicates a semantic error with message
    [msg], occurring at location [loc]. *)
exception SemanticError of Semantic_error.t

type t =
  | FileNotFound of string
  | Syntax_error of syntax_error
  | Semantic_error of Semantic_error.t

val pp : ?printed_filename:string -> ?code:string -> t Fmt.t
(** Pretty-printer for error type [t]. Replace occurances of
  filename from locations with [printed_filename], if supplied.
  If [code] is supplied, read context from that string. Otherwise,
  it will attempt to open the original file.
 *)

val to_string : t -> string
(** Format an error [t] as a string. Should only be used in testing!
  For user facing code, prefer [pp]
  *)

val pp_semantic_error :
     ?printed_filename:string
  -> ?code:string
  -> Format.formatter
  -> Semantic_error.t
  -> unit
(** A semantic error message used when handling a SemanticError *)
