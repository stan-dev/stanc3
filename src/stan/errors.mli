(** Our two kinds of syntax error information *)
type parse_error =
  | Lexing of string * Lexing.position
  | Parsing of string option * Lexing.position * Lexing.position

(** Exception for Syntax Errors *)
exception SyntaxError of parse_error

val report_syntax_error : parse_error -> unit
(** A syntax error message used when handling a SyntaxError *)

(** Source code locations for semantic errors. *)
type location =
  | Location of Lexing.position * Lexing.position  (** delimited location *)
  | Nowhere  (** no location *)

val location_of_lex : Lexing.lexbuf -> location
(** Convert a [Lexing.lexbuf] location to a [location] *)

val make_location : Lexing.position -> Lexing.position -> location
(** [make_location p1 p2] creates a location which starts at [p1] and ends at [p2]. *)

(** Exception [SemanticError (loc, msg)] indicates a semantic error with message
    [msg], occurring at location [loc]. *)
exception SemanticError of (location * string)

val semantic_error : ?loc:location -> string -> 'a
(** Throw a semantic error reported by the toplevel *)

val report_semantic_error : location * string -> unit
(** A semantic error message used when handling a SemanticError *)
