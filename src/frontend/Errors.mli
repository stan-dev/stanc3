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

val pp : ?printed_filename:string -> t Fmt.t
val to_string : t -> string

(** Exception for Fatal Errors. These should perhaps be left unhandled,
    so we can trace their origin. *)
exception FatalError of string

val fatal_error : ?msg:string -> unit -> 'a
(** Throw a fatal error reported by the toplevel *)

val pp_syntax_error :
  ?printed_filename:string -> Format.formatter -> syntax_error -> unit
(** A syntax error message used when handling a SyntaxError *)

val pp_semantic_error :
  ?printed_filename:string -> Format.formatter -> Semantic_error.t -> unit
(** A semantic error message used when handling a SemanticError *)
