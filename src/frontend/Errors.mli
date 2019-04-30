(** Some plumbing for our compiler errors *)
open Mir

(** Our type of syntax error information *)
type parse_error =
  | Lexing of string * location
  | Include of string * location
  | Parsing of string * location_span

(** Exception for Syntax Errors *)
exception SyntaxError of parse_error

(** Exception [SemanticError (loc, msg)] indicates a semantic error with message
    [msg], occurring at location [loc]. *)
exception SemanticError of (string * location_span)

(** Exception for Fatal Errors. These should perhaps be left unhandled,
    so we can trace their origin. *)
exception FatalError of string

val semantic_error : loc:location_span -> string -> 'a
(** Throw a semantic error reported by the toplevel *)

val fatal_error : ?msg:string -> unit -> 'a
(** Throw a fatal error reported by the toplevel *)

val loc_span_of_pos : Lexing.position -> Lexing.position -> location_span
(** Take the AST.location_span corresponding to a pair of Lexing.position's *)

val location_of_position : Lexing.position -> location
(** Take the AST.location corresponding to a Lexing.position *)

val report_syntax_error : parse_error -> unit
(** A syntax error message used when handling a SyntaxError *)

val report_semantic_error : string * location_span -> unit
(** A semantic error message used when handling a SemanticError *)

val warn_deprecated : Lexing.position * string -> unit
(** Warn that a language construct is deprecated *)
