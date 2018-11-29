(** Some plumbing for our compiler errors *)

(** Our type of syntax error information *)
type parse_error =
  | Lexing of string * Lexing.position
  | Includes of string * Lexing.position
  | Parsing of string option * Lexing.position * Lexing.position

(** Exception for Syntax Errors *)
exception SyntaxError of parse_error

(** Exception [SemanticError (loc, msg)] indicates a semantic error with message
    [msg], occurring at location [loc]. *)
exception SemanticError of (Ast.location * string)

(** Exception for Fatal Errors *)
exception FatalError of string

val report_syntax_error : parse_error -> unit
(** A syntax error message used when handling a SyntaxError *)

val report_semantic_error : Ast.location * string -> unit
(** A semantic error message used when handling a SemanticError *)

val semantic_error : ?loc:Ast.location -> string -> 'a
(** Throw a semantic error reported by the toplevel *)

val fatal_error : string -> 'a
(** Throw a fatal error reported by the toplevel *)
(* TODO: Should fatal errors be handled? *)
