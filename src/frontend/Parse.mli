open Core_kernel
(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

type syntax_error

val syntax_error_message : syntax_error -> string
val syntax_error_location : syntax_error -> Middle.location_span
val render_syntax_error : syntax_error -> string

val parse_file :
     (   Lexing.position
      -> Ast.untyped_program Parser.MenhirInterpreter.checkpoint)
  -> string
  -> (Ast.untyped_program, syntax_error) result
(** A helper function to take a parser, a filename and produce an AST. Under the
    hood, it takes care of Menhir's custom syntax error messages. *)

val parse_string :
     (   Lexing.position
      -> Ast.untyped_program Parser.MenhirInterpreter.checkpoint)
  -> string
  -> (Ast.untyped_program, syntax_error) result
(** A helper function to take a parser, a string and produce an AST. Under the
    hood, it takes care of Menhir's custom syntax error messages. *)
