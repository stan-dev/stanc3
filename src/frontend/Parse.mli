(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)
open Core_kernel

val parse_file :
     (Lexing.position -> Ast.untyped_program Parser.MenhirInterpreter.checkpoint)
  -> string
  -> (Ast.untyped_program, Middle.Errors.t) result * Middle.Warnings.t list
(** A helper function to take a parser, a filename and produce an AST. Under the
    hood, it takes care of Menhir's custom syntax error messages. *)

val parse_string :
     (Lexing.position -> Ast.untyped_program Parser.MenhirInterpreter.checkpoint)
  -> string
  -> (Ast.untyped_program, Middle.Errors.t) result * Middle.Warnings.t list
(** A helper function to take a parser, a string and produce an AST. Under the
    hood, it takes care of Menhir's custom syntax error messages. *)
