(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Core
open Includes_intf

val parse_file :
     (Lexing.position -> Ast.untyped_program Parser.MenhirInterpreter.checkpoint)
  -> string
  -> (Ast.untyped_program, Errors.t) result * Warnings.t list
(** A helper function to take a parser, a filename and produce an AST. Under the
    hood, it takes care of Menhir's custom syntax error messages. *)

val parse_in_memory :
     (Lexing.position -> Ast.untyped_program Parser.MenhirInterpreter.checkpoint)
  -> string
  -> (Ast.untyped_program, Errors.t) result * Warnings.t list
(** Parse from code stored in the string argument and handle #includes
through the [In_memory_includes] module *)

val parse_string :
     (module LEXBUF_LOCATOR)
  -> (   Lexing.position
      -> Ast.untyped_program Parser.MenhirInterpreter.checkpoint)
  -> string
  -> (Ast.untyped_program, Errors.t) result * Warnings.t list
(** Parse from code stored in the string, generic over how #includes are found *)
