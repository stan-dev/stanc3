(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)
open Core_kernel

val parse_file :
     (Lexing.position -> 'a Parser.MenhirInterpreter.checkpoint)
  -> string
  -> ('a, Middle.Errors.t) result * Middle.Warnings.t list
(** A helper function to take a parser, a filename and produce an AST. Under the
    hood, it takes care of Menhir's custom syntax error messages. *)

val parse_string :
     (Lexing.position -> 'a Parser.MenhirInterpreter.checkpoint)
  -> string
  -> ('a, Middle.Errors.t) result * Middle.Warnings.t list
(** A helper function to take a parser, a string and produce an AST. Under the
    hood, it takes care of Menhir's custom syntax error messages. *)
