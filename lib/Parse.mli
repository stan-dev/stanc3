(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

(** A helper function to take a parser, a filename and produce an AST. Under the
    hood, it takes care of Menhir's custom syntax error messages. *)
val parse_file :
     (Lexing.position -> Ast.untyped_program Parser.MenhirInterpreter.checkpoint)
  -> string
  -> Ast.untyped_program
