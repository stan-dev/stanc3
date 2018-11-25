(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

val parse_file :
     (   Lexing.position
      -> Ast.untyped_program list Parser.MenhirInterpreter.checkpoint)
  -> string
  -> Ast.untyped_program list
