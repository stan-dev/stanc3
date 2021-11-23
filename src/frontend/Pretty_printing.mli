val pp_expression : Format.formatter -> Ast.untyped_expression -> unit
val pp_typed_expression : Format.formatter -> Ast.typed_expression -> unit

val pretty_print_program :
     ?bare_functions:bool
  -> ?line_length:int
  -> ?inline_includes:bool
  -> Ast.untyped_program
  -> string

val pretty_print_typed_program :
     ?bare_functions:bool
  -> ?line_length:int
  -> ?inline_includes:bool
  -> Ast.typed_program
  -> string
