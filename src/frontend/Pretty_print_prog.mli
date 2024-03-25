(** Pretty print a complete Stan program.

This module relies on [Parser] to be able to run a "sanity check" on the
output of the pretty printer, confirming that the output is a valid Stan
program and matches the input program.

However, the parser relies on some pretty printing functions to be able to
produce nicer error messages, hence the split between [Pretty_printing]
and this module.
*)

val pretty_print_typed_program :
     ?bare_functions:bool
  -> ?line_length:int
  -> ?inline_includes:bool
  -> ?strip_comments:bool
  -> Ast.typed_program
  -> string
