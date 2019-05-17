(** Translate from the AST to the MIR *)
open Middle

val trans_prog : string -> Ast.typed_program -> Mir.typed_prog
