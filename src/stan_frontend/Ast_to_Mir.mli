(** Translate from the AST to the MIR *)
open Stan_mir

val trans_prog :
     string
  -> Ast.typed_program
  -> (Mir.expr_typed_located, Mir.stmt_loc) Mir.prog
