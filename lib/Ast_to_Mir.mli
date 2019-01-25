(** Translate from the AST to the MIR *)

val trans_prog : string -> Ast.typed_program -> Mir.stmt_loc Mir.prog
