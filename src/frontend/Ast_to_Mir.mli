(** Translate from the AST to the MIR *)

val trans_prog : string -> Ast.typed_program -> Middle.typed_prog
val trans_expr : Ast.typed_expression -> Middle.mtype_loc_ad Middle.with_expr
