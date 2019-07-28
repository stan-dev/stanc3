(** Translate from the AST to the MIR *)

val trans_prog : string -> Ast.typed_program -> Middle.Program.Typed.t

val trans_expr :
  Ast.typed_expr_meta Ast.expr_with -> Middle.Expr.Typed.t 
