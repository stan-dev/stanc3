(** Translate from the AST to the MIR *)
open Middle

val gather_declarations :
     Ast.typed_statement Ast.block option
  -> (Expr.Typed.t SizedType.t * Expr.Typed.t Transformation.t * string) list

val trans_prog : string -> Ast.typed_program -> Program.Typed.t
