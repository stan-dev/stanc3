(** Translate from the AST to the MIR *)
open Middle

val gather_data :
     Ast.typed_program
  -> (Expr.Typed.t SizedType.t * Expr.Typed.t Transformation.t * string) list

val trans_prog : string -> Ast.typed_program -> Program.Typed.t
