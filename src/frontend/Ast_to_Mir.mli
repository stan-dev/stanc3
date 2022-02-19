(** Translate from the AST to the MIR *)
open Middle

val trans_data :
     Ast.typed_program
  -> ( Expr.Typed.Meta.t Expr.Fixed.t SizedType.t
     * Expr.Typed.Meta.t Expr.Fixed.t Transformation.t
     * string )
     list

val trans_prog : string -> Ast.typed_program -> Program.Typed.t
