(*
open Mir

let rec ast_to_mir_expr (Ast.TypedExpr (e, meta)) =
  match e with
  | Ast.TernaryOp (lhs, op, rhs) ->
    Cond (ast_to_mir_expr lhs, (match op with
        | Ast.TypedExpr (e, meta') ->
          (match e with
           | Ast.InfixOp (_, _, _) -> (??)
      )
      , ast_to_mir_expr rhs)
  | Ast.InfixOp (_, _, _) -> (??)
  | Ast.PrefixOp (_, _) -> (??)
  | Ast.PostfixOp (_, _) -> (??)
  | Ast.Variable _ -> (??)
  | Ast.IntNumeral _ -> (??)
  | Ast.RealNumeral _ -> (??)
  | Ast.FunApp (_, _) -> (??)
  | Ast.CondFunApp (_, _) -> (??)
  | Ast.GetLP -> (??)
  | Ast.GetTarget -> (??)
  | Ast.ArrayExpr _ -> (??)
  | Ast.RowVectorExpr _ -> (??)
  | Ast.Paren _ -> (??)
  | Ast.Indexed (_, _) -> (??)
*)
