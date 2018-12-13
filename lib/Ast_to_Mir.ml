(*
open Mir
open Core_kernel

let rec ast_to_mir_expr (Ast.TypedExpr (e, meta)) =
  let raise_expr msg e = raise_s [%message msg (e: Ast.typed_expression Ast.expression)]in
  match e with
  | Ast.TernaryIf (cond, ifb, elseb) ->
    TernaryIf(ast_to_mir_expr cond, ast_to_mir_expr ifb, ast_to_mir_expr elseb)
  | Ast.BinOp (lhs, op, rhs) -> BinOp(ast_to_mir_expr lhs, op, ast_to_mir_expr rhs)
  | Ast.PrefixOp (op, e) -> FnApp(Operators.)
  | Ast.PostfixOp (_, _) -> (??)
  | Ast.Variable _ -> (??)
  | Ast.IntNumeral _ -> (??)
  | Ast.RealNumeral _ -> (??)
  | Ast.FunApp (_, _) -> (??)
  | Ast.CondDistApp (_, _) -> (??)
  | Ast.GetLP -> (??)
  | Ast.GetTarget -> (??)
  | Ast.ArrayExpr _ -> (??)
  | Ast.RowVectorExpr _ -> (??)
  | Ast.Paren _ -> (??)
  | Ast.Indexed (_, _) -> (??)
*)
