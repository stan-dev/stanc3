(*
open Mir
open Core_kernel

let rec ast_to_mir_expr (Ast.TypedExpr (e, meta)) =
  let raise_expr msg e = raise_s [%message msg (e: Ast.typed_expression Ast.expression)]in
  match e with
  | Ast.TernaryOp (Ast.TypedExpr (e', meta'), ifb, elseb) ->
    let op = (match e' with
        | Ast.InfixOp (lhs, op, rhs) ->
          let
      | _ -> raise_expr "Expected conditional infix operator, got: " e
      )
  | Ast.InfixOp (_, _, _) -> (??)
  | Ast.PrefixOp (_, _) -> (??)
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
