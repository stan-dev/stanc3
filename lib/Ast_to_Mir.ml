open Mir
open Core_kernel

let rec trans_expr {Ast.expr_typed; expr_typed_type= t; _} =
  match expr_typed with
  | Ast.TernaryIf (cond, ifb, elseb) ->
      TernaryIf (trans_expr cond, trans_expr ifb, trans_expr elseb)
  | Ast.BinOp (lhs, op, rhs) -> BinOp (trans_expr lhs, op, trans_expr rhs)
  | Ast.PrefixOp (op, e) | Ast.PostfixOp (e, op) ->
      FnApp (Operators.operator_name op, [trans_expr e])
  | Ast.Variable {name; _} -> Var name
  | Ast.IntNumeral x -> Lit (Int, x)
  | Ast.RealNumeral x -> Lit (Real, x)
  | Ast.FunApp ({name; _}, args) | Ast.CondDistApp ({name; _}, args) ->
      FnApp (name, List.map ~f:trans_expr args)
  | Ast.GetLP | Ast.GetTarget -> Var "target"
  | Ast.ArrayExpr eles -> FnApp ("make_array", List.map ~f:trans_expr eles)
  | Ast.RowVectorExpr eles -> FnApp ("make_rowvec", List.map ~f:trans_expr eles)
  | Ast.Paren x -> trans_expr x
  | Ast.Indexed (lhs, indices) ->
      Indexed (trans_expr lhs, List.map ~f:(trans_idx t) indices)

and trans_idx t = function
  | Ast.All -> All
  | Ast.Upfrom e -> Upfrom (trans_expr e)
  | Ast.Downfrom e -> Downfrom (trans_expr e)
  | Ast.Between (lb, ub) -> Between (trans_expr lb, trans_expr ub)
  | Ast.Single e ->
      if t = e.expr_typed_type then MultiIndex (trans_expr e)
      else Single (trans_expr e)
