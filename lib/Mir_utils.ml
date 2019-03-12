open Core_kernel
open Mir
open Dataflow_types

let vexpr_of_expr_exn (ex : expr) : vexpr =
  match ex with
  | Var s -> VVar s
  | _ -> raise (Failure "Non-var expression found, but var expected")

(**
   The set of variables in an expression, including inside an index

   For use in RHS sets, not LHS assignment sets, except in a target term
*)
let rec expr_var_set (ex : expr) : vexpr Set.Poly.t =
  let union_recur exprs =
    Set.Poly.union_list (List.map exprs ~f:expr_var_set)
  in
  match ex with
  | Var s -> Set.Poly.singleton (VVar s)
  | Lit _ -> Set.Poly.empty
  | FunApp (_, exprs) -> union_recur exprs
  | BinOp (expr1, _, expr2) -> union_recur [expr1; expr2]
  | TernaryIf (expr1, expr2, expr3) -> union_recur [expr1; expr2; expr3]
  | Indexed (expr, ix) ->
      Set.Poly.union_list (expr_var_set expr :: List.map ix ~f:index_var_set)

and index_var_set (ix : index) : vexpr Set.Poly.t =
  match ix with
  | All -> Set.Poly.empty
  | Single expr -> expr_var_set expr
  | Upfrom expr -> expr_var_set expr
  | Downfrom expr -> expr_var_set expr
  | Between (expr1, expr2) ->
      Set.Poly.union (expr_var_set expr1) (expr_var_set expr2)
  | MultiIndex expr -> expr_var_set expr

(**
   The variable being assigned to when `ex` is LHS
*)
let expr_assigned_var (ex : expr) : vexpr =
  match ex with
  | Var s -> VVar s
  | Indexed (Var s, _) -> VVar s
  | _ -> raise (Failure "Unimplemented: analysis of assigning to non-var")

