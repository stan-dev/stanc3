open Core_kernel
open Mir
open Dataflow_types

(** See interface file *)
let vexpr_of_expr_exn (ex : expr_typed_located) : vexpr =
  match ex.texpr with
  | Var s -> VVar s
  | _ -> raise (Failure "Non-var expression found, but var expected")

(** See interface file *)
let rec expr_var_set (ex : expr_typed_located) : vexpr Set.Poly.t =
  let union_recur exprs =
    Set.Poly.union_list (List.map exprs ~f:expr_var_set)
  in
  match ex.texpr with
  | Var s -> Set.Poly.singleton (VVar s)
  | Lit _ -> Set.Poly.empty
  | FunApp (_, exprs) -> union_recur exprs
  | TernaryIf (expr1, expr2, expr3) -> union_recur [expr1; expr2; expr3]
  | Indexed (expr, ix) ->
      Set.Poly.union_list (expr_var_set expr :: List.map ix ~f:index_var_set)

and index_var_set (ix : expr_typed_located index) : vexpr Set.Poly.t =
  match ix with
  | All -> Set.Poly.empty
  | Single expr -> expr_var_set expr
  | Upfrom expr -> expr_var_set expr
  | Downfrom expr -> expr_var_set expr
  | Between (expr1, expr2) ->
      Set.Poly.union (expr_var_set expr1) (expr_var_set expr2)
  | MultiIndex expr -> expr_var_set expr

(** See interface file *)
let expr_assigned_var (ex : expr_typed_located) : vexpr =
  match ex.texpr with
  | Var s -> VVar s
  | Indexed ({texpr= Var s; _}, _) -> VVar s
  | _ -> raise (Failure "Unimplemented: analysis of assigning to non-var")

(** See interface file *)
let rec summation_terms (rhs : expr_typed_located) : expr_typed_located list =
  match rhs.texpr with
  | FunApp ("Plus__", [e1; e2]) ->
      List.append (summation_terms e1) (summation_terms e2)
  | _ -> [rhs]

(** See interface file *)
let stmt_of_block b = {stmt= SList b; sloc= Mir.no_span}
