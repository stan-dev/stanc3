open Core_kernel
open Middle
open Dataflow_types
open Mir_utils

(*open Dataflow_utils*)
open Dependence_analysis

type factor =
  | TargetTerm of Expr.Typed.t
  | Reject
  | LPFunction of (string * Expr.Typed.t list)
[@@deriving sexp, hash, compare]

let extract_factors_statement stmt =
  match stmt with
  | Stmt.Fixed.Pattern.TargetPE e ->
      List.map (summation_terms e) ~f:(fun x -> TargetTerm x)
  | NRFunApp (_, f, _) when Internal_fun.of_string_opt f = Some FnReject ->
      [Reject]
  | NRFunApp (_, s, args) when String.suffix s 3 = "_lp" ->
      [LPFunction (s, args)]
  | Assignment (_, _)
   |NRFunApp (_, _, _)
   |Break | Continue | Return _ | Skip
   |IfElse (_, _, _)
   |While (_, _)
   |For _ | Block _ | SList _
   |Decl {decl_id= _; _} ->
      []

let rec extract_factors statement_map label =
  let stmt, _ = Map.Poly.find_exn statement_map label in
  let this_stmt =
    List.map (extract_factors_statement stmt) ~f:(fun x -> (label, x))
  in
  Stmt.Fixed.Pattern.fold
    (fun s _ -> s)
    (fun state label -> List.append state (extract_factors statement_map label))
    this_stmt stmt

let factor_rhs (factor : factor) : vexpr Set.Poly.t =
  match factor with
  | TargetTerm e -> Set.Poly.map (expr_var_set e) ~f:fst
  | Reject -> Set.Poly.empty
  | LPFunction (_, es) -> Set.Poly.of_list (List.map es ~f:vexpr_of_expr_exn)

let factor_var_dependencies statement_map (label, factor) =
  let rhs = factor_rhs factor in
  let dep_labels = node_vars_dependencies statement_map rhs label in
  let label_vars l =
    Set.Poly.map
      (stmt_rhs_var_set (fst (Map.Poly.find_exn statement_map l)))
      ~f:fst
  in
  let dep_vars = union_map dep_labels ~f:label_vars in
  Set.Poly.union dep_vars rhs

(* So far just extracts factors in log_prob, finds their dependencies *)
let prog_factor_graph prog =
  let statement_map = log_prob_build_dep_info_map prog in
  let factors = extract_factors statement_map 1 in
  List.map factors ~f:(fun (fac, l) ->
      (fac, l, factor_var_dependencies statement_map (fac, l)) )
