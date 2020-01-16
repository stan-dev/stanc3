open Core_kernel
open Middle
open Dataflow_types
open Dataflow_utils
open Mir_utils

(*open Dataflow_utils*)
open Dependence_analysis

type factor =
  | TargetTerm of Expr.Typed.t
  | Reject
  | LPFunction of (string * Expr.Typed.t list)
[@@deriving sexp, hash, compare]

type factor_graph =
  { factor_map : (factor * label, vexpr Set.Poly.t) Map.Poly.t
  ; var_map : (vexpr, (factor * label) Set.Poly.t) Map.Poly.t
  }
[@@deriving sexp, compare]

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


let build_adjacency_maps (factors : (label * factor * vexpr Set.Poly.t) List.t) : factor_graph =
  let factor_map =
    List.fold ~f:merge_set_maps ~init:Map.Poly.empty
      (List.map ~f:(fun (l, fac, vars) ->
           Map.Poly.singleton (fac, l) vars) factors)
  in
  let var_map =
    List.fold ~f:merge_set_maps ~init:Map.Poly.empty
      (List.concat_map factors ~f:(fun (l, fac, vars) ->
           List.map
             ~f:(fun v -> Map.Poly.singleton v (Set.Poly.singleton (fac, l)))
             (Set.Poly.to_list vars)))
  in { factor_map; var_map }

(* So far just extracts factors in log_prob, finds their dependencies *)
let prog_factor_graph prog : factor_graph =
  let statement_map = log_prob_build_dep_info_map prog in
  let factors = extract_factors statement_map 1 in
  let vars = Set.Poly.map
      ~f:(fun v -> VVar v)
      (Set.Poly.union (parameter_set prog) (data_set prog))
  in
  let factor_list =
    List.map factors ~f:(fun (l, fac) ->
        (l, fac, Set.Poly.inter vars (factor_var_dependencies statement_map (l, fac))) )
  in build_adjacency_maps factor_list

let fg_remove_var (var : vexpr) (fg : factor_graph) : factor_graph =
  let factor_map =
    Map.Poly.map fg.factor_map ~f:(fun vars -> Set.Poly.remove vars var)
  in
  let var_map =
    Map.Poly.remove fg.var_map var
  in
  {factor_map; var_map}

let fg_reaches (starts : vexpr Set.Poly.t) (goals : vexpr Set.Poly.t) (fg : factor_graph) : bool =
  let vneighbors v =
    let factors = Map.Poly.find_exn fg.var_map v in
    union_map factors ~f:(Map.Poly.find_exn fg.factor_map)
  in
  let rec step (frontier : vexpr List.t) (visited : vexpr Set.Poly.t) =
    match frontier with
    | (next::frontier') ->
      (if Set.mem visited next then
        step frontier' visited
      else
        let visited' = Set.Poly.add visited next in
        let expansion = vneighbors next in
        if not (Set.Poly.is_empty (Set.Poly.inter expansion goals)) then
          true
        else
          step (List.append frontier' (Set.Poly.to_list expansion)) visited')
    | [] -> false
  in step (Set.Poly.to_list starts) Set.Poly.empty

let fg_factor_reaches (start : factor * label) (goals : vexpr Set.Poly.t) (fg : factor_graph) : bool =
  let var_starts = Map.Poly.find_exn fg.factor_map start in
  fg_reaches var_starts goals fg

let fg_factor_is_prior (var : vexpr) (fac : factor * label) (data : vexpr Set.Poly.t) (fg : factor_graph) : bool =
  let fg' = fg_remove_var var fg in
  not (fg_factor_reaches fac data fg')

let fg_var_priors (var : vexpr) (data : vexpr Set.Poly.t) (fg : factor_graph) : (factor * label) Set.Poly.t option =
  match Map.Poly.find fg.var_map var with
  | Some factors -> Some
    (Set.Poly.filter factors ~f:(fun fac -> fg_factor_is_prior var fac data fg))
  | None -> None

let list_priors (mir : Program.Typed.t) : (vexpr, (factor * label) Set.Poly.t option) Map.Poly.t =
  let fg = prog_factor_graph mir in
  let params = Set.Poly.map ~f:(fun v -> VVar v) (parameter_set mir) in
  let data = Set.Poly.map ~f:(fun v -> VVar v) (data_set mir) in
  generate_map params ~f:(fun p -> fg_var_priors p data fg)

(*
 BFS:
Set var -> FactorGraph -> var -> Bool
 Remove var:
   var -> FactorGraph -> FactorGraph
 Get adjacent factors:
   var -> FactorGraph -> Set factor
*)

(* Pull out:
  What are the parameter dependencies of this label? 

Final data structure? Tight:
   Map factor (Set var)
Or swap:
   Map var (Set factor)

   In a bipartite graph, you can have an adjacency list with only one part without redundancy

   Going to watch to go from var -> factor as well though, should maybe cache that at the cost of redundancy.

   Need to write BFS on the data structure.

   To find prior for V, remove V and run BFS on each neighbor F of V, then F is a prior if no data is encountered.
*)



