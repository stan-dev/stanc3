open Std
open Std.Sexp_conv
open Middle
open Dataflow_types
open Dataflow_utils
open Mir_utils
open Dependence_analysis

type factor =
  | TargetTerm of Expr.Typed.t
  | Reject
  | LPFunction of (string * Expr.Typed.t list)
[@@deriving sexp_of]

module FactorMap = Map.Make (struct
  type t = factor * label

  let compare = Stdlib.compare
end)

type factor_graph =
  { factor_map: string Set.Poly.t FactorMap.t
  ; var_map: (factor * label) Set.Poly.t String.Map.t }

let extract_factors_statement stmt =
  match stmt with
  | Stmt.Pattern.TargetPE e | JacobianPE e ->
      List.map (summation_terms e) ~f:(fun x -> TargetTerm x)
  | NRFunApp (CompilerInternal (FnReject | FnFatalError), _) -> [Reject]
  | NRFunApp ((UserDefined (s, FnTarget) | StanLib (s, FnTarget)), args) ->
      [LPFunction (s, args)]
  | Assignment (_, _, _)
   |NRFunApp (_, _)
   |Break | Continue | Return _ | Skip
   |IfElse (_, _, _)
   |While (_, _)
   |For _ | Profile _ | Block _ | SList _
   |Decl {decl_id= _; _} ->
      []

let rec extract_factors statement_map label =
  let stmt, _ = LabelMap.find label statement_map in
  let this_stmt =
    List.map (extract_factors_statement stmt) ~f:(fun x -> (label, x)) in
  Stmt.Pattern.fold Fun.const
    (fun state label -> List.append state (extract_factors statement_map label))
    this_stmt stmt

let factor_rhs (term : factor) : string Set.Poly.t =
  match term with
  | TargetTerm e -> Set.Poly.map (expr_var_set e) ~f:fst
  | Reject -> Set.Poly.empty
  | LPFunction (_, es) -> Set.Poly.of_list (List.map es ~f:var_name_of_expr_exn)

let factor_var_dependencies statement_map blockers (label, factor) =
  let term_read_names = factor_rhs factor in
  let dependency_labels =
    node_vars_dependencies statement_map ~blockers term_read_names label in
  let dependency_read_names =
    read_variables_at statement_map dependency_labels in
  (* A direct input such as mu needs no further traversal. Preserve the term's
     own read even when the returned dependency-label set is empty. *)
  Set.Poly.union dependency_read_names term_read_names

let build_adjacency_maps (factors : (label * factor * string Set.Poly.t) list) :
    factor_graph =
  let factor_map =
    List.fold_left
      ~f:(merge_set_maps (module FactorMap))
      ~init:FactorMap.empty
      (List.map
         ~f:(fun (l, fac, vars) -> FactorMap.singleton (fac, l) vars)
         factors) in
  let var_map =
    List.fold_left
      ~f:(merge_set_maps (module String.Map))
      ~init:String.Map.empty
      (List.concat_map factors ~f:(fun (l, fac, vars) ->
           List.map
             ~f:(fun v -> String.Map.singleton v (Set.Poly.singleton (fac, l)))
             (Set.Poly.to_list vars))) in
  {factor_map; var_map}

let fg_remove_fac (fac : factor * label) (fg : factor_graph) : factor_graph =
  let factor_map = FactorMap.remove fac fg.factor_map in
  {fg with factor_map}

let fg_remove_var (var : string) (fg : factor_graph) : factor_graph =
  let factor_map =
    FactorMap.map fg.factor_map ~f:(fun vars -> Set.Poly.remove var vars) in
  let var_map = String.Map.remove var fg.var_map in
  {factor_map; var_map}

let remove_touching vars fg =
  let facs =
    Set.Poly.union_map vars ~f:(fun v ->
        Option.value ~default:Set.Poly.empty (String.Map.find_opt v fg.var_map))
  in
  let without_vars = Set.Poly.fold ~f:fg_remove_var ~init:fg vars in
  let without_facs = Set.Poly.fold ~f:fg_remove_fac ~init:without_vars facs in
  without_facs

let prog_factor_graph ?(exclude_data_facs : bool = false) prog : factor_graph =
  let statement_map = log_prob_build_dep_info_map prog in
  let factors = extract_factors statement_map 1 in
  let data_vars = data_set prog in
  let vars =
    Set.Poly.union data_vars
      (parameter_names_set ~include_transformed:false prog) in
  let factor_list =
    List.map factors ~f:(fun (l, fac) ->
        ( l
        , fac
        , Set.Poly.inter vars
            (factor_var_dependencies statement_map vars (l, fac)) )) in
  let fg = build_adjacency_maps factor_list in
  if exclude_data_facs then remove_touching data_vars fg else fg

let fg_reaches (starts : string Set.Poly.t) (goals : string Set.Poly.t)
    (fg : factor_graph) : bool =
  let vneighbors v =
    let factors = String.Map.find v fg.var_map in
    Set.Poly.union_map factors ~f:(fun f -> FactorMap.find f fg.factor_map)
  in
  let rec step (frontier : string List.t) (visited : string Set.Poly.t) =
    match frontier with
    | next :: frontier' ->
        if Set.Poly.mem next visited then step frontier' visited
        else
          let visited' = Set.Poly.add next visited in
          let expansion = vneighbors next in
          if not (Set.Poly.is_empty (Set.Poly.inter expansion goals)) then true
          else
            step (List.append frontier' (Set.Poly.to_list expansion)) visited'
    | [] -> false in
  step (Set.Poly.to_list starts) Set.Poly.empty

let fg_factor_reaches (start : factor * label) (goals : string Set.Poly.t)
    (fg : factor_graph) : bool =
  let var_starts = FactorMap.find start fg.factor_map in
  fg_reaches var_starts goals fg

let fg_factor_is_prior (var : string) (fac : factor * label)
    (data : string Set.Poly.t) (fg : factor_graph) : bool =
  let fg' = fg_remove_var var fg in
  not (fg_factor_reaches fac data fg')

let fg_var_priors (var : string) (data : string Set.Poly.t) (fg : factor_graph)
    : (factor * label) Set.Poly.t option =
  match String.Map.find_opt var fg.var_map with
  | Some factors ->
      Some
        (Set.Poly.filter factors ~f:(fun fac ->
             fg_factor_is_prior var fac data fg))
  | None -> None

let list_priors ?factor_graph:(fg_opt = None) (mir : Program.Typed.t) :
    ((factor * label) Set.Poly.t option * Location_span.t) String.Map.t =
  let fg = Option.value ~default:(prog_factor_graph mir) fg_opt in
  let params =
    Set.Poly.map ~f:(fun (v, _, loc) -> (v, loc)) (parameter_set mir) in
  let data = data_set mir in
  let likely_sizes = Set.Poly.diff data (data_set ~exclude_ints:true mir) in
  let fg' = Set.Poly.fold ~init:fg ~f:fg_remove_var likely_sizes in
  Set.Poly.fold params ~init:String.Map.empty ~f:(fun (p, loc) m ->
      String.Map.add m ~key:p ~data:(fg_var_priors p data fg', loc))

let string_of_factor (factor : factor) : string =
  match factor with
  | TargetTerm e -> Fmt.str "\"%a\"" Expr.Typed.pp e
  | Reject -> "reject"
  | LPFunction (s, _) -> s

let factor_graph_to_dot (fg : factor_graph) : string =
  let factors = FactorMap.to_list fg.factor_map in
  let names =
    List.map
      ~f:(fun ((f, _), ps) -> (string_of_factor f, Set.Poly.to_list ps))
      factors in
  let factor_names, param_name_lists = List.split names in
  let factor_strings =
    List.map factor_names ~f:(fun n ->
        String.concat ~sep:"" [n; " [shape=box]"]) in
  let param_strings =
    List.sort_uniq ~cmp:String.compare (List.concat param_name_lists) in
  let edge_strings =
    List.concat_map
      ~f:(fun (f, ps) ->
        List.map ~f:(fun p -> String.concat ~sep:"" [f; " -- "; p]) ps)
      names in
  [["graph {"]; factor_strings; param_strings; edge_strings; ["}"]]
  |> List.concat |> String.concat ~sep:"\n"
