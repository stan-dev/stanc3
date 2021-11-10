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
  { factor_map: (factor * label, vexpr Set.Poly.t) Map.Poly.t
  ; var_map: (vexpr, (factor * label) Set.Poly.t) Map.Poly.t }
[@@deriving sexp, compare]

let extract_factors_statement stmt =
  match stmt with
  | Stmt.Fixed.Pattern.TargetPE e ->
      List.map (summation_terms e) ~f:(fun x -> TargetTerm x)
  | NRFunApp (CompilerInternal FnReject, _) -> [Reject]
  | NRFunApp ((UserDefined (s, FnTarget) | StanLib (s, FnTarget, _)), args) ->
      [LPFunction (s, args)]
  | Assignment (_, _)
   |NRFunApp (_, _)
   |Break | Continue | Return _ | Skip
   |IfElse (_, _, _)
   |While (_, _)
   |For _ | Profile _ | Block _ | SList _
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

let factor_var_dependencies statement_map blockers (label, factor) =
  let rhs = factor_rhs factor in
  let dep_labels = node_vars_dependencies statement_map ~blockers rhs label in
  let label_vars l =
    Set.Poly.map
      (stmt_rhs_var_set (fst (Map.Poly.find_exn statement_map l)))
      ~f:fst
  in
  let dep_vars = union_map dep_labels ~f:label_vars in
  Set.Poly.union dep_vars rhs

(** Helper function to generate the factor graph adjacency map representation
   from a factor-adjacency list *)
let build_adjacency_maps (factors : (label * factor * vexpr Set.Poly.t) List.t)
    : factor_graph =
  let factor_map =
    List.fold ~f:merge_set_maps ~init:Map.Poly.empty
      (List.map
         ~f:(fun (l, fac, vars) -> Map.Poly.singleton (fac, l) vars)
         factors)
  in
  let var_map =
    List.fold ~f:merge_set_maps ~init:Map.Poly.empty
      (List.concat_map factors ~f:(fun (l, fac, vars) ->
           List.map
             ~f:(fun v -> Map.Poly.singleton v (Set.Poly.singleton (fac, l)))
             (Set.Poly.to_list vars) ))
  in
  {factor_map; var_map}

let fg_remove_fac (fac : factor * cf_state) (fg : factor_graph) : factor_graph
    =
  let factor_map = Map.Poly.remove fg.factor_map fac in
  {fg with factor_map}

let fg_remove_var (var : vexpr) (fg : factor_graph) : factor_graph =
  let factor_map =
    Map.Poly.map fg.factor_map ~f:(fun vars -> Set.Poly.remove vars var)
  in
  let var_map = Map.Poly.remove fg.var_map var in
  {factor_map; var_map}

let remove_touching vars fg =
  let facs =
    union_map vars ~f:(fun v ->
        Option.value ~default:Set.Poly.empty (Map.Poly.find fg.var_map v) )
  in
  let without_vars =
    Set.fold ~f:(fun g v -> fg_remove_var v g) ~init:fg vars
  in
  let without_facs =
    Set.fold ~f:(fun g f -> fg_remove_fac f g) ~init:without_vars facs
  in
  without_facs

(** Build a factor graph from prog.log_prob using dependency analysis *)
let prog_factor_graph ?(exclude_data_facs : bool = false) prog : factor_graph =
  let statement_map = log_prob_build_dep_info_map prog in
  let factors = extract_factors statement_map 1 in
  let data_vars = data_set prog in
  let vars =
    Set.Poly.map
      ~f:(fun v -> VVar v)
      (Set.Poly.union data_vars
         (parameter_names_set ~include_transformed:false prog))
  in
  let factor_list =
    List.map factors ~f:(fun (l, fac) ->
        ( l
        , fac
        , Set.Poly.inter vars
            (factor_var_dependencies statement_map vars (l, fac)) ) )
  in
  let fg = build_adjacency_maps factor_list in
  if exclude_data_facs then
    remove_touching (Set.Poly.map ~f:(fun v -> VVar v) data_vars) fg
  else fg

(** BFS on 'fg' with initial frontier 'starts' and terminating at any
   element of 'goals' *)
let fg_reaches (starts : vexpr Set.Poly.t) (goals : vexpr Set.Poly.t)
    (fg : factor_graph) : bool =
  let vneighbors v =
    let factors = Map.Poly.find_exn fg.var_map v in
    union_map factors ~f:(Map.Poly.find_exn fg.factor_map)
  in
  let rec step (frontier : vexpr List.t) (visited : vexpr Set.Poly.t) =
    match frontier with
    | next :: frontier' ->
        if Set.mem visited next then step frontier' visited
        else
          let visited' = Set.Poly.add visited next in
          let expansion = vneighbors next in
          if not (Set.Poly.is_empty (Set.Poly.inter expansion goals)) then true
          else
            step (List.append frontier' (Set.Poly.to_list expansion)) visited'
    | [] -> false
  in
  step (Set.Poly.to_list starts) Set.Poly.empty

let fg_factor_reaches (start : factor * label) (goals : vexpr Set.Poly.t)
    (fg : factor_graph) : bool =
  let var_starts = Map.Poly.find_exn fg.factor_map start in
  fg_reaches var_starts goals fg

let fg_factor_is_prior (var : vexpr) (fac : factor * label)
    (data : vexpr Set.Poly.t) (fg : factor_graph) : bool =
  (* build G'=G\V *)
  let fg' = fg_remove_var var fg in
  (* Check if the data is now unreachable *)
  not (fg_factor_reaches fac data fg')

(** Priors of V are neighbors of V which have no connection to any data except though V
   So for graph G and each parameter V:
     G' = G\V;
     For each neighbor F:
       Use BFS starting from F in G' and search for any data,
           if there is none, F is a prior
*)
let fg_var_priors (var : vexpr) (data : vexpr Set.Poly.t) (fg : factor_graph) :
    (factor * label) Set.Poly.t option =
  match Map.Poly.find fg.var_map var with
  | Some factors ->
      Some
        (Set.Poly.filter factors ~f:(fun fac ->
             fg_factor_is_prior var fac data fg ))
  | None -> None

let list_priors ?factor_graph:(fg_opt = None) (mir : Program.Typed.t) :
    (vexpr, (factor * label) Set.Poly.t option) Map.Poly.t =
  let fg = Option.value ~default:(prog_factor_graph mir) fg_opt in
  let params = Set.Poly.map ~f:(fun v -> VVar v) (parameter_names_set mir) in
  let data = Set.Poly.map ~f:(fun v -> VVar v) (data_set mir) in
  let likely_sizes =
    Set.Poly.diff data
      (Set.Poly.map ~f:(fun v -> VVar v) (data_set ~exclude_ints:true mir))
  in
  let fg' =
    Set.Poly.fold ~init:fg
      ~f:(fun fg likely_size -> fg_remove_var likely_size fg)
      likely_sizes
  in
  (* for each param, apply fg_var_priors and collect results in a map*)
  generate_map params ~f:(fun p -> fg_var_priors p data fg')

let string_of_factor (factor : factor) : string =
  match factor with
  | TargetTerm e -> Fmt.strf "\"%a\"" Expr.Typed.pp e
  | Reject -> "reject"
  | LPFunction (s, _) -> s

let string_of_vexpr (vexpr : vexpr) : string = match vexpr with VVar s -> s

(** Utility to print a factor graph to the Graphviz dot language for
   visualization *)
let factor_graph_to_dot (fg : factor_graph) : string =
  let factors = Map.Poly.to_alist ~key_order:`Decreasing fg.factor_map in
  let names =
    List.map
      ~f:(fun ((f, _), ps) ->
        (string_of_factor f, List.map ~f:string_of_vexpr (Set.Poly.to_list ps))
        )
      factors
  in
  let factor_names, param_name_lists = List.unzip names in
  let factor_strings =
    List.map factor_names ~f:(fun n -> String.concat [n; " [shape=box]"])
  in
  let param_strings =
    List.dedup_and_sort ~compare:String.compare (List.concat param_name_lists)
  in
  let edge_strings =
    List.concat_map
      ~f:(fun (f, ps) -> List.map ~f:(fun p -> String.concat [f; " -- "; p]) ps)
      names
  in
  [["graph {"]; factor_strings; param_strings; edge_strings; ["}"]]
  |> List.concat |> String.concat ~sep:"\n"
