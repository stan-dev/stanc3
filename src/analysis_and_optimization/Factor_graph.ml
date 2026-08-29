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

module FactorMap = struct
  include Map.Make (struct
    type t = factor * label

    let compare = Stdlib.compare
  end)

  let sexp_of_t f t =
    sexp_of_list
      (sexp_of_pair (sexp_of_pair sexp_of_factor sexp_of_label) f)
      (to_list t)
end

module VExprMap = struct
  include Map.Make (struct
    type t = vexpr

    let compare (VVar s1) (VVar s2) = String.compare s1 s2
  end)

  let sexp_of_t f t = sexp_of_list (sexp_of_pair sexp_of_vexpr f) (to_list t)
end

type factor_graph =
  { factor_map: vexpr Set.Poly.t FactorMap.t
  ; var_map: (factor * label) Set.Poly.t VExprMap.t }
[@@deriving sexp_of]

let extract_factors_statement stmt =
  match stmt with
  | Stmt.Pattern.TargetPE e | JacobianPE e ->
      List.map (summation_terms e) ~f:(fun x -> TargetTerm x)
  | NRFunApp (CompilerInternal (FnReject | FnFatalError), _) -> [Reject]
  | NRFunApp ((UserDefined (s, FnTarget) | StanLib (s, FnTarget, _)), args) ->
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

let factor_rhs (factor : factor) : vexpr Set.Poly.t =
  match factor with
  | TargetTerm e -> Set.Poly.map (expr_var_set e) ~f:fst
  | Reject -> Set.Poly.empty
  | LPFunction (_, es) -> Set.Poly.of_list (List.map es ~f:vexpr_of_expr_exn)

let factor_var_dependencies statement_map blockers (label, factor) =
  let rhs = factor_rhs factor in
  let dep_labels = node_vars_dependencies statement_map ~blockers rhs label in
  let label_vars l = stmt_rhs_names_set (fst (LabelMap.find l statement_map)) in
  let dep_vars = Set.Poly.union_map dep_labels ~f:label_vars in
  Set.Poly.union dep_vars rhs

(** Helper function to generate the factor graph adjacency map representation
    from a factor-adjacency list *)
let build_adjacency_maps (factors : (label * factor * vexpr Set.Poly.t) list) :
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
      ~f:(merge_set_maps (module VExprMap))
      ~init:VExprMap.empty
      (List.concat_map factors ~f:(fun (l, fac, vars) ->
           List.map
             ~f:(fun v -> VExprMap.singleton v (Set.Poly.singleton (fac, l)))
             (Set.Poly.to_list vars))) in
  {factor_map; var_map}

let fg_remove_fac (fac : factor * cf_state) (fg : factor_graph) : factor_graph =
  let factor_map = FactorMap.remove fac fg.factor_map in
  {fg with factor_map}

let fg_remove_var (var : vexpr) (fg : factor_graph) : factor_graph =
  let factor_map =
    FactorMap.map fg.factor_map ~f:(fun vars -> Set.Poly.remove var vars) in
  let var_map = VExprMap.remove var fg.var_map in
  {factor_map; var_map}

let remove_touching vars fg =
  let facs =
    Set.Poly.union_map vars ~f:(fun v ->
        Option.value ~default:Set.Poly.empty (VExprMap.find_opt v fg.var_map))
  in
  let without_vars = Set.Poly.fold ~f:fg_remove_var ~init:fg vars in
  let without_facs = Set.Poly.fold ~f:fg_remove_fac ~init:without_vars facs in
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
         (parameter_names_set ~include_transformed:false prog)) in
  let factor_list =
    List.map factors ~f:(fun (l, fac) ->
        ( l
        , fac
        , Set.Poly.inter vars
            (factor_var_dependencies statement_map vars (l, fac)) )) in
  let fg = build_adjacency_maps factor_list in
  if exclude_data_facs then
    remove_touching (Set.Poly.map ~f:(fun v -> VVar v) data_vars) fg
  else fg

(** BFS on 'fg' with initial frontier 'starts' and terminating at any element of
    'goals' *)
let fg_reaches (starts : vexpr Set.Poly.t) (goals : vexpr Set.Poly.t)
    (fg : factor_graph) : bool =
  let vneighbors v =
    let factors = VExprMap.find v fg.var_map in
    Set.Poly.union_map factors ~f:(fun f -> FactorMap.find f fg.factor_map)
  in
  let rec step (frontier : vexpr List.t) (visited : vexpr Set.Poly.t) =
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

let fg_factor_reaches (start : factor * label) (goals : vexpr Set.Poly.t)
    (fg : factor_graph) : bool =
  let var_starts = FactorMap.find start fg.factor_map in
  fg_reaches var_starts goals fg

let fg_factor_is_prior (var : vexpr) (fac : factor * label)
    (data : vexpr Set.Poly.t) (fg : factor_graph) : bool =
  (* build G'=G\V *)
  let fg' = fg_remove_var var fg in
  (* Check if the data is now unreachable *)
  not (fg_factor_reaches fac data fg')

(** Check if a variable is the 'subject' of a target term factor. For
    distribution functions (_lpdf/_lpmf and their unnormalized variants), only
    the first argument is the distribution subject. A factor should only be
    considered a potential prior for a variable if the variable appears as the
    subject, not merely as a distribution argument. For non-distribution
    factors, conservatively return true. *)
let is_factor_subject (var : vexpr) (factor : factor) : bool =
  match factor with
  | TargetTerm
      { pattern=
          FunApp
            ( ( StanLib (_, (FnLpdf _ | FnLpmf _), _)
              | UserDefined (_, (FnLpdf _ | FnLpmf _)) )
            , first_arg :: _ )
      ; _ } ->
      Set.Poly.mem var (Set.Poly.map (expr_var_set first_arg) ~f:fst)
  | _ -> true

(** Priors of V are neighbors of V which have no connection to any data except
    though V So for graph G and each parameter V: G' = G\V; For each neighbor F:
    Use BFS starting from F in G' and search for any data, if there is none, F
    is a prior. Additionally, F is only a prior for V if V is the subject (first
    argument) of a distribution factor, not merely an argument. *)
let fg_var_priors (var : vexpr) (data : vexpr Set.Poly.t) (fg : factor_graph) :
    (factor * label) Set.Poly.t option =
  match VExprMap.find_opt var fg.var_map with
  | Some factors ->
      Some
        (Set.Poly.filter factors ~f:(fun ((fac, _) as fl) ->
             is_factor_subject var fac && fg_factor_is_prior var fl data fg))
  | None -> None

let list_priors ?factor_graph:(fg_opt = None) (mir : Program.Typed.t) :
    ((factor * label) Set.Poly.t option * Location_span.t) VExprMap.t =
  let fg = Option.value ~default:(prog_factor_graph mir) fg_opt in
  let params =
    Set.Poly.map ~f:(fun (v, _, loc) -> (VVar v, loc)) (parameter_set mir) in
  let data = Set.Poly.map ~f:(fun v -> VVar v) (data_set mir) in
  let likely_sizes =
    Set.Poly.diff data
      (Set.Poly.map ~f:(fun v -> VVar v) (data_set ~exclude_ints:true mir))
  in
  let fg' = Set.Poly.fold ~init:fg ~f:fg_remove_var likely_sizes in
  (* for each param, apply fg_var_priors and collect results in a map *)
  Set.Poly.fold params ~init:VExprMap.empty ~f:(fun (p, loc) m ->
      VExprMap.add m ~key:p ~data:(fg_var_priors p data fg', loc))

let string_of_factor (factor : factor) : string =
  match factor with
  | TargetTerm e -> Fmt.str "\"%a\"" Expr.Typed.pp e
  | Reject -> "reject"
  | LPFunction (s, _) -> s

let string_of_vexpr (vexpr : vexpr) : string = match vexpr with VVar s -> s

(** Utility to print a factor graph to the Graphviz dot language for
    visualization *)
let factor_graph_to_dot (fg : factor_graph) : string =
  let factors = FactorMap.to_list fg.factor_map in
  let names =
    List.map
      ~f:(fun ((f, _), ps) ->
        (string_of_factor f, List.map ~f:string_of_vexpr (Set.Poly.to_list ps)))
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
