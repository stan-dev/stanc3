open Core_kernel
open Middle
open Dataflow_types
open Mir_utils
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

(***********************************)
(* Dependency analysis & interface *)
(***********************************)

type node_dep_info =
  { predecessors: label Set.Poly.t
  ; parents: label Set.Poly.t
  ; reaching_defn_entry: reaching_defn Set.Poly.t
  ; reaching_defn_exit: reaching_defn Set.Poly.t }

(**
   Find all of the reaching definitions of a variable in an RD set
*)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : vexpr) :
    label Set.Poly.t =
  Set.Poly.map (Set.Poly.filter rds ~f:(fun (var', _) -> var' = var)) ~f:snd

let node_immediate_dependencies
    (statement_map :
      (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t)
    (label : label) : label Set.Poly.t =
  let stmt, info = Map.Poly.find_exn statement_map label in
  let rhs_set = stmt_rhs_var_set stmt in
  let rhs_deps =
    union_map rhs_set ~f:(reaching_defn_lookup info.reaching_defn_entry) in
  Set.Poly.union info.parents rhs_deps

(*
   This is doing an explicit graph traversal with edges defined by
   node_immediate_dependencies.
*)
let rec node_dependencies_rec
    (statement_map :
      (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t)
    (visited : label Set.Poly.t) (label : label) : label Set.Poly.t =
  if Set.Poly.mem visited label then visited
  else
    let visited' = Set.Poly.add visited label in
    let deps = node_immediate_dependencies statement_map label in
    Set.Poly.fold deps ~init:visited' ~f:(node_dependencies_rec statement_map)

let node_dependencies
    (statement_map :
      (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t)
    (label : label) : label Set.Poly.t =
  node_dependencies_rec statement_map Set.Poly.empty label

let node_vars_dependencies
    (statement_map :
      (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t)
    (vars : vexpr Set.Poly.t) (label : label) : label Set.Poly.t =
  let _, info = Map.Poly.find_exn statement_map label in
  let var_deps =
    union_map vars ~f:(reaching_defn_lookup info.reaching_defn_entry) in
  Set.Poly.fold
    (Set.union info.parents var_deps)
    ~init:Set.Poly.empty
    ~f:(node_dependencies_rec statement_map)

(*
   The strategy here is to write an update function on the whole dependency graph in terms
   of node_immediate_dependencies, and then to find a fixed-point. Since it's updating the
   dependencies for the whole graph at a time, it should be more efficient than doing a
   graph traversal for each node.
*)
let all_node_dependencies
    (statement_map :
      (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t)
    : (label, label Set.Poly.t) Map.Poly.t =
  let immediate_map =
    Map.mapi statement_map ~f:(fun ~key:label ~data:_ ->
        node_immediate_dependencies statement_map label) in
  let step_node label m =
    let immediate = Map.find_exn immediate_map label in
    let updated =
      Set.union
        (union_map immediate ~f:(fun label -> Map.find_exn m label))
        immediate in
    Set.remove updated label in
  let step_map m =
    Map.mapi m ~f:(fun ~key:label ~data:_ -> step_node label m) in
  let map_equal = Map.Poly.equal Set.Poly.equal in
  let rec step_until_fixed m =
    let m' = step_map m in
    if map_equal m m' then m else step_until_fixed m' in
  step_until_fixed immediate_map

let reaching_defns
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    (label, reaching_defn Set.Poly.t entry_exit) Map.Poly.t =
  Map.Poly.mapi statement_map ~f:(fun ~key:_ ~data:_ ->
      (* TODO: figure out how to call RDs *)
      {entry= Set.Poly.empty; exit= Set.Poly.empty})

let build_dep_info_map
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t =
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = reaching_defns statement_map in
  Map.Poly.mapi statement_map ~f:(fun ~key:label ~data:(stmt, _) ->
      let rds = Map.find_exn rd_map label in
      ( stmt
      , { predecessors= Map.find_exn preds label
        ; parents= Map.find_exn parents label
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit } ))

let mir_reaching_definitions (mir : typed_prog) (stmt : stmt_loc) :
    (label, reaching_defn Set.Poly.t entry_exit) Map.Poly.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt stmt in
  let (module Flowgraph) = flowgraph in
  let rd_map =
    reaching_definitions_mfp mir (module Flowgraph) flowgraph_to_mir in
  let to_rd_set set =
    Set.Poly.map set ~f:(fun (s, label_opt) ->
        (VVar s, Option.value label_opt ~default:0)) in
  Map.Poly.map rd_map ~f:(fun {entry; exit} ->
      {entry= to_rd_set entry; exit= to_rd_set exit})

let log_prob_build_dep_info_map (mir : Middle.typed_prog) :
    (label, (expr_typed_located, label) statement * node_dep_info) Map.Poly.t =
  let log_prob_stmt = {smeta= Middle.no_span; stmt= SList mir.log_prob} in
  let statement_map =
    build_statement_map (fun s -> s.stmt) (fun s -> s.smeta) log_prob_stmt
  in
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = mir_reaching_definitions mir log_prob_stmt in
  Map.Poly.mapi statement_map ~f:(fun ~key:label ~data:(stmt, _) ->
      let rds = Map.find_exn rd_map label in
      ( stmt
      , { predecessors= Map.find_exn preds label
        ; parents= Map.find_exn parents label
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit } ))

let stmt_map_dependency_graph
    (statement_map :
      (label, (expr_typed_located, label) statement * 'm) Map.Poly.t) :
    (label, label Set.Poly.t) Map.Poly.t =
  let dep_info_map = build_dep_info_map statement_map in
  all_node_dependencies dep_info_map

let log_prob_dependency_graph (mir : Middle.typed_prog) :
    (label, label Set.Poly.t) Map.Poly.t =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map
