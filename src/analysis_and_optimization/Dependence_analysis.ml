open Std
open Middle
open Dataflow_types
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

type node_dep_info =
  { predecessors: label Set.Poly.t
  ; parents: label Set.Poly.t
  ; reaching_defn_entry: reaching_defn Set.Poly.t
  ; reaching_defn_exit: reaching_defn Set.Poly.t
  ; loops: Dependence.loop list
  ; accesses: Accesses.index Accesses.t
  ; meta: Location_span.t }

type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

type dependency_graph = label Set.Poly.t LabelMap.t

(* Label-only queries use the root as a marker for external definitions. The
   root exists in every map; read records retain the external variable name. *)
let root_label : label = 1

let mir_reaching_definitions (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    reaching_defn Set.Poly.t entry_exit LabelMap.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt stmt in
  let (module Flowgraph) = flowgraph in
  reaching_definitions_mfp mir (module Flowgraph) flowgraph_to_mir

let enclosing_loops_by_statement statement_map =
  let rec collect statement_id enclosing_loops loops_map =
    let pattern, _ = LabelMap.find statement_id statement_map in
    let child_enclosing_loops =
      (* A loop's bounds or condition belong to the loop statement. Only its
         descendants execute inside the loop. *)
      match pattern with
      | Stmt.Pattern.For {loopvar; _} ->
          enclosing_loops @ [{Dependence.id= statement_id; var= Some loopvar}]
      | While _ -> enclosing_loops @ [{Dependence.id= statement_id; var= None}]
      | _ -> enclosing_loops in
    let loops_map =
      LabelMap.add loops_map ~key:statement_id ~data:enclosing_loops in
    Stmt.Pattern.fold
      (fun loops_map _ -> loops_map)
      (fun loops_map child_id ->
        collect child_id child_enclosing_loops loops_map)
      loops_map pattern in
  collect root_label [] LabelMap.empty

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    dep_info_map =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let _, predecessor_ids, parent_ids = build_cf_graphs statement_map in
  let reaching_definitions = mir_reaching_definitions mir stmt in
  let enclosing_loops = enclosing_loops_by_statement statement_map in
  let all_stmt_accesses =
    LabelMap.map statement_map ~f:(fun (pattern, _) -> Accesses.of_stmt pattern)
  in
  (* An input written anywhere in the analyzed region can change. For example, a
     write [m = n] prevents classifying index [m] as an invariant symbol. *)
  let written_vars =
    LabelMap.fold all_stmt_accesses ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:stmt_accesses running_written_vars ->
        Set.Poly.union running_written_vars
          (Accesses.written_vars stmt_accesses)) in
  let make_node_info statement_id
      ((pattern : (Expr.Typed.t, label) Stmt.Pattern.t), meta) =
    let definitions = LabelMap.find statement_id reaching_definitions in
    let loops = LabelMap.find statement_id enclosing_loops in
    let loopvars =
      Set.Poly.of_list
        (List.filter_map loops ~f:(fun loop -> loop.Dependence.var)) in
    let raw_accesses = LabelMap.find statement_id all_stmt_accesses in
    let classified_accesses =
      Accesses.classify ~loopvars ~written_vars raw_accesses in
    ( pattern
    , { predecessors= LabelMap.find statement_id predecessor_ids
      ; parents= LabelMap.find statement_id parent_ids
      ; reaching_defn_entry= definitions.entry
      ; reaching_defn_exit= definitions.exit
      ; loops
      ; accesses= classified_accesses
      ; meta } ) in
  LabelMap.mapi statement_map ~f:make_node_info

let log_prob_build_dep_info_map (mir : Program.Typed.t) : dep_info_map =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dep_info_map mir log_prob_stmt

let reaching_defn_lookup (definitions : reaching_defn Set.Poly.t) (var : string)
    : label option Set.Poly.t =
  Set.Poly.map
    (Set.Poly.filter definitions ~f:(fun (defined, _) ->
         String.equal defined var))
    ~f:snd

let flows_into (source : 'index Accesses.access) (sink : 'index Accesses.access)
    : bool =
  String.equal source.var sink.var
  &&
  match (source.kind, sink.kind) with
  | (Write | Increment), Read | Write, Increment -> true
  | Read, _ | Increment, Increment | _, Write -> false

let writes_read_by (statement_map : dep_info_map) ~(sink_id : label)
    (var : string) : label Set.Poly.t =
  let _, sink_info = LabelMap.find sink_id statement_map in
  let sink_accesses = Accesses.of_var var sink_info.accesses in
  let candidate_definitions =
    reaching_defn_lookup sink_info.reaching_defn_entry var in
  let source_may_supply_read source_id =
    let _, source_info = LabelMap.find source_id statement_map in
    let source_accesses = Accesses.of_var var source_info.accesses in
    let source_has_writes =
      not (Set.Poly.is_empty (Accesses.written_vars source_accesses)) in
    let sink_has_reads =
      not (Set.Poly.is_empty (Accesses.read_vars sink_accesses)) in
    (* A for loop defines its loop variable without a write access. A variable
       query can also request a name absent from the sink's reads. Without a
       source write or sink read to compare, keep the candidate selected by
       name. *)
    if (not source_has_writes) || not sink_has_reads then true
    else
      let loops = Dependence.common_loops source_info.loops sink_info.loops in
      (* Statement labels follow source order. Equal labels represent a write
         and read in the same statement, where the read executes first. *)
      let source_first_in_text = source_id < sink_id in
      List.exists source_accesses ~f:(fun source ->
          List.exists sink_accesses ~f:(fun sink ->
              if not (flows_into source sink) then false
              else
                let path_constraints =
                  Dependence.compare_paths ~loops source sink in
                Dependence.may_exist ~source_first_in_text path_constraints))
  in
  Set.Poly.filter_map candidate_definitions ~f:(function
    | None ->
        (* External definitions have no statement access record. Preserve the
           possible source using the root marker in the label-only result. *)
        Some root_label
    | Some source_id ->
        if source_may_supply_read source_id then Some source_id else None)

let read_variables (info : node_dep_info) : string Set.Poly.t =
  Accesses.read_vars info.accesses

let node_immediate_dependencies (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (node_id : label) :
    label Set.Poly.t =
  let _, node_info = LabelMap.find node_id statement_map in
  let read_dependency_ids =
    Set.Poly.union_map
      (Set.Poly.diff (read_variables node_info) blockers)
      ~f:(writes_read_by statement_map ~sink_id:node_id) in
  Set.Poly.union node_info.parents read_dependency_ids

let rec node_dependencies_rec (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (node_id : label)
    (visited_ids : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem node_id visited_ids then visited_ids
  else
    (* The visited set also serves as the result, including the starting
       label. *)
    let visited_with_node_ids = Set.Poly.add node_id visited_ids in
    let dependency_ids =
      node_immediate_dependencies statement_map ~blockers node_id in
    Set.Poly.fold dependency_ids ~init:visited_with_node_ids
      ~f:(node_dependencies_rec statement_map ~blockers)

let node_dependencies (statement_map : dep_info_map) (node_id : label) :
    label Set.Poly.t =
  node_dependencies_rec statement_map node_id Set.Poly.empty

let node_vars_dependencies (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (vars : string Set.Poly.t)
    (node_id : label) : label Set.Poly.t =
  let _, node_info = LabelMap.find node_id statement_map in
  let var_dependency_ids =
    Set.Poly.union_map
      (Set.Poly.diff vars blockers)
      ~f:(writes_read_by statement_map ~sink_id:node_id) in
  Set.Poly.fold
    (Set.Poly.union node_info.parents var_dependency_ids)
    ~init:Set.Poly.empty
    ~f:(node_dependencies_rec statement_map ~blockers)

let all_node_dependencies (statement_map : dep_info_map) : dependency_graph =
  let immediate_map =
    LabelMap.mapi statement_map ~f:(fun label _ ->
        node_immediate_dependencies statement_map label) in
  let step_node label m =
    let immediate = LabelMap.find label immediate_map in
    let updated =
      Set.Poly.union
        (Set.Poly.union_map immediate ~f:(fun label -> LabelMap.find label m))
        immediate in
    Set.Poly.remove label updated in
  let step_map m = LabelMap.mapi m ~f:(fun label _ -> step_node label m) in
  let map_equal = LabelMap.equal ~cmp:Set.Poly.equal in
  let rec step_until_fixed m =
    let m' = step_map m in
    if map_equal m m' then m else step_until_fixed m' in
  step_until_fixed immediate_map

let log_prob_dependency_graph (mir : Program.Typed.t) : dependency_graph =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map

let read_variables_at (statement_map : dep_info_map) (labels : label Set.Poly.t)
    : string Set.Poly.t =
  Set.Poly.union_map labels ~f:(fun label ->
      read_variables (snd (LabelMap.find label statement_map)))
