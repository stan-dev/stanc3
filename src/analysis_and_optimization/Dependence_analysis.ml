open Core_kernel
open Core_kernel.Poly
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
  ; reaching_defn_exit: reaching_defn Set.Poly.t
  ; meta: Location_span.t }

(**
   Find all of the reaching definitions of a variable in an RD set
*)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : vexpr) :
    label Set.Poly.t =
  Set.Poly.map (Set.Poly.filter rds ~f:(fun (var', _) -> var' = var)) ~f:snd

let node_immediate_dependencies
    (statement_map :
      ( label
      , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * node_dep_info )
      Map.Poly.t ) ?(blockers : vexpr Set.Poly.t = Set.Poly.empty)
    (label : label) : label Set.Poly.t =
  let stmt, info = Map.Poly.find_exn statement_map label in
  let rhs_set = Set.Poly.map (stmt_rhs_var_set stmt) ~f:fst in
  let rhs_deps =
    union_map
      (Set.Poly.diff rhs_set blockers)
      ~f:(reaching_defn_lookup info.reaching_defn_entry) in
  Set.Poly.union info.parents rhs_deps

(*
   This is doing an explicit graph traversal with edges defined by
   node_immediate_dependencies.
*)
let rec node_dependencies_rec
    (statement_map :
      ( label
      , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * node_dep_info )
      Map.Poly.t ) ?(blockers : vexpr Set.Poly.t = Set.Poly.empty)
    (visited : label Set.Poly.t) (label : label) : label Set.Poly.t =
  if Set.Poly.mem visited label then visited
  else
    let visited' = Set.Poly.add visited label in
    let deps = node_immediate_dependencies statement_map ~blockers label in
    Set.Poly.fold deps ~init:visited' ~f:(node_dependencies_rec statement_map)

let node_dependencies
    (statement_map :
      ( label
      , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * node_dep_info )
      Map.Poly.t ) (label : label) : label Set.Poly.t =
  node_dependencies_rec statement_map Set.Poly.empty label

let node_vars_dependencies
    (statement_map :
      ( label
      , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * node_dep_info )
      Map.Poly.t ) ?(blockers : vexpr Set.Poly.t = Set.Poly.empty)
    (vars : vexpr Set.Poly.t) (label : label) : label Set.Poly.t =
  let _, info = Map.Poly.find_exn statement_map label in
  let var_deps =
    union_map
      (Set.Poly.diff vars blockers)
      ~f:(reaching_defn_lookup info.reaching_defn_entry) in
  Set.Poly.fold
    (Set.union info.parents var_deps)
    ~init:Set.Poly.empty
    ~f:(node_dependencies_rec statement_map ~blockers)

(*
   The strategy here is to write an update function on the whole dependency graph in terms
   of node_immediate_dependencies, and then to find a fixed-point. Since it's updating the
   dependencies for the whole graph at a time, it should be more efficient than doing a
   graph traversal for each node.
*)
let all_node_dependencies
    (statement_map :
      ( label
      , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * node_dep_info )
      Map.Poly.t ) : (label, label Set.Poly.t) Map.Poly.t =
  let immediate_map =
    Map.mapi statement_map ~f:(fun ~key:label ~data:_ ->
        node_immediate_dependencies statement_map label ) in
  let step_node label m =
    let immediate = Map.find_exn immediate_map label in
    let updated =
      Set.union
        (union_map immediate ~f:(fun label -> Map.find_exn m label))
        immediate in
    Set.remove updated label in
  let step_map m = Map.mapi m ~f:(fun ~key:label ~data:_ -> step_node label m) in
  let map_equal = Map.Poly.equal Set.Poly.equal in
  let rec step_until_fixed m =
    let m' = step_map m in
    if map_equal m m' then m else step_until_fixed m' in
  step_until_fixed immediate_map

let mir_reaching_definitions (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    (label, reaching_defn Set.Poly.t entry_exit) Map.Poly.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt stmt in
  let (module Flowgraph) = flowgraph in
  let rd_map =
    reaching_definitions_mfp mir (module Flowgraph) flowgraph_to_mir in
  let to_rd_set set =
    Set.Poly.map set ~f:(fun (s, label_opt) ->
        (VVar s, Option.value label_opt ~default:1) ) in
  Map.Poly.map rd_map ~f:(fun {entry; exit} ->
      {entry= to_rd_set entry; exit= to_rd_set exit} )

let all_labels
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH with type labels = int)
    : int Set.Poly.t =
  let step set =
    Set.Poly.union set
      (union_map set ~f:(fun l -> Map.Poly.find_exn Flowgraph.successors l))
  in
  let rec step_fix set =
    let next = step set in
    if Set.Poly.equal set next then set else step_fix next in
  step_fix Flowgraph.initials

let prog_rhs_variables
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t)
    (labels : int Set.Poly.t) : string Set.Poly.t =
  let label_vars label =
    Set.Poly.map
      ~f:(fun (VVar s, _) -> s)
      (stmt_rhs_var_set (Map.Poly.find_exn flowgraph_to_mir label).pattern)
  in
  union_map labels ~f:label_vars

let stmt_uninitialized_variables (exceptions : string Set.Poly.t)
    (stmt : Stmt.Located.t) : (Location_span.t * string) Set.Poly.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt ~flatten_loops:true stmt in
  let (module Flowgraph) = flowgraph in
  let labels = all_labels (module Flowgraph) in
  let all_variables = prog_rhs_variables flowgraph_to_mir labels in
  let initialized_vars_map =
    initialized_vars_mfp all_variables (module Flowgraph) flowgraph_to_mir in
  let uninitialized =
    Map.Poly.fold initialized_vars_map ~init:Set.Poly.empty
      ~f:(fun ~key:label ~data:inits acc ->
        let stmt = Map.Poly.find_exn flowgraph_to_mir label in
        let rhs =
          Set.Poly.map
            ~f:(fun (VVar s, {loc; _}) -> (loc, s))
            (stmt_rhs_var_set stmt.pattern) in
        let uninitialized (_, var) = not (Set.Poly.mem inits.entry var) in
        let uninitialized_set = Set.Poly.filter ~f:uninitialized rhs in
        Set.Poly.union acc uninitialized_set ) in
  Set.Poly.filter uninitialized ~f:(fun (_, v) ->
      not (Set.Poly.mem exceptions v) )

let mir_uninitialized_variables (mir : Program.Typed.t) :
    (Location_span.t * string) Set.Poly.t =
  let flag_variables = List.map ~f:Flag_vars.to_string Flag_vars.enumerate in
  let data_vars = data_set ~exclude_transformed:true mir in
  let trans_data_vars = data_set ~exclude_transformed:false mir in
  let globals =
    Set.Poly.union
      (Set.Poly.of_list flag_variables)
      (Set.Poly.singleton "target") in
  let parameters =
    Set.Poly.of_list
      (List.map ~f:fst
         (List.filter
            ~f:(fun (_, {out_block; _}) -> out_block = Parameters)
            mir.output_vars ) ) in
  let globals_data = Set.Poly.union globals data_vars in
  let globals_data_prep =
    Set.Poly.union_list [globals_data; trans_data_vars; parameters] in
  Set.Poly.union_list
    [ (* prepare_data scope: data *)
      stmt_uninitialized_variables globals_data
        {pattern= SList mir.prepare_data; meta= Location_span.empty}
      (* log_prob scope: data, prep declarations *)
    ; stmt_uninitialized_variables globals_data_prep
        {pattern= SList mir.log_prob; meta= Location_span.empty}
      (* gen quant scope: data, prep declarations *)
    ; stmt_uninitialized_variables globals_data_prep
        {pattern= SList mir.generate_quantities; meta= Location_span.empty}
      (* functions scope: arguments *)
    ; Set.Poly.union_list
        (List.map mir.functions_block ~f:(fun {fdbody; fdargs; _} ->
             let arg_vars =
               Set.Poly.of_list
                 (List.map fdargs ~f:(fun (_, arg_name, _) -> arg_name)) in
             Option.value_map fdbody ~default:Set.Poly.empty ~f:(fun fdbody ->
                 stmt_uninitialized_variables
                   (Set.Poly.union arg_vars globals)
                   fdbody ) ) ) ]

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    ( label
    , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * node_dep_info )
    Map.Poly.t =
  let statement_map =
    build_statement_map
      (fun Stmt.Fixed.{pattern; _} -> pattern)
      (fun Stmt.Fixed.{meta; _} -> meta)
      stmt in
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = mir_reaching_definitions mir stmt in
  Map.Poly.mapi statement_map ~f:(fun ~key:label ~data:(stmt, meta) ->
      let rds = Map.find_exn rd_map label in
      ( stmt
      , { predecessors= Map.find_exn preds label
        ; parents= Map.find_exn parents label
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit
        ; meta } ) )

let log_prob_build_dep_info_map (mir : Program.Typed.t) :
    ( label
    , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * node_dep_info )
    Map.Poly.t =
  let log_prob_stmt =
    Stmt.Fixed.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dep_info_map mir log_prob_stmt

let log_prob_dependency_graph (mir : Program.Typed.t) :
    (label, label Set.Poly.t) Map.Poly.t =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map
