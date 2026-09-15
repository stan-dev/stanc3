open Std
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
  ; accesses: access list
        (** the node's own reads and writes, classified with respect to the
            innermost enclosing loop (L4, §7.7) *)
  ; meta: Location_span.t }

(** Find all of the reaching definitions of a variable in an RD set *)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : vexpr) :
    label Set.Poly.t =
  Set.Poly.map (Set.Poly.filter rds ~f:(fun (var', _) -> var' = var)) ~f:snd

(** With [refine], a reaching definition [(v, l')] of a right-hand-side variable
    [v] at [label] is dropped when every write access to [v] at [l'] is
    [Independent] of every read access to [v] at [label] (§7.7): e.g.
    [theta[2] = b] does not reach [normal(theta[1], s)]. A definition whose node
    has no recorded write to [v] (a definition from outside the analysed
    statement), or a node with no recorded read, is always kept. The per-access
    subscripts are relative to each node's own loop, which is sound for this
    purpose: [access_dependence] only answers [Independent] when no pair of
    integer iteration values can make the subscripts coincide. *)
let refined_reaching_defn_lookup
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    (info : node_dep_info) (v : string) : label Set.Poly.t =
  let defs = reaching_defn_lookup info.reaching_defn_entry (VVar v) in
  let reads =
    List.filter info.accesses ~f:(fun a ->
        access_reads a && String.equal a.var v) in
  Set.Poly.filter defs ~f:(fun def_label ->
      match LabelMap.find_opt def_label statement_map with
      | None -> true
      | Some (_, def_info) -> (
          let writes =
            List.filter def_info.accesses ~f:(fun a ->
                access_writes a && String.equal a.var v) in
          match (writes, reads) with
          | [], _ | _, [] -> true
          | writes, reads ->
              List.exists writes ~f:(fun w ->
                  List.exists reads ~f:(fun r ->
                      match Loop_dependence.access_dependence w r with
                      | Independent -> false
                      | Dependent _ -> true))))

let node_immediate_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    ?(blockers : vexpr Set.Poly.t = Set.Poly.empty) ?(refine = false)
    (label : label) : label Set.Poly.t =
  let stmt, info = LabelMap.find label statement_map in
  let rhs_set = Set.Poly.map (stmt_rhs_var_set stmt) ~f:fst in
  let lookup (VVar v as var) =
    if refine then refined_reaching_defn_lookup statement_map info v
    else reaching_defn_lookup info.reaching_defn_entry var in
  let rhs_deps = Set.Poly.union_map (Set.Poly.diff rhs_set blockers) ~f:lookup in
  Set.Poly.union info.parents rhs_deps

(* This is doing an explicit graph traversal with edges defined by
   node_immediate_dependencies. *)
let rec node_dependencies_rec
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    ?(blockers : vexpr Set.Poly.t = Set.Poly.empty) (label : label)
    (visited : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem label visited then visited
  else
    let visited' = Set.Poly.add label visited in
    let deps = node_immediate_dependencies statement_map ~blockers label in
    Set.Poly.fold deps ~init:visited' ~f:(node_dependencies_rec statement_map)

let node_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    (label : label) : label Set.Poly.t =
  node_dependencies_rec statement_map label Set.Poly.empty

let node_vars_dependencies
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    ?(blockers : vexpr Set.Poly.t = Set.Poly.empty) (vars : vexpr Set.Poly.t)
    (label : label) : label Set.Poly.t =
  let _, info = LabelMap.find label statement_map in
  let var_deps =
    Set.Poly.union_map
      (Set.Poly.diff vars blockers)
      ~f:(reaching_defn_lookup info.reaching_defn_entry) in
  Set.Poly.fold
    (Set.Poly.union info.parents var_deps)
    ~init:Set.Poly.empty
    ~f:(node_dependencies_rec statement_map ~blockers)

(* The strategy here is to write an update function on the whole dependency
   graph in terms of node_immediate_dependencies, and then to find a
   fixed-point. Since it's updating the dependencies for the whole graph at a
   time, it should be more efficient than doing a graph traversal for each
   node. *)
let all_node_dependencies ?(refine = false)
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t) :
    label Set.Poly.t LabelMap.t =
  let immediate_map =
    LabelMap.mapi statement_map ~f:(fun label _ ->
        node_immediate_dependencies statement_map ~refine label) in
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

let mir_reaching_definitions (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    reaching_defn Set.Poly.t entry_exit LabelMap.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt stmt in
  let (module Flowgraph) = flowgraph in
  let rd_map =
    reaching_definitions_mfp mir (module Flowgraph) flowgraph_to_mir in
  let to_rd_set set =
    Set.Poly.map set ~f:(fun (s, label_opt) ->
        (VVar s, Option.value label_opt ~default:1)) in
  LabelMap.map rd_map ~f:(fun {entry; exit} ->
      {entry= to_rd_set entry; exit= to_rd_set exit})

let all_labels
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int) : int Set.Poly.t =
  let step set =
    Set.Poly.union set
      (Set.Poly.union_map set ~f:(fun l -> LabelMap.find l Flowgraph.successors))
  in
  let rec step_fix set =
    let next = step set in
    if Set.Poly.equal set next then set else step_fix next in
  step_fix Flowgraph.initials

let prog_rhs_variables
    (flowgraph_to_mir : Stmt.Located.Non_recursive.t LabelMap.t)
    (labels : int Set.Poly.t) : string Set.Poly.t =
  let label_vars label =
    Set.Poly.map
      ~f:(fun (VVar s, _) -> s)
      (stmt_rhs_var_set (LabelMap.find label flowgraph_to_mir).pattern) in
  Set.Poly.union_map labels ~f:label_vars

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
    LabelMap.fold initialized_vars_map ~init:Set.Poly.empty
      ~f:(fun ~key:label ~data:inits acc ->
        let stmt = LabelMap.find label flowgraph_to_mir in
        let rhs =
          Set.Poly.map
            ~f:(fun (VVar s, Expr.Typed.Meta.{loc; _}) -> (loc, s))
            (stmt_rhs_var_set stmt.pattern) in
        let uninitialized (_, var) = not (Set.Poly.mem var inits.entry) in
        let uninitialized_set = Set.Poly.filter ~f:uninitialized rhs in
        Set.Poly.union acc uninitialized_set) in
  Set.Poly.filter uninitialized ~f:(fun (_, v) ->
      not (Set.Poly.mem v exceptions))

let mir_uninitialized_variables (mir : Program.Typed.t) :
    (Location_span.t * string) Set.Poly.t =
  let flag_variables = List.map ~f:Flag_vars.to_string Flag_vars.enumerate in
  let function_names = function_names mir in
  let data_vars = data_set ~exclude_transformed:true mir in
  let globals =
    Set.Poly.union function_names (Set.Poly.of_list flag_variables) in
  let parameters =
    Set.Poly.of_list
      (List.map ~f:fst3
         (List.filter
            ~f:(fun (_, _, Program.{out_block; _}) -> out_block = Parameters)
            mir.output_vars)) in
  let globals_data = Set.Poly.union globals data_vars in
  let check_against_data b =
    stmt_uninitialized_variables globals_data
      {pattern= SList b; meta= Location_span.empty} in
  let check_against_data_params b =
    let globals_data_prep = Set.Poly.union globals_data parameters in
    stmt_uninitialized_variables globals_data_prep
      (* prepend prepare_data to detect bad transformed data usages *)
      {pattern= SList (mir.prepare_data @ b); meta= Location_span.empty} in
  Set.Poly.union_list
    [ (* prepare_data scope: data *)
      check_against_data mir.prepare_data
      (* log_prob scope: data, prep declarations *)
    ; check_against_data_params mir.log_prob
    ; check_against_data_params mir.reverse_mode_log_prob
      (* gen quant scope: data, prep declarations *)
    ; check_against_data_params mir.generate_quantities
      (* functions scope: arguments *)
    ; Set.Poly.union_list
        (List.map mir.functions_block ~f:(fun Program.{fdbody; fdargs; _} ->
             let arg_vars =
               Set.Poly.of_list
                 (List.map fdargs ~f:(fun (_, arg_name, _) -> arg_name)) in
             Option.value_map fdbody ~default:Set.Poly.empty ~f:(fun fdbody ->
                 stmt_uninitialized_variables
                   (Set.Poly.union arg_vars globals)
                   fdbody))) ]

(** The own accesses of every node of a statement map (substatements are their
    own nodes), classified with respect to the innermost enclosing [For]: its
    loop variable, and the names assigned or declared anywhere in its body.
    Nodes outside any loop have no loop variable, so their subscripts are
    [Invariant] or [Varying]. *)
let node_accesses_map
    (statement_map : ((Expr.Typed.t, label) Stmt.Pattern.t * 'm) LabelMap.t) :
    access list LabelMap.t =
  let child_labels pattern =
    Stmt.Pattern.fold (fun acc _ -> acc) (fun acc l -> l :: acc) [] pattern
  in
  let parent_of =
    LabelMap.fold statement_map ~init:LabelMap.empty
      ~f:(fun ~key ~data:(pattern, _) acc ->
        List.fold_left (child_labels pattern) ~init:acc ~f:(fun acc child ->
            LabelMap.add acc ~key:child ~data:key)) in
  let own_written (pattern : (Expr.Typed.t, label) Stmt.Pattern.t) =
    match pattern with
    | Assignment (lhs, _, _) ->
        Set.Poly.singleton (Stmt.Helpers.lhs_variable lhs)
    | Decl {decl_id; _} -> Set.Poly.singleton decl_id
    | For {loopvar; _} -> Set.Poly.singleton loopvar
    | TargetPE _ | JacobianPE _ | NRFunApp _ | Break | Continue | Return _
     |Skip | IfElse _ | While _ | Profile _ | Block _ | SList _ ->
        Set.Poly.empty in
  let rec subtree_written label =
    let pattern, _ = LabelMap.find label statement_map in
    List.fold_left (child_labels pattern) ~init:(own_written pattern)
      ~f:(fun acc child -> Set.Poly.union acc (subtree_written child)) in
  let rec enclosing_loop label =
    match LabelMap.find_opt label parent_of with
    | None -> None
    | Some parent -> (
        match fst (LabelMap.find parent statement_map) with
        | For {loopvar; _} -> Some (loopvar, parent)
        | Assignment _ | TargetPE _ | JacobianPE _ | NRFunApp _ | Break
         |Continue | Return _ | Skip | IfElse _ | While _ | Profile _
         |Block _ | SList _ | Decl _ ->
            enclosing_loop parent) in
  LabelMap.mapi statement_map ~f:(fun label (pattern, _) ->
      let loopvar, written_vars =
        match enclosing_loop label with
        | Some (loopvar, for_label) -> (loopvar, subtree_written for_label)
        | None -> ("", Set.Poly.empty) in
      Loop_dependence.accesses_of_pattern ~loopvar ~written_vars ~label
        ~sub:(fun _ -> [])
        pattern)

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = mir_reaching_definitions mir stmt in
  let accesses = node_accesses_map statement_map in
  LabelMap.mapi statement_map ~f:(fun label (stmt, meta) ->
      let rds = LabelMap.find label rd_map in
      ( stmt
      , { predecessors= LabelMap.find label preds
        ; parents= LabelMap.find label parents
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit
        ; accesses= LabelMap.find label accesses
        ; meta } ))

let log_prob_build_dep_info_map (mir : Program.Typed.t) :
    ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dep_info_map mir log_prob_stmt

let log_prob_dependency_graph (mir : Program.Typed.t) :
    label Set.Poly.t LabelMap.t =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map
