open Std
open Middle
open Dataflow_types
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

let write_path (accesses : Accesses.index Accesses.t) (var : string) :
    Accesses.index Accesses.step list option =
  List.find_map accesses ~f:(fun (access : Accesses.index Accesses.access) ->
      match access.kind with
      | Write when String.equal access.var var -> Some access.path
      | Write | Read | Increment -> None)

(* The [kills] callback for reaching definitions. A later write [killer] removes
   the definition at [killed] when it must overwrite the element of [killed].
   Two rules give this result. *)
let element_kills ~(accesses : Accesses.index Accesses.t LabelMap.t)
    ~(controls : control list LabelMap.t)
    ~(loops : Dependence.loop list LabelMap.t) ~(killer : label)
    ~(killed : label) (var : string) : bool =
  let killed_controls = LabelMap.find killed controls in
  (* Whether each if, loop and jump that decides if [statement] runs also
     decides if [killed] runs. *)
  let controls_cover (statement : label) : bool =
    List.for_all (LabelMap.find statement controls) ~f:(fun control ->
        List.mem control ~set:killed_controls) in
  match
    ( write_path (LabelMap.find killer accesses) var
    , write_path (LabelMap.find killed accesses) var )
  with
  | Some killer_path, Some killed_path -> (
      (* Rule A of 2: [killer] writes the element of [killed] in every
         iteration. *)
      Accesses.must_overwrite ~same_iteration:false ~killer:killer_path
        ~killed:killed_path
      ||
      match List.rev (LabelMap.find killed loops) with
      | innermost :: _ ->
          Accesses.must_overwrite ~same_iteration:true ~killer:killer_path
            ~killed:killed_path
          && killer > killed
          && List.equal
               ~eq:(fun (left : Dependence.loop) (right : Dependence.loop) ->
                 left.id = right.id)
               (LabelMap.find killer loops)
               (LabelMap.find killed loops)
          && controls_cover killer
          && controls_cover innermost.id
      | [] -> false)
  | None, _ | _, None -> false

type node_dep_info =
  { predecessors: label Set.Poly.t
  ; controls: control list
  ; reaching_definitions: reaching_defn Set.Poly.t entry_exit
  ; accesses: Accesses.index Accesses.t
  ; meta: Location_span.t }

type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

let mir_reaching_definitions
    ~(kills : killer:label -> killed:label -> string -> bool)
    (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    reaching_defn Set.Poly.t entry_exit LabelMap.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt stmt in
  let (module Flowgraph) = flowgraph in
  reaching_definitions_mfp ~kills mir (module Flowgraph) flowgraph_to_mir

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    dep_info_map =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let _, predecessor_ids, control_ids = build_cf_graphs statement_map in
  let enclosing_loops =
    LabelMap.mapi statement_map ~f:(fun statement_id (pattern, _) ->
        loops_around ~statement_id pattern
          (LabelMap.find statement_id control_ids)) in
  let all_stmt_accesses =
    LabelMap.map statement_map ~f:(fun (pattern, _) -> Accesses.of_stmt pattern)
  in
  (* [classify] marks an index variable as invariant only if no statement in
     [stmt] writes it. A written variable can change between iterations. For
     example, a write [m = n] stops the classification of index [m] as
     invariant. *)
  let written_vars =
    LabelMap.fold all_stmt_accesses ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:stmt_accesses running_written_vars ->
        Set.Poly.union running_written_vars
          (Accesses.written_vars stmt_accesses)) in
  let classified_accesses =
    LabelMap.mapi all_stmt_accesses ~f:(fun statement_id raw_accesses ->
        let loopvars =
          Set.Poly.of_list
            (List.filter_map (LabelMap.find statement_id enclosing_loops)
               ~f:(fun loop -> loop.Dependence.var)) in
        Accesses.classify ~loopvars ~written_vars raw_accesses) in
  (* The kills use the classified accesses, so classification comes first. *)
  let kills =
    element_kills ~accesses:classified_accesses ~controls:control_ids
      ~loops:enclosing_loops in
  let reaching_definitions = mir_reaching_definitions ~kills mir stmt in
  LabelMap.mapi statement_map ~f:(fun statement_id (pattern, meta) ->
      ( pattern
      , { predecessors= LabelMap.find statement_id predecessor_ids
        ; controls= LabelMap.find statement_id control_ids
        ; reaching_definitions= LabelMap.find statement_id reaching_definitions
        ; accesses= LabelMap.find statement_id classified_accesses
        ; meta } ))

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

(* The root label is the label of the analyzed statement itself. Results that
   hold only labels use it to mark a definition from outside the analyzed
   statement. Each label map of the analysis contains the root. *)
let root_label : label = 1

let writes_read_by (statement_map : dep_info_map) ~(sink_id : label)
    (var : string) : label Set.Poly.t =
  let sink_pattern, sink_info = LabelMap.find sink_id statement_map in
  let sink_accesses = Accesses.of_var var sink_info.accesses in
  let candidate_definitions =
    reaching_defn_lookup sink_info.reaching_definitions.entry var in
  let sink_has_reads =
    List.exists sink_accesses ~f:(fun access ->
        match access.Accesses.kind with
        | Read | Increment -> true
        | Write -> false) in
  let source_may_supply_read source_id =
    let source_pattern, source_info = LabelMap.find source_id statement_map in
    let source_accesses = Accesses.of_var var source_info.accesses in
    let source_has_writes =
      List.exists source_accesses ~f:(fun access ->
          match access.Accesses.kind with
          | Write | Increment -> true
          | Read -> false) in
    if (not source_has_writes) || not sink_has_reads then true
    else
      let loops =
        Dependence.common_loops
          (loops_around ~statement_id:source_id source_pattern
             source_info.controls)
          (loops_around ~statement_id:sink_id sink_pattern sink_info.controls)
      in
      (* Labels follow source order. Equal labels mean a write and a read in one
         statement, and the read runs first. *)
      let source_first_in_text = source_id < sink_id in
      List.exists source_accesses ~f:(fun source ->
          List.exists sink_accesses ~f:(fun sink ->
              flows_into source sink
              && Dependence.is_plausible ~source_first_in_text
                   (Dependence.compare_paths ~loops source sink))) in
  Set.Poly.filter_map candidate_definitions ~f:(function
    | None ->
        (* A definition from outside has no access record, so return the root
           label for it. *)
        Some root_label
    | Some source_id ->
        if source_may_supply_read source_id then Some source_id else None)

let node_immediate_dependencies (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty)
    ?(vars : string Set.Poly.t option) (node_id : label) : label Set.Poly.t =
  let _, node_info = LabelMap.find node_id statement_map in
  let vars =
    Option.value vars ~default:(Accesses.read_vars node_info.accesses) in
  let var_dependency_ids =
    Set.Poly.union_map
      (Set.Poly.diff vars blockers)
      ~f:(writes_read_by statement_map ~sink_id:node_id) in
  Set.Poly.union (control_parents node_info.controls) var_dependency_ids

let rec node_dependencies_rec (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (node_id : label)
    (visited_ids : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem node_id visited_ids then visited_ids
  else
    (* The visited set is the result, and it includes the start label. *)
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
  Set.Poly.fold
    (node_immediate_dependencies statement_map ~blockers ~vars node_id)
    ~init:Set.Poly.empty
    ~f:(node_dependencies_rec statement_map ~blockers)

let read_variables_at (statement_map : dep_info_map) (labels : label Set.Poly.t)
    : string Set.Poly.t =
  Set.Poly.union_map labels ~f:(fun label ->
      let _, info = LabelMap.find label statement_map in
      Accesses.read_vars info.accesses)

type dependency_graph = label Set.Poly.t LabelMap.t

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

let log_prob_build_dep_info_map (mir : Program.Typed.t) : dep_info_map =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dep_info_map mir log_prob_stmt

let log_prob_dependency_graph (mir : Program.Typed.t) : dependency_graph =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map
