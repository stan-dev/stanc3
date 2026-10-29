(* The interface file describes the analysis and the running example. This file
   uses the terms below.

   An access is one read, write or increment of a variable. The path of an
   access lists the subscripts and tuple fields that select an element of the
   variable ({!Accesses}).

   A definition records a write as the variable name and the label of the
   writing statement. A definition reaches a statement if the program can go
   from the write to the statement with no write between them that overwrites
   the same element. Reaching definitions
   ({!Monotone_framework.reaching_definitions_mfp}) finds these definitions.

   A later write kills an earlier definition when the later write overwrites
   each element that the earlier write wrote. The killed definition no longer
   reaches later statements. An element kill compares the paths of two writes to
   single elements, such as [theta[n, 1]], to decide this.

   The controls of a statement are the ifs and loops around the statement, and
   the jumps ([break], [continue] and [return]) that can skip the statement
   ({!Dataflow_types.controls}).

   Each reaching definition of a variable that a statement reads is a candidate
   source for the read, and the read is the sink. {!Dependence} rejects a
   candidate source that can never supply the value of the sink. *)

open Std
open Middle
open Dataflow_types
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

(** Returns the path of the first write to [var] in [accesses], or [None] if
    [accesses] does not write [var]. *)
let write_path (accesses : Accesses.index Accesses.t) (var : string) :
    Accesses.index Accesses.step list option =
  List.find_map accesses ~f:(fun (access : Accesses.index Accesses.access) ->
      match access.kind with
      | Write when String.equal access.var var -> Some access.path
      | Write | Read | Increment -> None)

type statement =
  { pattern: (Expr.Typed.t, label) Stmt.Pattern.t
  ; meta: Location_span.t
  ; controls: controls
  ; accesses: Accesses.index Accesses.t }

type dependence_info =
  { statements: statement LabelMap.t
  ; reaching_definitions: reaching_defn Set.Poly.t LabelMap.t }

(** The loops around the statement at [statement_id], outermost first. *)
let statement_loops (statements : statement LabelMap.t) (statement_id : label) :
    loop list =
  let statement = LabelMap.find statement_id statements in
  loops_around ~statement_id statement.pattern statement.controls

(** The [kills] callback for reaching definitions, which decides element kills.
    A later write [killer] kills the definition at [killed] when [killer] must
    overwrite the element of [killed]. Two rules give this result. *)
let element_kills ~(statements : statement LabelMap.t) ~(killer : label)
    ~(killed : label) (var : string) : bool =
  let killer_statement = LabelMap.find killer statements in
  let killed_statement = LabelMap.find killed statements in
  (* Whether each if, loop and jump that decides if [statement_id] runs also
     decides if [killed] runs. *)
  let controls_cover (statement_id : label) : bool =
    let covered = (LabelMap.find statement_id statements).controls in
    List.for_all covered.enclosing ~f:(fun enclosing ->
        List.mem enclosing ~set:killed_statement.controls.enclosing)
    && Set.Poly.subset covered.jumps killed_statement.controls.jumps in
  match
    ( write_path killer_statement.accesses var
    , write_path killed_statement.accesses var )
  with
  | Some killer_path, Some killed_path -> (
      (* Rule A of 2: [killer] writes the element of [killed] in every
         iteration. *)
      Accesses.must_overwrite ~same_iteration:false ~killer:killer_path
        ~killed:killed_path
      ||
      (* Rule B of 2: [killer] writes the element of [killed] in the same
         iteration of the innermost loop around [killed]. [killer] comes later
         in source order and has the same loops. Each control of [killer], and
         of the innermost loop, is also a control of [killed]. The loops are
         found last, after the cheaper checks. *)
      killer > killed
      && Accesses.must_overwrite ~same_iteration:true ~killer:killer_path
           ~killed:killed_path
      &&
      let killed_loops = statement_loops statements killed in
      match List.rev killed_loops with
      | innermost :: _ ->
          List.equal ~eq:same_loop
            (statement_loops statements killer)
            killed_loops
          && controls_cover killer
          && controls_cover (loop_id innermost)
      | [] -> false)
  | None, _ | _, None -> false

(** Runs reaching definitions with [kills] on the forward flowgraph of
    [statement_map]. Returns the definitions that reach the start of each label.
*)
let mir_reaching_definitions
    ~(kills : killer:label -> killed:label -> string -> bool)
    (mir : Program.Typed.t)
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t) :
    reaching_defn Set.Poly.t LabelMap.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_statement_map statement_map in
  let (module Flowgraph) = flowgraph in
  LabelMap.map
    (reaching_definitions_mfp ~kills mir (module Flowgraph) flowgraph_to_mir)
    ~f:(fun definitions -> definitions.entry)

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    dependence_info =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let _, _, control_ids = build_cf_graphs statement_map in
  let all_stmt_accesses =
    LabelMap.map statement_map ~f:(fun (pattern, _) -> Accesses.of_stmt pattern)
  in
  (* [classify] marks an index variable as invariant (the same in each
     iteration) only if no statement in [stmt] writes that variable. A written
     variable can change between iterations. For example, a write [m = n] stops
     the classification of index [m] as invariant. *)
  let written_vars =
    LabelMap.fold all_stmt_accesses ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:stmt_accesses running_written_vars ->
        Set.Poly.union running_written_vars
          (Accesses.written_vars stmt_accesses)) in
  let statements =
    LabelMap.mapi statement_map ~f:(fun statement_id (pattern, meta) ->
        let controls = LabelMap.find statement_id control_ids in
        let loopvars =
          Set.Poly.of_list
            (List.filter_map (loops_around ~statement_id pattern controls)
               ~f:(function
              | For {var; _} -> Some var
              | While _ -> None)) in
        let accesses =
          Accesses.classify ~loopvars ~written_vars
            (LabelMap.find statement_id all_stmt_accesses) in
        {pattern; meta; controls; accesses}) in
  (* The kills use the classified accesses, so classification comes first. *)
  let reaching_definitions =
    mir_reaching_definitions
      ~kills:(element_kills ~statements)
      mir statement_map in
  {statements; reaching_definitions}

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

(** The root label is the label of the analyzed statement itself. Results that
    hold only labels use the root label to mark a definition from outside the
    analyzed statement. Each label map of the analysis contains the root. *)
let root_label : label = 1

(** Returns the labels of the candidate sources of [var] at [sink_id] that
    {!Dependence} keeps. A candidate stays if the source does not write or
    increment [var], if the sink does not use [var], or if some access of [var]
    in the source and some access in the sink pass {!flows_into} and
    {!Dependence.is_plausible}. A definition from outside gives [root_label]. *)
let writes_read_by (info : dependence_info) ~(sink_id : label) (var : string) :
    label Set.Poly.t =
  let sink = LabelMap.find sink_id info.statements in
  let sink_accesses = Accesses.of_var var sink.accesses in
  let candidate_definitions =
    reaching_defn_lookup (LabelMap.find sink_id info.reaching_definitions) var
  in
  let sink_has_reads =
    List.exists sink_accesses ~f:(fun access ->
        match access.Accesses.kind with
        | Read | Increment -> true
        | Write -> false) in
  let sink_loops = lazy (statement_loops info.statements sink_id) in
  let source_may_supply_read source_id =
    let source = LabelMap.find source_id info.statements in
    let source_accesses = Accesses.of_var var source.accesses in
    let source_has_writes =
      List.exists source_accesses ~f:(fun access ->
          match access.Accesses.kind with
          | Write | Increment -> true
          | Read -> false) in
    if (not source_has_writes) || not sink_has_reads then true
    else
      let loops =
        common_loops
          (statement_loops info.statements source_id)
          (Lazy.force sink_loops) in
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
        (* A definition from outside does not have an access record, so the root
           label stands for the definition. *)
        Some root_label
    | Some source_id ->
        if source_may_supply_read source_id then Some source_id else None)

let node_immediate_dependencies (info : dependence_info)
    ?(blockers : string Set.Poly.t = Set.Poly.empty)
    ?(vars : string Set.Poly.t option) (node_id : label) : label Set.Poly.t =
  let node = LabelMap.find node_id info.statements in
  let vars = Option.value vars ~default:(Accesses.read_vars node.accesses) in
  let var_dependency_ids =
    Set.Poly.union_map
      (Set.Poly.diff vars blockers)
      ~f:(writes_read_by info ~sink_id:node_id) in
  Set.Poly.union (control_parents node.controls) var_dependency_ids

(** Adds [node_id] and the direct and transitive dependencies of [node_id] to
    [visited_ids], without following the definitions of [blockers]. *)
let rec node_dependencies_rec (info : dependence_info)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (node_id : label)
    (visited_ids : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem node_id visited_ids then visited_ids
  else
    (* The visited set is the result and includes the start label. *)
    let visited_with_node_ids = Set.Poly.add node_id visited_ids in
    let dependency_ids = node_immediate_dependencies info ~blockers node_id in
    Set.Poly.fold dependency_ids ~init:visited_with_node_ids
      ~f:(node_dependencies_rec info ~blockers)

let node_dependencies (info : dependence_info) (node_id : label) :
    label Set.Poly.t =
  node_dependencies_rec info node_id Set.Poly.empty

let node_vars_dependencies (info : dependence_info)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (vars : string Set.Poly.t)
    (node_id : label) : label Set.Poly.t =
  Set.Poly.fold
    (node_immediate_dependencies info ~blockers ~vars node_id)
    ~init:Set.Poly.empty
    ~f:(node_dependencies_rec info ~blockers)

let read_variables_at (info : dependence_info) (labels : label Set.Poly.t) :
    string Set.Poly.t =
  Set.Poly.union_map labels ~f:(fun label ->
      Accesses.read_vars (LabelMap.find label info.statements).accesses)

let all_node_dependencies (info : dependence_info) : label Set.Poly.t LabelMap.t
    =
  let immediate_map =
    LabelMap.mapi info.statements ~f:(fun label _ ->
        node_immediate_dependencies info label) in
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

let log_prob_build_dep_info_map (mir : Program.Typed.t) : dependence_info =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dep_info_map mir log_prob_stmt

let log_prob_dependency_graph (mir : Program.Typed.t) :
    label Set.Poly.t LabelMap.t =
  all_node_dependencies (log_prob_build_dep_info_map mir)
