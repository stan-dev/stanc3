open Std
open Middle
open Dataflow_types
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework
open Dependence
open Dependence.Accesses

type location = Access_function.t Accesses.location

(* External definitions have no defining statement. *)
type reaching_definition = {statement_id: label option; location: location}

module Writer_dependencies = struct
  type t = label Set.Poly.t String.Map.t

  let empty = String.Map.empty

  let for_variable variable writers =
    match String.Map.find_opt variable writers with
    | None -> Set.Poly.empty
    | Some labels -> labels

  let add variable writer writers =
    let labels = Set.Poly.add writer (for_variable variable writers) in
    String.Map.add writers ~key:variable ~data:labels

  let fold_writer_sets blockers writers initial f =
    String.Map.fold writers ~init:initial
      ~f:(fun ~key:variable ~data:labels result ->
        if Set.Poly.mem variable blockers then result else f labels result)

  let fold_selected_writer_sets variables blockers writers initial f =
    Set.Poly.fold variables ~init:initial ~f:(fun variable result ->
        if Set.Poly.mem variable blockers then result
        else f (for_variable variable writers) result)

  let fold_writers blockers writers initial f =
    fold_writer_sets blockers writers initial (fun labels result ->
        Set.Poly.fold labels ~init:result ~f)

  let fold_selected_writers variables blockers writers initial f =
    fold_selected_writer_sets variables blockers writers initial
      (fun labels result -> Set.Poly.fold labels ~init:result ~f)

  let writer_ids blockers writers =
    fold_writer_sets blockers writers Set.Poly.empty Set.Poly.union

  let writer_ids_for_variables variables blockers writers =
    fold_selected_writer_sets variables blockers writers Set.Poly.empty
      Set.Poly.union
end

type statement =
  { id: label
  ; pattern: (Expr.Typed.t, label) Stmt.Pattern.t
  ; meta: Location_span.t
  ; controls: controls
  ; loop_ids: label list
  ; effects: Access_function.t Accesses.t }

type stmt_info = {statement: statement; writers: Writer_dependencies.t}

(* The first statement must include every enclosing branch/loop and jump control
   of the referenced statement. This compares controls, not addresses. *)
let has_all_controls_of (statements : statement LabelMap.t)
    (covering : statement) (statement_id : label) : bool =
  let covered = (LabelMap.find statement_id statements).controls in
  List.for_all covered.enclosing ~f:(fun enclosing ->
      List.mem enclosing ~set:covering.controls.enclosing)
  && Set.Poly.subset covered.jump_ids covering.controls.jump_ids

let is_killed_in_same_iteration (statements : statement LabelMap.t)
    (killer : statement) (killer_path : Dependence.path)
    (killed_statement : statement) (killed_path : Dependence.path) : bool =
  killed_statement.id < killer.id
  && Dependence.definitely_overwrites ~is_same_iteration:true killer_path
       killed_path
  &&
  match List.rev killed_statement.loop_ids with
  | innermost :: _ ->
      List.equal ~eq:Int.equal killer.loop_ids killed_statement.loop_ids
      && has_all_controls_of statements killed_statement killer.id
      && has_all_controls_of statements killed_statement innermost
  | [] -> false

let is_killed (statements : statement LabelMap.t) (killer : statement) writes
    (killed : reaching_definition) : bool =
  List.exists writes ~f:(fun write ->
      String.equal write.variable killed.location.variable
      && (Dependence.definitely_overwrites ~is_same_iteration:false write.path
            killed.location.path
         ||
         match killed.statement_id with
         | None -> false
         | Some killed_id ->
             is_killed_in_same_iteration statements killer write.path
               (LabelMap.find killed_id statements)
               killed.location.path))

(* IDs are unique per distinct statement/location definition. Only IDs enter the
   fixed point; expression trees stay here. *)
type definitions =
  { values: reaching_definition array
  ; generated: int Set.Poly.t LabelMap.t
  ; external_ids: int Set.Poly.t }

let build_definition_table (mir : Program.Typed.t) statements =
  let next_id = ref 0 in
  let values = ref [] in
  let add_definition statement_id location ids =
    let id = !next_id in
    incr next_id;
    values := {statement_id; location} :: !values;
    Set.Poly.add id ids in
  let add_external ids (variable, _, _) =
    add_definition None {variable; path= []} ids in
  let external_ids =
    List.fold_left mir.output_vars
      ~init:(List.fold_left mir.input_vars ~init:Set.Poly.empty ~f:add_external)
      ~f:add_external in
  let generated =
    LabelMap.map statements ~f:(fun statement ->
        List.fold_left
          (List.sort_uniq ~cmp:Dependence.compare_locations
             statement.effects.writes) ~init:Set.Poly.empty
          ~f:(fun ids location ->
            add_definition (Some statement.id) location ids)) in
  {values= Array.of_list (List.rev !values); generated; external_ids}

let locations_by_variable locations =
  List.fold_left locations ~init:String.Map.empty
    ~f:(fun by_variable location ->
      let previous =
        Option.value
          (String.Map.find_opt location.variable by_variable)
          ~default:[] in
      String.Map.add by_variable ~key:location.variable
        ~data:(location :: previous))

let compute_reaching_definitions (statements : statement LabelMap.t) definitions
    (module Flowgraph : FLOWGRAPH with type labels = int) =
  (* Overwrite is a property of two immutable statements, not of the current
     dataflow state. Cache only pairs actually encountered by the solver. *)
  let writes_by_statement =
    LabelMap.map statements ~f:(fun statement ->
        locations_by_variable statement.effects.writes) in
  let overwrite_cache = Stdlib.Hashtbl.create 32 in
  let overwrites statement_id definition_id =
    let definition = definitions.values.(definition_id) in
    let variable_writes = LabelMap.find statement_id writes_by_statement in
    match String.Map.find_opt definition.location.variable variable_writes with
    | None -> false
    | Some candidate_writes -> (
        match
          Stdlib.Hashtbl.find_opt overwrite_cache (statement_id, definition_id)
        with
        | Some result -> result
        | None ->
            let statement = LabelMap.find statement_id statements in
            let result =
              is_killed statements statement candidate_writes definition in
            Stdlib.Hashtbl.add overwrite_cache
              (statement_id, definition_id)
              result;
            result) in
  let generated statement_id =
    LabelMap.find statement_id definitions.generated in
  let results =
    reaching_definitions_mfp definitions.external_ids generated overwrites
      (module Flowgraph) in
  LabelMap.map results ~f:(fun statement_result -> statement_result.entry)

(** Both locations already have their read/write roles. This predicate concerns
    overlap and execution order, not the representation of dependency edges. *)
let write_may_supply_read (writer : statement) (write : location)
    (reader : statement) (read : location) : bool =
  if not (String.equal write.variable read.variable) then false
  else
    let overlap =
      Dependence.analyze_dependence writer.loop_ids write.path reader.loop_ids
        read.path in
    match overlap with
    | None -> false
    | Some vector ->
        Dependence.may_write_precede_read (writer.id < reader.id) vector

let build_writer_dependencies (statements : statement LabelMap.t) definitions
    (reader : statement) reaching_ids : Writer_dependencies.t =
  let variable_reads = locations_by_variable reader.effects.reads in
  Set.Poly.fold reaching_ids ~init:Writer_dependencies.empty
    ~f:(fun definition_id writers ->
      let definition = definitions.values.(definition_id) in
      match definition.statement_id with
      | None -> writers
      | Some writer_id -> (
          let write = definition.location in
          match String.Map.find_opt write.variable variable_reads with
          | None -> writers
          | Some candidate_reads ->
              let writer = LabelMap.find writer_id statements in
              if
                List.exists candidate_reads
                  ~f:(write_may_supply_read writer write reader)
              then Writer_dependencies.add write.variable writer_id writers
              else writers))

let prepend_control_dependencies info pending =
  Set.Poly.fold (control_dependency_ids info.statement.controls) ~init:pending
    ~f:(fun control_id queued_ids -> control_id :: queued_ids)

let prepend_dependencies blockers info pending =
  let with_controls = prepend_control_dependencies info pending in
  Writer_dependencies.fold_writers blockers info.writers with_controls
    (fun writer_id queued_ids -> writer_id :: queued_ids)

let dependency_closure statements blockers starts =
  let rec visit visited pending =
    match pending with
    | [] -> visited
    | id :: rest when Set.Poly.mem id visited -> visit visited rest
    | id :: rest ->
        let visited_with_statement = Set.Poly.add id visited in
        let pending_dependencies =
          prepend_dependencies blockers (LabelMap.find id statements) rest in
        visit visited_with_statement pending_dependencies in
  visit Set.Poly.empty starts

let transitive_dependence_ids statements statement_id =
  dependency_closure statements Set.Poly.empty [statement_id]

let variable_dependence_ids statements blockers variables statement_id =
  (* Variable selection applies only at the query root; blockers apply to every
     expanded statement, including a root reached again through a cycle. *)
  let info = LabelMap.find statement_id statements in
  let controls = prepend_control_dependencies info [] in
  let starts =
    Writer_dependencies.fold_selected_writers variables blockers info.writers
      controls (fun writer_id queued_ids -> writer_id :: queued_ids) in
  dependency_closure statements blockers starts

let read_variables_at (statements : stmt_info LabelMap.t)
    (statement_ids : label Set.Poly.t) : string Set.Poly.t =
  Set.Poly.fold statement_ids ~init:Set.Poly.empty ~f:(fun statement_id names ->
      let statement = (LabelMap.find statement_id statements).statement in
      List.fold_left statement.effects.reads ~init:names
        ~f:(fun accumulated_names read ->
          Set.Poly.add read.variable accumulated_names))

let build_dependence_info (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    stmt_info LabelMap.t =
  let statement_map = build_statement_map stmt in
  let exits, predecessors, control_ids = build_control_flow statement_map in
  let inverse =
    (module struct
      type labels = int

      let initials = exits
      let successors = predecessors
    end : FLOWGRAPH
      with type labels = int) in
  let flowgraph = Monotone_framework.reverse inverse in
  let collected_effects =
    LabelMap.map statement_map ~f:(fun (pattern, _) ->
        Accesses.of_statement pattern) in
  let defined_vars =
    LabelMap.fold collected_effects ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:effects names ->
        List.fold_left effects.writes ~init:names
          ~f:(fun accumulated_names write ->
            Set.Poly.add write.variable accumulated_names)) in
  let context = Loop_context.create statement_map defined_vars in
  let statements =
    LabelMap.mapi statement_map ~f:(fun statement_id (pattern, meta) ->
        let controls = LabelMap.find statement_id control_ids in
        let loop_ids = enclosing_loop_ids statement_id pattern controls in
        let scope = Loop_context.create_scope context loop_ids in
        let collected = LabelMap.find statement_id collected_effects in
        (* Loop-variable reads are represented by the enclosing control, rather
           than by a reaching definition of the loop variable. *)
        let reads =
          List.filter collected.reads ~f:(fun read ->
              not (Loop_context.is_loop_variable scope read.variable)) in
        let effects =
          Accesses.map_scalars
            (Access_function.of_expr scope)
            {collected with reads} in
        {id= statement_id; pattern; meta; controls; loop_ids; effects}) in
  let definitions = build_definition_table mir statements in
  let reaching_definitions =
    compute_reaching_definitions statements definitions flowgraph in
  LabelMap.mapi statements ~f:(fun statement_id statement ->
      let reaching_ids = LabelMap.find statement_id reaching_definitions in
      { statement
      ; writers=
          build_writer_dependencies statements definitions statement
            reaching_ids })

let build_log_prob_dependence_info (mir : Program.Typed.t) :
    stmt_info LabelMap.t =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dependence_info mir log_prob_stmt
