open Std
open Middle
open Dataflow_types
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework
open Dependence

type access = Access_function.t Accesses.access

type reaching_definition =
  {access: access; statement_id: label option; loop_ids: label list}

type stmt_info =
  { id: label
  ; pattern: (Expr.Typed.t, label) Stmt.Pattern.t
  ; meta: Location_span.t
  ; controls: controls
  ; loop_ids: label list
  ; accesses: access list
  ; reaching_definitions: reaching_definition Set.Poly.t
  ; writers: (string * label) Set.Poly.t }

let is_covered_by ~(statements : stmt_info LabelMap.t) ~(covering : stmt_info)
    (statement_id : label) : bool =
  let covered = (LabelMap.find statement_id statements).controls in
  List.for_all covered.enclosing ~f:(fun enclosing ->
      List.mem enclosing ~set:covering.controls.enclosing)
  && Set.Poly.subset covered.jump_ids covering.controls.jump_ids

let is_killed_in_same_iteration ~(statements : stmt_info LabelMap.t)
    ~(killer : label) (killer_path : Dependence.path)
    (killed : reaching_definition) : bool =
  let killer_statement = LabelMap.find killer statements in
  match killed.statement_id with
  | None -> false
  | Some killed_id -> (
      let killed_statement = LabelMap.find killed_id statements in
      killed_id < killer
      && Dependence.is_overwritten ~is_same_iteration:true killer_path
           killed.access.path
      &&
      match List.rev killed.loop_ids with
      | innermost :: _ ->
          List.equal ~eq:Int.equal killer_statement.loop_ids killed.loop_ids
          && is_covered_by ~statements ~covering:killed_statement killer
          && is_covered_by ~statements ~covering:killed_statement innermost
      | [] -> false)

let is_killed ~(statements : stmt_info LabelMap.t) ~(killer : label)
    (killed : reaching_definition) : bool =
  let killer_statement = LabelMap.find killer statements in
  List.exists killer_statement.accesses ~f:(fun (access : access) ->
      String.equal access.var killed.access.var
      &&
      match access.kind with
      | Read -> false
      | Write | ReadWrite ->
          Dependence.is_overwritten ~is_same_iteration:false access.path
            killed.access.path
          || is_killed_in_same_iteration ~statements ~killer access.path killed)

let outside_definition ((name, _, _) : string * 'location * 'var_type) :
    reaching_definition =
  {access= {var= name; path= []; kind= Write}; statement_id= None; loop_ids= []}

let definition_of_access (statement : stmt_info) (access : access) :
    reaching_definition option =
  match access.kind with
  | Write | ReadWrite ->
      Some
        {access; statement_id= Some statement.id; loop_ids= statement.loop_ids}
  | Read -> None

let definitions_of (statements : stmt_info LabelMap.t) (statement_id : label) :
    reaching_definition Set.Poly.t =
  let statement = LabelMap.find statement_id statements in
  statement.accesses
  |> List.filter_map ~f:(definition_of_access statement)
  |> Set.Poly.of_list

let compute_reaching_definitions (mir : Program.Typed.t)
    (statements : stmt_info LabelMap.t) :
    reaching_definition Set.Poly.t LabelMap.t =
  let flowgraph, _ =
    statements
    |> LabelMap.map ~f:(fun {pattern; meta; _} -> (pattern, meta))
    |> Monotone_framework.forward_flowgraph_of_statement_map in
  let (module Flowgraph) = flowgraph in
  let outside_definitions =
    Set.Poly.of_list
      (List.map mir.input_vars ~f:outside_definition
      @ List.map mir.output_vars ~f:outside_definition) in
  let definitions_at_entry_and_exit =
    reaching_definitions_mfp ~initial:outside_definitions
      ~gen:(definitions_of statements)
      ~is_killed:(is_killed ~statements)
      (module Flowgraph) in
  LabelMap.map definitions_at_entry_and_exit ~f:(fun definitions ->
      definitions.entry)

let is_flow_dependence (writer_id : label) (definition : reaching_definition)
    (statement : stmt_info) (read : access) : bool =
  String.equal definition.access.var read.var
  && Dependence.is_writer_possibly_first (writer_id < statement.id)
       (Dependence.test_dependence
          (definition.loop_ids, definition.access.path)
          (statement.loop_ids, read.path))

let writers_of_statement (statement : stmt_info) : (string * label) Set.Poly.t =
  let reads =
    List.filter statement.accesses ~f:(fun (access : access) ->
        match access.kind with Read | ReadWrite -> true | Write -> false) in
  Set.Poly.union_map statement.reaching_definitions ~f:(fun definition ->
      match definition.statement_id with
      | None -> Set.Poly.empty
      | Some writer_id ->
          reads
          |> List.filter ~f:(is_flow_dependence writer_id definition statement)
          |> List.map ~f:(fun (read : access) -> (read.var, writer_id))
          |> Set.Poly.of_list)

let direct_dependence_ids ?(blockers : string Set.Poly.t = Set.Poly.empty)
    ?(vars : string Set.Poly.t option) (statement : stmt_info) :
    label Set.Poly.t * label Set.Poly.t =
  let writer_ids =
    Set.Poly.filter_map statement.writers ~f:(fun (var, writer_id) ->
        let is_followed =
          (not (Set.Poly.mem var blockers))
          && Option.value_map vars ~default:true ~f:(Set.Poly.mem var) in
        if is_followed then Some writer_id else None) in
  (control_parent_ids statement.controls, writer_ids)

let rec add_transitive_dependence_ids (statements : stmt_info LabelMap.t)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (statement_id : label)
    (visited_ids : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem statement_id visited_ids then visited_ids
  else
    let visited_with_statement_ids = Set.Poly.add statement_id visited_ids in
    let control_ids, writer_ids =
      direct_dependence_ids ~blockers (LabelMap.find statement_id statements)
    in
    Set.Poly.union control_ids writer_ids
    |> Set.Poly.fold ~init:visited_with_statement_ids
         ~f:(add_transitive_dependence_ids statements ~blockers)

let transitive_dependence_ids (statements : stmt_info LabelMap.t)
    (statement_id : label) : label Set.Poly.t =
  add_transitive_dependence_ids statements statement_id Set.Poly.empty

let var_dependence_ids (statements : stmt_info LabelMap.t)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (vars : string Set.Poly.t)
    (statement_id : label) : label Set.Poly.t =
  let control_ids, writer_ids =
    direct_dependence_ids ~blockers ~vars
      (LabelMap.find statement_id statements) in
  Set.Poly.union control_ids writer_ids
  |> Set.Poly.fold ~init:Set.Poly.empty
       ~f:(add_transitive_dependence_ids statements ~blockers)

let used_vars_at (statements : stmt_info LabelMap.t)
    (statement_ids : label Set.Poly.t) : string Set.Poly.t =
  Set.Poly.union_map statement_ids ~f:(fun statement_id ->
      Accesses.used_vars (LabelMap.find statement_id statements).accesses)

let is_loop_var_read (context : Loop_context.t) ~(loop_ids : label list)
    (collected : Expr.Typed.t Accesses.access) : bool =
  match collected.kind with
  | Read ->
      Option.is_some (Loop_context.for_loop_id context ~loop_ids collected.var)
  | Write | ReadWrite -> false

let build_dependence_info (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    stmt_info LabelMap.t =
  let statement_map = build_statement_map stmt in
  let control_ids = build_controls statement_map in
  let collected_accesses =
    LabelMap.map statement_map ~f:(fun (pattern, _) ->
        Accesses.of_statement pattern) in
  let defined_vars =
    LabelMap.fold collected_accesses ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:accesses running_defined_vars ->
        Set.Poly.union running_defined_vars (Accesses.defined_vars accesses))
  in
  let context = Loop_context.create statement_map ~defined_vars in
  let statements_without_definitions =
    LabelMap.mapi statement_map ~f:(fun statement_id (pattern, meta) ->
        let controls = LabelMap.find statement_id control_ids in
        let loop_ids = enclosing_loop_ids statement_id pattern controls in
        let accesses =
          LabelMap.find statement_id collected_accesses
          |> List.filter ~f:(fun collected ->
              not (is_loop_var_read context ~loop_ids collected))
          |> List.map
               ~f:
                 (Accesses.map_scalars
                    ~f:(Access_function.of_expr context ~loop_ids)) in
        { id= statement_id
        ; pattern
        ; meta
        ; controls
        ; loop_ids
        ; accesses
        ; reaching_definitions= Set.Poly.empty
        ; writers= Set.Poly.empty }) in
  let reaching_definitions =
    compute_reaching_definitions mir statements_without_definitions in
  LabelMap.mapi statements_without_definitions ~f:(fun statement_id statement ->
      let with_definitions =
        { statement with
          reaching_definitions= LabelMap.find statement_id reaching_definitions
        } in
      {with_definitions with writers= writers_of_statement with_definitions})

let build_log_prob_dependence_info (mir : Program.Typed.t) :
    stmt_info LabelMap.t =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dependence_info mir log_prob_stmt
