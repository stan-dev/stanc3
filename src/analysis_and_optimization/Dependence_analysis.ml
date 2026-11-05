(* The interface file describes the analysis and the running example. This file
   uses the terms below.

   An access is one read, write or increment of a variable. The path of an
   access lists the subscripts and tuple fields that select an element of the
   variable ({!Accesses}).

   A definition records a write as the access of the write, the label of the
   writing statement and the loops around that statement. A definition reaches a
   statement if the program can go from the write to the statement with no write
   between them that overwrites the same element. Reaching definitions
   ({!Monotone_framework.reaching_definitions_mfp}) finds these definitions.

   A later write kills an earlier definition when the later write overwrites
   each element that the earlier write wrote. The killed definition no longer
   reaches later statements. An element kill compares the paths of two writes to
   single elements, such as [theta[n, 1]], to decide this.

   The controls of a statement are the ifs and loops around the statement, and
   the jumps ([break], [continue] and [return]) that can skip the statement
   ({!Dataflow_types.controls}).

   Each reaching definition of a variable that a statement reads is a candidate
   writer for the read, and the statement that reads is the reader.
   {!Dependence} rejects a candidate writer that can never supply the value that
   the reader uses. *)

open Std
open Middle
open Dataflow_types
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

type reaching_definition =
  {access: Accesses.access; statement_id: label option; loop_ids: label list}

type stmt_info =
  { id: label
  ; pattern: (Expr.Typed.t, label) Stmt.Pattern.t
  ; meta: Location_span.t
  ; controls: controls
  ; loop_ids: label list
  ; accesses: Accesses.t
  ; reaching_definitions: reaching_definition Set.Poly.t
  ; writers: (string * label) Set.Poly.t }

(** The [is_killed] callback for reaching definitions. A write at [killer] kills
    a definition when the write must overwrite the element that the definition
    wrote, by one of two rules. An increment of a variable kills every earlier
    definition of the variable, so the definition of [target] is always the
    latest increment. *)
let is_killed ~(statements : stmt_info LabelMap.t) ~(killer : label)
    (killed : reaching_definition) : bool =
  let killer_statement = LabelMap.find killer statements in
  (* Rule B of 2: [killer] writes the element of [killed] in the same iteration
     of the innermost loop around [killed]. [killer] comes later in source order
     and has the same loops. Each control of [killer], and of the innermost
     loop, is also a control of [killed]. The loops are found last, after the
     cheaper checks. A definition from outside has no label and no iteration. *)
  let is_killed_in_same_iteration
      (killer_path : Affine_subscript.t Accesses.step list) : bool =
    match killed.statement_id with
    | None -> false
    | Some killed_id -> (
        let killed_statement = LabelMap.find killed_id statements in
        (* Whether each if, loop and jump that decides if [statement_id] runs
           also decides if [killed] runs. *)
        let is_covered_by_killed (statement_id : label) : bool =
          let covered = (LabelMap.find statement_id statements).controls in
          List.for_all covered.enclosing ~f:(fun enclosing ->
              List.mem enclosing ~set:killed_statement.controls.enclosing)
          && Set.Poly.subset covered.jump_ids killed_statement.controls.jump_ids
        in
        killed_id < killer
        && Dependence.is_overwritten ~is_same_iteration:true killer_path
             killed.access.path
        &&
        match List.rev killed.loop_ids with
        | innermost :: _ ->
            List.equal ~eq:Int.equal killer_statement.loop_ids killed.loop_ids
            && is_covered_by_killed killer
            && is_covered_by_killed innermost
        | [] -> false) in
  List.exists killer_statement.accesses ~f:(fun (access : Accesses.access) ->
      String.equal access.var killed.access.var
      &&
      match access.kind with
      | Read -> false
      | Increment -> true
      | Write ->
          (* Rule A of 2: [killer] writes the element of [killed] in every
             iteration. A write of the whole variable has an empty path and
             overwrites every element, including an outside definition. *)
          Dependence.is_overwritten ~is_same_iteration:false access.path
            killed.access.path
          || is_killed_in_same_iteration access.path)

(** Runs reaching definitions on the forward flowgraph of [statements]. The
    definitions from outside are the input and output variables of [mir], each
    statement adds a definition for each write or increment in the accesses of
    the statement, and {!is_killed} removes definitions. Returns the definitions
    that reach the node of each label. *)
let compute_reaching_definitions (mir : Program.Typed.t)
    (statements : stmt_info LabelMap.t) :
    reaching_definition Set.Poly.t LabelMap.t =
  let flowgraph, _ =
    statements
    |> LabelMap.map ~f:(fun {pattern; meta; _} -> (pattern, meta))
    |> Monotone_framework.forward_flowgraph_of_statement_map in
  let (module Flowgraph) = flowgraph in
  let outside_definition (name, _, _) : reaching_definition =
    { access= {var= name; path= []; kind= Write}
    ; statement_id= None
    ; loop_ids= [] } in
  let outside_definitions =
    Set.Poly.of_list
      (List.map mir.input_vars ~f:outside_definition
      @ List.map mir.output_vars ~f:outside_definition) in
  let definitions_of (statement_id : label) : reaching_definition Set.Poly.t =
    let statement = LabelMap.find statement_id statements in
    let definition_of_access (access : Accesses.access) =
      match access.kind with
      | Write | Increment ->
          Some
            { access
            ; statement_id= Some statement.id
            ; loop_ids= statement.loop_ids }
      | Read -> None in
    statement.accesses
    |> List.filter_map ~f:definition_of_access
    |> Set.Poly.of_list in
  let definitions_at_entry_and_exit =
    reaching_definitions_mfp ~initial:outside_definitions ~gen:definitions_of
      ~is_killed:(is_killed ~statements)
      (module Flowgraph) in
  LabelMap.map definitions_at_entry_and_exit ~f:(fun definitions ->
      definitions.entry)

let is_flow_dependence (common_loop_ids : label list) (is_writer_first : bool)
    (write : Accesses.access) (read : Accesses.access) : bool =
  String.equal write.var read.var
  && (match (write.kind, read.kind) with
    | (Write | Increment), Read | Write, Increment -> true
    | Read, _ | Increment, Increment | _, Write -> false)
  && Dependence.is_lexicographically_positive common_loop_ids is_writer_first
       write.path read.path

let writers_of_statement (statement : stmt_info) : (string * label) Set.Poly.t =
  let reads =
    List.filter statement.accesses ~f:(fun (access : Accesses.access) ->
        match access.kind with Read | Increment -> true | Write -> false) in
  Set.Poly.union_map statement.reaching_definitions ~f:(fun definition ->
      match definition.statement_id with
      | None -> Set.Poly.empty
      | Some writer_id ->
          let common_loop_ids =
            List.filter definition.loop_ids ~f:(fun loop_id ->
                List.mem loop_id ~set:statement.loop_ids) in
          let is_writer_first = writer_id < statement.id in
          let filter_flow_dependence =
            is_flow_dependence common_loop_ids is_writer_first definition.access
          in
          reads
          |> List.filter ~f:filter_flow_dependence
          |> List.map ~f:(fun (read : Accesses.access) -> (read.var, writer_id))
          |> Set.Poly.of_list)

(** Returns the labels of the direct dependences of the statement, the control
    parents first and the writers of the variables the statement reads second.
    [vars], when given, keeps only the writers of those variables, and the
    writers of the variables in [blockers] are left out. *)
let direct_dependence_ids ?(blockers : string Set.Poly.t = Set.Poly.empty)
    ?(vars : string Set.Poly.t option) (statement : stmt_info) :
    label Set.Poly.t * label Set.Poly.t =
  let is_followed var =
    (not (Set.Poly.mem var blockers))
    && Option.value_map vars ~default:true ~f:(Set.Poly.mem var) in
  let writer_ids =
    Set.Poly.filter_map statement.writers ~f:(fun (var, writer_id) ->
        if is_followed var then Some writer_id else None) in
  (control_parent_ids statement.controls, writer_ids)

(** Adds [statement_id] and the direct and transitive dependences of
    [statement_id] to [visited_ids], without following the definitions of
    [blockers]. *)
let rec add_transitive_dependence_ids (statements : stmt_info LabelMap.t)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (statement_id : label)
    (visited_ids : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem statement_id visited_ids then visited_ids
  else
    (* The visited set is the result and includes the start label. Each
       dependence is a label, so the walk looks up the record at that label. *)
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

let build_dependence_info (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    stmt_info LabelMap.t =
  let statement_map = build_statement_map stmt in
  let control_ids = build_controls statement_map in
  let add_defined_vars ~key:_ ~data:(pattern, _) running_defined_vars =
    pattern
    |> Accesses.of_statement ~classify:(fun _ -> None)
    |> Accesses.defined_vars
    |> Set.Poly.union running_defined_vars in
  let defined_vars =
    LabelMap.fold statement_map ~init:Set.Poly.empty ~f:add_defined_vars in
  let statements_without_definitions =
    LabelMap.mapi statement_map ~f:(fun statement_id (pattern, meta) ->
        let controls = LabelMap.find statement_id control_ids in
        let loopvar_and_id loop_id =
          match fst (LabelMap.find loop_id statement_map) with
          | For {loopvar; _} -> Some (loopvar, loop_id)
          | _ -> None in
        let loop_ids = enclosing_loop_ids statement_id pattern controls in
        let for_loops = List.filter_map loop_ids ~f:loopvar_and_id in
        let is_loop_var_read (access : Accesses.access) =
          match access.kind with
          | Read ->
              List.exists for_loops ~f:(fun (loopvar, _) ->
                  String.equal loopvar access.var)
          | Write | Increment -> false in
        let accesses =
          pattern
          |> Accesses.of_statement
               ~classify:(Affine_subscript.of_expr ~for_loops ~defined_vars)
          |> List.filter ~f:(fun access -> not (is_loop_var_read access)) in
        { id= statement_id
        ; pattern
        ; meta
        ; controls
        ; loop_ids
        ; accesses
        ; reaching_definitions= Set.Poly.empty
        ; writers= Set.Poly.empty }) in
  (* The definitions and the kills use the classified accesses, so the records
     come first, with empty definition and writer sets. The dataflow fills the
     definitions, and the writers come from the definitions. *)
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
