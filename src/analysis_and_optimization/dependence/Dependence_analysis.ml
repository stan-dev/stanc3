(* The interface file describes the analysis, defines the terms (access,
   definition, kill, control, writer and reader) and walks through the running
   example that the unit tests print. *)

open Std
open Middle
open Dataflow_types
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework
open Dependence

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
    wrote, by rule A (every iteration) or rule B (same iteration) below. *)
let is_killed ~(statements : stmt_info LabelMap.t) ~(killer : label)
    (killed : reaching_definition) : bool =
  let killer_statement = LabelMap.find killer statements in
  (* Rule B, same iteration: [killer] writes the element of [killed] in the same
     iteration of the innermost loop around [killed]. [killer] comes later in
     source order and has the same loops. Each control of [killer], and of the
     innermost loop, is also a control of [killed]. The loops are found last,
     after the cheaper checks. A definition from outside has no label and no
     iteration. *)
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
      | Write | ReadWrite ->
          (* Rule A, every iteration: [killer] writes the element of [killed] in
             every iteration. A write of the whole variable has an empty path
             and overwrites every element, including an outside definition. *)
          Dependence.is_overwritten ~is_same_iteration:false access.path
            killed.access.path
          || is_killed_in_same_iteration access.path)

(** The definitions from outside are the input and output variables of [mir].
    Returns the definitions at the entry of each node. *)
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
      | Write | ReadWrite ->
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

(** Whether [write] can supply the value that [read] uses. The two must name one
    variable, [write] must write (a [Write] or a [ReadWrite]), [read] must read
    (a [Read] or a [ReadWrite]), and the write must be able to run before the
    read ({!Dependence.is_lexicographically_positive}). *)
let is_flow_dependence (common_loop_ids : label list) (is_writer_first : bool)
    (write : Accesses.access) (read : Accesses.access) : bool =
  String.equal write.var read.var
  && (match write.kind with Write | ReadWrite -> true | Read -> false)
  && (match read.kind with Read | ReadWrite -> true | Write -> false)
  && Dependence.is_lexicographically_positive common_loop_ids is_writer_first
       write.path read.path

let writers_of_statement (statement : stmt_info) : (string * label) Set.Poly.t =
  let reads =
    List.filter statement.accesses ~f:(fun (access : Accesses.access) ->
        match access.kind with Read | ReadWrite -> true | Write -> false) in
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
          | Write | ReadWrite -> false in
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

(* Printing *)

(** Prints the output of [pp] on one line: each line of the output is trimmed,
    and the lines are joined by one space. *)
let one_line (pp : 'a Fmt.t) : 'a Fmt.t =
 fun ppf x ->
  let buffer = Buffer.create 128 in
  let line_ppf = Format.formatter_of_buffer buffer in
  Format.pp_set_margin line_ppf 1_000_000;
  Fmt.pf line_ppf "%a@?" pp x;
  Buffer.contents buffer |> String.split_lines |> List.map ~f:String.trim
  |> String.concat ~sep:" " |> Fmt.string ppf

let pp_pattern ppf (pattern : (Expr.Typed.t, label) Stmt.Pattern.t) =
  let pp_label ppf label = Fmt.pf ppf "#%d" label in
  let pp_labels = Fmt.(list ~sep:(any " ") pp_label) in
  match pattern with
  | SList labels -> Fmt.pf ppf "SList { %a }" pp_labels labels
  | Block labels -> Fmt.pf ppf "Block { %a }" pp_labels labels
  | _ -> one_line (Stmt.Pattern.pp Expr.Typed.pp pp_label) ppf pattern

let pp_controls ~(loop_name : label -> string) ppf
    ({enclosing; jump_ids} : controls) =
  let pp_enclosing ppf = function
    | {id; side= Some Then} -> Fmt.pf ppf "if %d then" id
    | {id; side= Some Else} -> Fmt.pf ppf "if %d else" id
    | {id; side= None} -> Fmt.pf ppf "loop %d (%s)" id (loop_name id) in
  Fmt.(list ~sep:(any "; ") string)
    ppf
    (List.map enclosing ~f:(Fmt.str "%a" pp_enclosing)
    @ List.map (Set.Poly.to_list jump_ids) ~f:(Fmt.str "jump %d"))

let pp_definitions ~(loop_name : label -> string) ppf
    (definitions : reaching_definition Set.Poly.t) =
  (* The variable and the path of a write, without the kind. *)
  let pp_access ppf ({var; path; _} : Accesses.access) =
    Fmt.pf ppf "%s%a" var (one_line (Accesses.pp_path ~loop_name)) path in
  let entries =
    List.map (Set.Poly.to_list definitions) ~f:(fun definition ->
        (Fmt.str "%a" pp_access definition.access, definition.statement_id))
  in
  (* The distinct access texts with a label, in set order. *)
  let texts =
    List.rev
      (List.fold_left entries ~init:[] ~f:(fun texts (text, label) ->
           match label with
           | Some _ when not (List.mem text ~set:texts) -> text :: texts
           | Some _ | None -> texts)) in
  let inside =
    List.map texts ~f:(fun text ->
        match
          List.filter_map entries ~f:(function
            | entry_text, Some label when String.equal entry_text text ->
                Some label
            | _, Some _ | _, None -> None)
        with
        | [label] -> Fmt.str "%s %d" text label
        | labels -> Fmt.str "%s [%a]" text Fmt.(list ~sep:(any ", ") int) labels)
  in
  (* A set with only definitions from outside the analyzed code prints
     [None]. *)
  if List.is_empty inside && not (Set.Poly.is_empty definitions) then
    Fmt.string ppf "None"
  else Fmt.(list ~sep:(any ", ") string) ppf inside

let pp_dependences ppf (statement : stmt_info) =
  let pp_ids ppf ids =
    Fmt.(list ~sep:(any " ") int) ppf (Set.Poly.to_list ids) in
  let labels_of kind_name labels =
    if Set.Poly.is_empty labels then None
    else Some (Fmt.str "%s %a" kind_name pp_ids labels) in
  let data_ids = Set.Poly.map statement.writers ~f:snd in
  let control_ids = control_parent_ids statement.controls in
  Fmt.(list ~sep:(any "  ") string)
    ppf
    (List.filter_map ~f:Fun.id
       [labels_of "data" data_ids; labels_of "control" control_ids])
