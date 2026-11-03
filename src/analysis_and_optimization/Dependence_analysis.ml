(* The interface file describes the analysis and the running example. This file
   uses the terms below.

   An access is one read, write or increment of a variable. The path of an
   access lists the subscripts and tuple fields that select an element of the
   variable ({!Accesses}).

   A definition records a write as the access of the write and the label of the
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

type reaching_definition = Accesses.access * label option

type stmt_info =
  { pattern: (Expr.Typed.t, label) Stmt.Pattern.t
  ; meta: Location_span.t
  ; controls: controls
  ; accesses: Accesses.t }

type dependence_info =
  { statements: stmt_info LabelMap.t
  ; reaching_definitions: reaching_definition Set.Poly.t LabelMap.t }

let common_loop_ids (statements : stmt_info LabelMap.t) (first_id : label)
    (second_id : label) : label list =
  let first = LabelMap.find first_id statements in
  let second = LabelMap.find second_id statements in
  (* Loops nest, so the loops around both statements are the labels at the start
     of both lists. After the first label that differs, no loop is around both
     statements. *)
  let rec common_prefix first_loop_ids second_loop_ids =
    match (first_loop_ids, second_loop_ids) with
    | first_loop_id :: first_rest, second_loop_id :: second_rest
      when first_loop_id = second_loop_id ->
        first_loop_id :: common_prefix first_rest second_rest
    | _ -> [] in
  common_prefix
    (enclosing_loop_ids first_id first.pattern first.controls)
    (enclosing_loop_ids second_id second.pattern second.controls)

(** The [is_killed] callback for reaching definitions. A write at [killer] kills
    a definition when the write must overwrite the element that the definition
    wrote, by one of two rules. An increment of a variable kills every earlier
    definition of the variable, so the definition of [target] is always the
    latest increment. *)
let is_killed ~(statements : stmt_info LabelMap.t) ~(killer : label)
    ((killed_access, killed_label) : reaching_definition) : bool =
  let killer_statement = LabelMap.find killer statements in
  (* Rule B of 2: [killer] writes the element of [killed] in the same iteration
     of the innermost loop around [killed]. [killer] comes later in source order
     and has the same loops. Each control of [killer], and of the innermost
     loop, is also a control of [killed]. The loops are found last, after the
     cheaper checks. A definition from outside has no label and no iteration. *)
  let is_killed_in_same_iteration
      (killer_path : Affine_subscript.t Accesses.step list) : bool =
    match killed_label with
    | None -> false
    | Some killed -> (
        let killed_statement = LabelMap.find killed statements in
        (* Whether each if, loop and jump that decides if [statement_id] runs
           also decides if [killed] runs. *)
        let is_covered_by_killed (statement_id : label) : bool =
          let covered = (LabelMap.find statement_id statements).controls in
          List.for_all covered.enclosing ~f:(fun enclosing ->
              List.mem enclosing ~set:killed_statement.controls.enclosing)
          && Set.Poly.subset covered.jump_ids killed_statement.controls.jump_ids
        in
        killed < killer
        && Dependence.is_overwritten ~is_same_iteration:true killer_path
             killed_access.path
        &&
        let killed_loop_ids =
          enclosing_loop_ids killed killed_statement.pattern
            killed_statement.controls in
        match List.rev killed_loop_ids with
        | innermost :: _ ->
            List.equal ~eq:Int.equal
              (enclosing_loop_ids killer killer_statement.pattern
                 killer_statement.controls)
              killed_loop_ids
            && is_covered_by_killed killer
            && is_covered_by_killed innermost
        | [] -> false) in
  List.exists killer_statement.accesses ~f:(fun (access : Accesses.access) ->
      String.equal access.var killed_access.var
      &&
      match access.kind with
      | Read -> false
      | Increment -> true
      | Write ->
          (* Rule A of 2: [killer] writes the element of [killed] in every
             iteration. A write of the whole variable has an empty path and
             overwrites every element, including an outside definition. *)
          Dependence.is_overwritten ~is_same_iteration:false access.path
            killed_access.path
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
    Monotone_framework.forward_flowgraph_of_statement_map
      (LabelMap.map statements ~f:(fun {pattern; meta; _} -> (pattern, meta)))
  in
  let (module Flowgraph) = flowgraph in
  let outside_definition (name, _, _) : reaching_definition =
    ({var= name; path= []; kind= Write}, None) in
  let outside_definitions =
    Set.Poly.of_list
      (List.map mir.input_vars ~f:outside_definition
      @ List.map mir.output_vars ~f:outside_definition) in
  let definitions_of (statement_id : label) : reaching_definition Set.Poly.t =
    Set.Poly.of_list
      (List.filter_map (LabelMap.find statement_id statements).accesses
         ~f:(fun (access : Accesses.access) ->
           match access.kind with
           | Write | Increment -> Some (access, Some statement_id)
           | Read -> None)) in
  LabelMap.map
    (reaching_definitions_mfp ~initial:outside_definitions ~gen:definitions_of
       ~is_killed:(is_killed ~statements)
       (module Flowgraph))
    ~f:(fun definitions -> definitions.entry)

let definition_ids (definitions : reaching_definition Set.Poly.t) (var : string)
    : label option Set.Poly.t =
  Set.Poly.map
    (Set.Poly.filter definitions ~f:(fun ((access : Accesses.access), _) ->
         String.equal access.var var))
    ~f:snd

let is_flow_kind (source : Accesses.access) (sink : Accesses.access) : bool =
  String.equal source.var sink.var
  &&
  match (source.kind, sink.kind) with
  | (Write | Increment), Read | Write, Increment -> true
  | Read, _ | Increment, Increment | _, Write -> false

let dependence_between (info : dependence_info)
    ((source_id, source) : label * Accesses.access)
    ((sink_id, sink) : label * Accesses.access) : Dependence.t =
  Dependence.test_dependence
    ~common_loop_ids:(common_loop_ids info.statements source_id sink_id)
    source sink

let is_flow_dependence (info : dependence_info)
    ((source_id, source_access) : label * Accesses.access)
    ((sink_id, sink_access) : label * Accesses.access) : bool =
  is_flow_kind source_access sink_access
  (* In one statement, the read runs before the write. *)
  && Dependence.is_lexicographically_positive
       ~is_loop_independent_possible:(source_id < sink_id)
       (dependence_between info (source_id, source_access) (sink_id, sink_access))

(** Returns the labels of the candidate sources of [var] at [sink_id] that
    {!Dependence} keeps. A candidate is a reaching definition of [var] at the
    sink. The candidate stays if the sink does not use [var], or if the write of
    the candidate and some access of [var] in the sink pass
    {!is_flow_dependence}. A definition from outside the analyzed statement has
    no label and is left out. *)
let source_ids (info : dependence_info) (sink_id : label) (var : string) :
    label Set.Poly.t =
  let sink = LabelMap.find sink_id info.statements in
  let sink_accesses = Accesses.filter_var var sink.accesses in
  let is_sink_reading =
    List.exists sink_accesses ~f:(fun access ->
        match access.Accesses.kind with
        | Read | Increment -> true
        | Write -> false) in
  Set.Poly.filter_map (LabelMap.find sink_id info.reaching_definitions)
    ~f:(fun ((source, source_label) : reaching_definition) ->
      match source_label with
      | Some source_id
        when String.equal source.var var
             && ((not is_sink_reading)
                || List.exists sink_accesses ~f:(fun sink ->
                    is_flow_dependence info (source_id, source) (sink_id, sink))
                ) ->
          Some source_id
      | Some _ | None -> None)

type kind = Data of label | Control of label

let direct_dependence_ids (info : dependence_info)
    ?(blockers : string Set.Poly.t = Set.Poly.empty)
    ?(vars : string Set.Poly.t option) (statement_id : label) : kind Set.Poly.t
    =
  let statement = LabelMap.find statement_id info.statements in
  let vars =
    Option.value vars ~default:(Accesses.used_vars statement.accesses) in
  let data_dependence_ids =
    Set.Poly.union_map
      (Set.Poly.diff vars blockers)
      ~f:(source_ids info statement_id) in
  Set.Poly.union
    (Set.Poly.map (control_parent_ids statement.controls) ~f:(fun id ->
         Control id))
    (Set.Poly.map data_dependence_ids ~f:(fun id -> Data id))

(** The label of a direct dependence, for the queries that follow dependences of
    either kind. *)
let label_of_kind : kind -> label = function Data id | Control id -> id

(** Adds [statement_id] and the direct and transitive dependences of
    [statement_id] to [visited_ids], without following the definitions of
    [blockers]. *)
let rec add_transitive_dependence_ids (info : dependence_info)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (statement_id : label)
    (visited_ids : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem statement_id visited_ids then visited_ids
  else
    (* The visited set is the result and includes the start label. *)
    let visited_with_statement_ids = Set.Poly.add statement_id visited_ids in
    let dependence_ids =
      Set.Poly.map
        (direct_dependence_ids info ~blockers statement_id)
        ~f:label_of_kind in
    Set.Poly.fold dependence_ids ~init:visited_with_statement_ids
      ~f:(add_transitive_dependence_ids info ~blockers)

let transitive_dependence_ids (info : dependence_info) (statement_id : label) :
    label Set.Poly.t =
  add_transitive_dependence_ids info statement_id Set.Poly.empty

let var_dependence_ids (info : dependence_info)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (vars : string Set.Poly.t)
    (statement_id : label) : label Set.Poly.t =
  Set.Poly.fold
    (Set.Poly.map
       (direct_dependence_ids info ~blockers ~vars statement_id)
       ~f:label_of_kind)
    ~init:Set.Poly.empty
    ~f:(add_transitive_dependence_ids info ~blockers)

let used_vars_at (info : dependence_info) (statement_ids : label Set.Poly.t) :
    string Set.Poly.t =
  Set.Poly.union_map statement_ids ~f:(fun statement_id ->
      Accesses.used_vars (LabelMap.find statement_id info.statements).accesses)

let dependence_graph (info : dependence_info) : label Set.Poly.t LabelMap.t =
  let immediate_map =
    LabelMap.mapi info.statements ~f:(fun label _ ->
        Set.Poly.map (direct_dependence_ids info label) ~f:label_of_kind) in
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

let build_dependence_info (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    dependence_info =
  let statement_map = build_statement_map stmt in
  let control_ids = build_controls statement_map in
  let defined_vars =
    LabelMap.fold statement_map ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:(pattern, _) running_defined_vars ->
        Set.Poly.union running_defined_vars
          (Accesses.defined_vars
             (Accesses.of_statement ~classify:(fun _ -> None) pattern))) in
  let statements =
    LabelMap.mapi statement_map ~f:(fun statement_id (pattern, meta) ->
        let controls = LabelMap.find statement_id control_ids in
        let for_loops =
          List.filter_map (enclosing_loop_ids statement_id pattern controls)
            ~f:(fun loop_id ->
              match fst (LabelMap.find loop_id statement_map) with
              | For {loopvar; _} -> Some (loopvar, loop_id)
              | _ -> None) in
        let is_loop_var_read (access : Accesses.access) =
          match access.kind with
          | Read ->
              List.exists for_loops ~f:(fun (loopvar, _) ->
                  String.equal loopvar access.var)
          | Write | Increment -> false in
        let accesses =
          List.filter
            (Accesses.of_statement
               ~classify:(Affine_subscript.of_expr ~for_loops ~defined_vars)
               pattern)
            ~f:(fun access -> not (is_loop_var_read access)) in
        {pattern; meta; controls; accesses}) in
  (* The definitions and the kills use the classified accesses, so
     classification comes first. *)
  let reaching_definitions = compute_reaching_definitions mir statements in
  {statements; reaching_definitions}

let build_log_prob_dependence_info (mir : Program.Typed.t) : dependence_info =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dependence_info mir log_prob_stmt
