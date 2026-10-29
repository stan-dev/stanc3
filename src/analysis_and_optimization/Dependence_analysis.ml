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
let write_path (accesses : Accesses.access list) (var : string) :
    Accesses.step list option =
  List.find_map accesses ~f:(fun (access : Accesses.access) ->
      match access.kind with
      | Write when String.equal access.var var -> Some access.path
      | Write | Read | Increment -> None)

type statement =
  { pattern: (Expr.Typed.t, label) Stmt.Pattern.t
  ; meta: Location_span.t
  ; controls: controls
  ; accesses: Accesses.access list }

type dependence_info =
  { statements: statement LabelMap.t
  ; reaching_definitions: reaching_definition Set.Poly.t LabelMap.t }

let common_loop_ids (statements : statement LabelMap.t) (first_id : label)
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
    (loop_ids first_id first.pattern first.controls)
    (loop_ids second_id second.pattern second.controls)

(** The [is_killed] callback for reaching definitions, which decides element
    kills. A later write [killer] kills the definition at [killed] when [killer]
    must overwrite the element of [killed]. Two rules give this result. *)
let is_element_killed ~(statements : statement LabelMap.t) ~(killer : label)
    ~(killed : label) (var : string) : bool =
  let killer_statement = LabelMap.find killer statements in
  let killed_statement = LabelMap.find killed statements in
  (* Whether each if, loop and jump that decides if [statement_id] runs also
     decides if [killed] runs. *)
  let is_covered_by_killed (statement_id : label) : bool =
    let covered = (LabelMap.find statement_id statements).controls in
    List.for_all covered.enclosing ~f:(fun enclosing ->
        List.mem enclosing ~set:killed_statement.controls.enclosing)
    && Set.Poly.subset covered.jump_ids killed_statement.controls.jump_ids in
  match
    ( write_path killer_statement.accesses var
    , write_path killed_statement.accesses var )
  with
  | Some killer_path, Some killed_path -> (
      (* Rule A of 2: [killer] writes the element of [killed] in every
         iteration. *)
      Dependence.is_overwritten ~is_same_iteration:false killer_path killed_path
      ||
      (* Rule B of 2: [killer] writes the element of [killed] in the same
         iteration of the innermost loop around [killed]. [killer] comes later
         in source order and has the same loops. Each control of [killer], and
         of the innermost loop, is also a control of [killed]. The loops are
         found last, after the cheaper checks. *)
      killed < killer
      && Dependence.is_overwritten ~is_same_iteration:true killer_path
           killed_path
      &&
      let killed_loop_ids =
        loop_ids killed killed_statement.pattern killed_statement.controls in
      match List.rev killed_loop_ids with
      | innermost :: _ ->
          List.equal ~eq:Int.equal
            (loop_ids killer killer_statement.pattern killer_statement.controls)
            killed_loop_ids
          && is_covered_by_killed killer
          && is_covered_by_killed innermost
      | [] -> false)
  | None, _ | _, None -> false

(** Runs reaching definitions with [is_killed] on the forward flowgraph of
    [statement_map]. Returns the definitions that reach the start of each label.
*)
let compute_reaching_definitions
    ~(is_killed : killer:label -> killed:label -> string -> bool)
    (mir : Program.Typed.t)
    (statement_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t) :
    reaching_definition Set.Poly.t LabelMap.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_statement_map statement_map in
  let (module Flowgraph) = flowgraph in
  LabelMap.map
    (reaching_definitions_mfp ~is_killed mir
       (module Flowgraph)
       flowgraph_to_mir)
    ~f:(fun definitions -> definitions.entry)

let build_dependence_info (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    dependence_info =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let _, _, control_ids = build_control_flow statement_map in
  (* A subscript is affine only if no statement in [stmt] defines the variables
     that the subscript uses, because a defined variable can change between
     iterations. For example, a write [m = n] stops [v[m]] from being affine.
     The defined names do not depend on the classification, so this first pass
     classifies no subscript. *)
  let defined_vars =
    LabelMap.fold statement_map ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:(pattern, _) running_defined_vars ->
        Set.Poly.union running_defined_vars
          (Accesses.defined_vars
             (Accesses.of_statement ~classify:(fun _ -> None) pattern))) in
  let statements =
    LabelMap.mapi statement_map ~f:(fun statement_id (pattern, meta) ->
        let controls = LabelMap.find statement_id control_ids in
        (* The variable and label of each [for] loop around the statement. The
           [for] statement at the label holds the variable. *)
        let for_loops =
          List.filter_map (loop_ids statement_id pattern controls)
            ~f:(fun loop_id ->
              match fst (LabelMap.find loop_id statement_map) with
              | For {loopvar; _} -> Some (loopvar, loop_id)
              | _ -> None) in
        (* A query reaches each enclosing loop, and the bounds of that loop,
           through the control dependence on the loop. No statement writes a
           loop variable, so a search for writes to a loop variable finds
           nothing. The reads of loop variables thus add no information and are
           dropped. *)
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
  (* The kills use the classified accesses, so classification comes first. *)
  let reaching_definitions =
    compute_reaching_definitions
      ~is_killed:(is_element_killed ~statements)
      mir statement_map in
  {statements; reaching_definitions}

let definition_ids (definitions : reaching_definition Set.Poly.t) (var : string)
    : label option Set.Poly.t =
  Set.Poly.map
    (Set.Poly.filter definitions ~f:(fun (defined, _) ->
         String.equal defined var))
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
    {!Dependence} keeps. A candidate stays if the source does not define [var],
    if the sink does not use [var], or if some access of [var] in the source and
    some access in the sink pass {!is_flow_dependence}. A definition from
    outside the analyzed statement has no label and is left out. *)
let source_ids (info : dependence_info) (sink_id : label) (var : string) :
    label Set.Poly.t =
  let sink = LabelMap.find sink_id info.statements in
  let sink_accesses = Accesses.filter_var var sink.accesses in
  let candidate_definitions =
    definition_ids (LabelMap.find sink_id info.reaching_definitions) var in
  let is_sink_reading =
    List.exists sink_accesses ~f:(fun access ->
        match access.Accesses.kind with
        | Read | Increment -> true
        | Write -> false) in
  let is_possible_source source_id =
    let source = LabelMap.find source_id info.statements in
    let source_accesses = Accesses.filter_var var source.accesses in
    let is_source_writing =
      List.exists source_accesses ~f:(fun access ->
          match access.Accesses.kind with
          | Write | Increment -> true
          | Read -> false) in
    (not is_source_writing) || (not is_sink_reading)
    || List.exists source_accesses ~f:(fun source ->
        List.exists sink_accesses ~f:(fun sink ->
            is_flow_dependence info (source_id, source) (sink_id, sink))) in
  Set.Poly.filter_map candidate_definitions ~f:(function
    | None -> None
    | Some source_id ->
        if is_possible_source source_id then Some source_id else None)

let direct_dependence_ids (info : dependence_info)
    ?(blockers : string Set.Poly.t = Set.Poly.empty)
    ?(vars : string Set.Poly.t option) (statement_id : label) : label Set.Poly.t
    =
  let statement = LabelMap.find statement_id info.statements in
  let vars =
    Option.value vars ~default:(Accesses.used_vars statement.accesses) in
  let data_dependence_ids =
    Set.Poly.union_map
      (Set.Poly.diff vars blockers)
      ~f:(source_ids info statement_id) in
  Set.Poly.union (control_parent_ids statement.controls) data_dependence_ids

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
    let dependence_ids = direct_dependence_ids info ~blockers statement_id in
    Set.Poly.fold dependence_ids ~init:visited_with_statement_ids
      ~f:(add_transitive_dependence_ids info ~blockers)

let transitive_dependence_ids (info : dependence_info) (statement_id : label) :
    label Set.Poly.t =
  add_transitive_dependence_ids info statement_id Set.Poly.empty

let var_dependence_ids (info : dependence_info)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (vars : string Set.Poly.t)
    (statement_id : label) : label Set.Poly.t =
  Set.Poly.fold
    (direct_dependence_ids info ~blockers ~vars statement_id)
    ~init:Set.Poly.empty
    ~f:(add_transitive_dependence_ids info ~blockers)

let used_vars_at (info : dependence_info) (statement_ids : label Set.Poly.t) :
    string Set.Poly.t =
  Set.Poly.union_map statement_ids ~f:(fun statement_id ->
      Accesses.used_vars (LabelMap.find statement_id info.statements).accesses)

let dependence_graph (info : dependence_info) : label Set.Poly.t LabelMap.t =
  let immediate_map =
    LabelMap.mapi info.statements ~f:(fun label _ ->
        direct_dependence_ids info label) in
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

let build_log_prob_dependence_info (mir : Program.Typed.t) : dependence_info =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dependence_info mir log_prob_stmt
