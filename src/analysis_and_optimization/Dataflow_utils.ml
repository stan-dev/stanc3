open Std
open Middle
open Dataflow_types
open Mir_utils

(** Merge two maps whose values are sets, and union the sets when there's a
    collision. *)
let merge_set_maps (module M : Map.S) m1 m2 =
  let merge_map_elems _ e1 e2 = Some (Set.Poly.union e1 e2) in
  M.union ~f:merge_map_elems m1 m2

(** See interface file *)
let build_statement_map extract metadata stmt =
  let rec build_statement_map_rec next_label map stmt =
    let this_label = next_label in
    let next_label' = next_label + 1 in
    let f (label, map) stmt = build_statement_map_rec label map stmt in
    let (next_label'', map), built =
      fwd_traverse_statement (extract stmt) ~init:(next_label', map) ~f in
    ( ( next_label''
      , LabelMap.add map ~key:this_label ~data:(built, metadata stmt) )
    , this_label ) in
  let (_, map), _ = build_statement_map_rec 1 LabelMap.empty stmt in
  map

(* TODO: this currently does not seem to be labelling inside function bodies.
   Could we also do that? *)

(** See interface file *)
let rec build_recursive_statement rebuild statement_map label =
  let stmt_ints, meta = LabelMap.find label statement_map in
  let build_stmt = build_recursive_statement rebuild statement_map in
  let stmt = Stmt.Pattern.map Fun.id build_stmt stmt_ints in
  rebuild stmt meta

(** Represents the state required to build control flow information during an
    MIR traversal, where
    - breaks is the set of Breaks seen since the beginning of their loop
    - continues is the set of Continues seen since the beginning of their loop
    - returns is the set of Returns seen since the beginning of their function
      definition
    - exits is the set of nodes that could have been the last one to execute
      before this node *)
type cf_state =
  { breaks: label Set.Poly.t
  ; continues: label Set.Poly.t
  ; returns: label Set.Poly.t
  ; exits: label Set.Poly.t }

(** Represents the control flow information at each node in the control graph,
    where
    - predecessors points to the nodes which could have executed before this
      node
    - parents points to the adjacent nodes which directly influence the
      execution of this node *)
type cf_edges = {predecessors: label Set.Poly.t; parents: label Set.Poly.t}

(** Join the state of a controlflow traversal across different branches of
    execution such as over if/else branch. *)
let join_cf_states (state1 : cf_state) (state2 : cf_state) : cf_state =
  { breaks= Set.Poly.union state1.breaks state2.breaks
  ; continues= Set.Poly.union state1.continues state2.continues
  ; returns= Set.Poly.union state1.returns state2.returns
  ; exits= Set.Poly.union state1.exits state2.exits }

(** Check if the statement controls the execution of its substatements. *)
let is_ctrl_flow pattern =
  match pattern with
  | Stmt.Pattern.IfElse _ -> true
  | While _ -> true
  | For _ -> true
  | _ -> false

(** Simultaneously builds the controlflow parent graph, the predecessor graph
    and the exit set of a statement. It's advantageous to build them together
    because they both rely on some of the same Break, Continue and Return
    bookkeeping. *)
let build_cf_graphs ?(flatten_loops = false) ?(blocks_after_body = true)
    statement_map =
  let rec build_cf_graph_rec (cf_parent : label option)
      ((in_state, in_map) : cf_state * cf_edges LabelMap.t) (label : label) :
      cf_state * cf_edges LabelMap.t =
    let stmt, _ = LabelMap.find label statement_map in
    (* Only control flow nodes should pass themselves down as parents *)
    let child_cf = if is_ctrl_flow stmt then Some label else cf_parent in
    (* This node is the parent of substatements, unless this is a Block, which
       is visited after substatements *)
    let substmt_preds =
      match stmt with
      | (Block _ | Profile _) when blocks_after_body -> in_state.exits
      | _ -> Set.Poly.singleton label in
    (* The accumulated state after traversing substatements *)
    let child_init = {in_state with exits= substmt_preds} in
    let substmt_state_unlooped, substmt_map =
      match stmt with
      | IfElse (_, then_s, else_s_opt) ->
          let then_state, after_then_map =
            build_cf_graph_rec child_cf (child_init, in_map) then_s in
          Option.value_map else_s_opt
            ~default:(join_cf_states then_state child_init, after_then_map)
            ~f:(fun else_s ->
              (* The control-flow state starts from the same point on both
                 branches. The graph map is only an accumulator of uniquely
                 labelled nodes, so carry it through the branches instead of
                 persistently unioning two maps that share the whole prefix. *)
              let else_state, after_both_map =
                build_cf_graph_rec child_cf (child_init, after_then_map) else_s
              in
              (join_cf_states then_state else_state, after_both_map))
      | _ ->
          fst
            (fwd_traverse_statement stmt ~init:(child_init, in_map)
               ~f:(fun state child ->
                 (build_cf_graph_rec child_cf state child, ()))) in
    (* If the statement is a loop, we need to include the loop body exits as
       predecessors of the loop *)
    let substmt_state, predecessors =
      match stmt with
      | For _ | While _ ->
          (* Loop statements are preceded by:

             1. The statements that come before the loop

             2. The natural exit points of the loop body

             3. Continue statements in the loop body This comment mangling
             brought to you by the autoformatter *)
          let loop_predecessors =
            Set.Poly.union_list
              [ (*1*) in_state.exits; (*2*) substmt_state_unlooped.exits; (*3*)
                Set.Poly.diff substmt_state_unlooped.continues
                  in_state.continues ] in
          (* Loop exits are:

             1. The loop node itself, since the last action of a typical loop
             execution is to check if there are any iterations remaining

             2. Break statements in the loop body, since broken loops don't
             execute the loop statement *)
          let loop_exits =
            if flatten_loops then substmt_state_unlooped.exits
            else
              Set.Poly.union_list
                [ (*1*) Set.Poly.singleton label; (*2*)
                  Set.Poly.diff substmt_state_unlooped.breaks in_state.breaks ]
          in
          ({substmt_state_unlooped with exits= loop_exits}, loop_predecessors)
      | (Block _ | Profile _) when blocks_after_body ->
          (* Block statements are preceded by the natural exit points of the
             block body *)
          let block_predecessors = substmt_state_unlooped.exits in
          (* Block exits are just the block node *)
          let block_exits = Set.Poly.singleton label in
          ({substmt_state_unlooped with exits= block_exits}, block_predecessors)
      | _ -> (substmt_state_unlooped, in_state.exits) in
    (* Some statements interact with the break/return/continue states E.g.,
       loops nullify breaks and continues in their body, but are still affected
       by breaks and input continues *)
    let breaks_out, returns_out, continues_out, extra_cf_deps =
      match stmt with
      | Break ->
          ( Set.Poly.add label substmt_state.breaks
          , substmt_state.returns
          , substmt_state.continues
          , Set.Poly.empty )
      | Return _ ->
          ( substmt_state.breaks
          , Set.Poly.add label substmt_state.returns
          , substmt_state.continues
          , Set.Poly.empty )
      | Continue ->
          ( substmt_state.breaks
          , substmt_state.returns
          , Set.Poly.add label substmt_state.continues
          , Set.Poly.empty )
      | While _ | For _ ->
          ( in_state.breaks
          , substmt_state.returns
          , in_state.continues
          , Set.Poly.union substmt_state.breaks substmt_state.returns )
      | _ ->
          ( substmt_state.breaks
          , substmt_state.returns
          , substmt_state.continues
          , Set.Poly.empty ) in
    let cf_parents =
      Set.Poly.union_list
        [ Option.value_map cf_parent ~default:Set.Poly.empty
            ~f:Set.Poly.singleton; in_state.returns; in_state.continues
        ; extra_cf_deps ] in
    ( { breaks= breaks_out
      ; continues= continues_out
      ; returns= returns_out
      ; exits= substmt_state.exits }
    , LabelMap.add substmt_map ~key:label
        ~data:{parents= cf_parents; predecessors} ) in
  let state, edges =
    build_cf_graph_rec None
      ( { breaks= Set.Poly.empty
        ; continues= Set.Poly.empty
        ; returns= Set.Poly.empty
        ; exits= Set.Poly.empty }
      , LabelMap.empty )
      1 in
  ( state.exits
  , LabelMap.map edges ~f:(fun e -> e.predecessors)
  , LabelMap.map edges ~f:(fun e -> e.parents) )

(** See interface file *)
let build_cf_graph statement_map =
  let _, _, cf_graph = build_cf_graphs statement_map in
  cf_graph

(** See interface file *)
let build_predecessor_graph ?(flatten_loops = false) ?(blocks_after_body = true)
    statement_map =
  let exits, pred_graph, _ =
    build_cf_graphs ~flatten_loops ~blocks_after_body statement_map in
  (exits, pred_graph)
