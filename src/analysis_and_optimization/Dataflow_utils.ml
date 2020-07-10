open Core_kernel
open Middle
open Dataflow_types
open Mir_utils

(** Union maps, preserving the left element in a collision *)
let union_maps_left (m1 : ('a, 'b) Map.Poly.t) (m2 : ('a, 'b) Map.Poly.t) :
    ('a, 'b) Map.Poly.t =
  let f ~key:_ opt =
    match opt with
    | `Left v -> Some v
    | `Right v -> Some v
    | `Both (v1, _) -> Some v1
  in
  Map.Poly.merge m1 m2 ~f

(**
   Merge two maps whose values are sets, and union the sets when there's a collision.
*)
let merge_set_maps m1 m2 =
  let merge_map_elems ~key:_ es =
    match es with
    | `Left e1 -> Some e1
    | `Right e2 -> Some e2
    | `Both (e1, e2) -> Some (Set.Poly.union e1 e2)
  in
  Map.Poly.merge ~f:merge_map_elems m1 m2

(**
   Generate a Map by applying a function to each element of a key set.
*)
let generate_map s ~f =
  Set.Poly.fold s ~init:Map.Poly.empty ~f:(fun m e ->
      Map.Poly.add_exn m ~key:e ~data:(f e) )

(**
   Like a forward traversal, but branches accumulate two different states that are
   recombined with join.
*)
let branching_traverse_statement stmt ~join ~init ~f =
  Stmt.Fixed.Pattern.(
    match stmt with
    | IfElse (pred, then_s, else_s_opt) ->
        let s', c = f init then_s in
        Option.value_map else_s_opt
          ~default:(join s' init, IfElse (pred, c, None))
          ~f:(fun else_s ->
            let s'', c' = f init else_s in
            (join s' s'', IfElse (pred, c, Some c')) )
    | _ as s -> fwd_traverse_statement s ~init ~f)

(** Like a branching traversal, but doesn't return an updated statement.
*)
let branching_fold_statement stmt ~join ~init ~f =
  fst
    (branching_traverse_statement stmt ~join ~init ~f:(fun s a -> (f s a, ())))

(**
   See interface file
*)
let build_statement_map extract metadata stmt =
  let rec build_statement_map_rec next_label map stmt =
    let this_label = next_label in
    let next_label' = next_label + 1 in
    let f (label, map) stmt = build_statement_map_rec label map stmt in
    let (next_label'', map), built =
      fwd_traverse_statement (extract stmt) ~init:(next_label', map) ~f
    in
    ( ( next_label''
      , union_maps_left map
          (Map.Poly.singleton this_label (built, metadata stmt)) )
    , this_label )
  in
  let (_, map), _ = build_statement_map_rec 1 Map.Poly.empty stmt in
  map

(* TODO: this currently does not seem to be labelling inside function bodies.
   Could we also do that? *)

(**
   See interface file
*)
let rec build_recursive_statement rebuild statement_map label =
  let stmt_ints, meta = Map.Poly.find_exn statement_map label in
  let build_stmt = build_recursive_statement rebuild statement_map in
  let stmt = Stmt.Fixed.Pattern.map Fn.id build_stmt stmt_ints in
  rebuild stmt meta

(** Represents the state required to build control flow information during an MIR
    traversal, where
     * breaks is the set of Breaks seen since the beginning of their loop
     * continues is the set of Continues seen since the beginning of their loop
     * returns is the set of Returns seen since the beginning of their function definition
     * exits is the set of nodes that could have been the last one to execute before this
       node
*)
type cf_state =
  { breaks: label Set.Poly.t
  ; continues: label Set.Poly.t
  ; returns: label Set.Poly.t
  ; exits: label Set.Poly.t }

(** Represents the control flow information at each node in the control graph, where
     * predecessors points to the nodes which could have executed before this node
     * parents points to the adjacent nodes which directly influence the execution of this
       node
*)
type cf_edges = {predecessors: label Set.Poly.t; parents: label Set.Poly.t}

(** Join the state of a controlflow traversal across different branches of execution such
    as over if/else branch. *)
let join_cf_states (state1 : cf_state) (state2 : cf_state) : cf_state =
  { breaks= Set.Poly.union state1.breaks state2.breaks
  ; continues= Set.Poly.union state1.continues state2.continues
  ; returns= Set.Poly.union state1.returns state2.returns
  ; exits= Set.Poly.union state1.exits state2.exits }

(** Check if the statement controls the execution of its substatements. *)
let is_ctrl_flow pattern =
  match pattern with
  | Stmt.Fixed.Pattern.IfElse _ -> true
  | While _ -> true
  | For _ -> true
  | _ -> false

(**
   Simultaneously builds the controlflow parent graph, the predecessor graph and the exit
   set of a statement. It's advantageous to build them together because they both rely on
   some of the same Break, Continue and Return bookkeeping.
*)
let build_cf_graphs ?(flatten_loops = false) ?(blocks_after_body = true)
    statement_map =
  let rec build_cf_graph_rec (cf_parent : label option)
      ((in_state, in_map) : cf_state * (label, cf_edges) Map.Poly.t)
      (label : label) : cf_state * (label, cf_edges) Map.Poly.t =
    let stmt, _ = Map.Poly.find_exn statement_map label in
    (* Only control flow nodes should pass themselves down as parents *)
    let child_cf = if is_ctrl_flow stmt then Some label else cf_parent in
    let join (state1, map1) (state2, map2) =
      (join_cf_states state1 state2, union_maps_left map1 map2)
    in
    (* This node is the parent of substatements, unless this is a Block, which
       is visited after substatements *)
    let substmt_preds =
      match stmt with
      | Block _ when blocks_after_body -> in_state.exits
      | _ -> Set.Poly.singleton label
    in
    (* The accumulated state after traversing substatements *)
    let substmt_state_unlooped, substmt_map =
      branching_fold_statement stmt ~join
        ~init:({in_state with exits= substmt_preds}, in_map)
        ~f:(build_cf_graph_rec child_cf)
    in
    (* If the statement is a loop, we need to include the loop body exits as predecessors
         of the loop *)
    let substmt_state, predecessors =
      match stmt with
      | For _ | While _ ->
          (* Loop statements are preceded by:
           1. The statements that come before the loop
           2. The natural exit points of the loop body
           3. Continue statements in the loop body
           This comment mangling brought to you by the autoformatter
        *)
          let loop_predecessors =
            Set.Poly.union_list
              [ (*1*) in_state.exits; (*2*) substmt_state_unlooped.exits
              ; (*3*)
                Set.Poly.diff substmt_state_unlooped.continues
                  in_state.continues ]
          in
          (* Loop exits are:
           1. The loop node itself, since the last action of a typical loop execution is
              to check if there are any iterations remaining
           2. Break statements in the loop body, since broken loops don't execute the
              loop statement
        *)
          let loop_exits =
            if flatten_loops then substmt_state_unlooped.exits
            else
              Set.Poly.union_list
                [ (*1*) Set.Poly.singleton label
                ; (*2*)
                  Set.Poly.diff substmt_state_unlooped.breaks in_state.breaks
                ]
          in
          ({substmt_state_unlooped with exits= loop_exits}, loop_predecessors)
      | Block _ when blocks_after_body ->
          (* Block statements are preceded by the natural exit points of the block
           body *)
          let block_predecessors = substmt_state_unlooped.exits in
          (* Block exits are just the block node *)
          let block_exits = Set.Poly.singleton label in
          ({substmt_state_unlooped with exits= block_exits}, block_predecessors)
      | _ -> (substmt_state_unlooped, in_state.exits)
    in
    (* Some statements interact with the break/return/continue states
       E.g., loops nullify breaks and continues in their body, but are still affected by
       breaks and input continues*)
    let breaks_out, returns_out, continues_out, extra_cf_deps =
      match stmt with
      | Break ->
          ( Set.Poly.add substmt_state.breaks label
          , substmt_state.returns
          , substmt_state.continues
          , Set.Poly.empty )
      | Return _ ->
          ( substmt_state.breaks
          , Set.Poly.add substmt_state.returns label
          , substmt_state.continues
          , Set.Poly.empty )
      | Continue ->
          ( substmt_state.breaks
          , substmt_state.returns
          , Set.Poly.add substmt_state.continues label
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
          , Set.Poly.empty )
    in
    let cf_parents =
      Set.Poly.union_list
        [ Option.value_map cf_parent ~default:Set.Poly.empty
            ~f:Set.Poly.singleton
        ; in_state.returns; in_state.continues; extra_cf_deps ]
    in
    ( { breaks= breaks_out
      ; continues= continues_out
      ; returns= returns_out
      ; exits= substmt_state.exits }
    , Map.Poly.add_exn substmt_map ~key:label
        ~data:{parents= cf_parents; predecessors} )
  in
  let state, edges =
    build_cf_graph_rec None
      ( { breaks= Set.Poly.empty
        ; continues= Set.Poly.empty
        ; returns= Set.Poly.empty
        ; exits= Set.Poly.empty }
      , Map.Poly.empty )
      1
  in
  ( state.exits
  , Map.Poly.map edges ~f:(fun e -> e.predecessors)
  , Map.Poly.map edges ~f:(fun e -> e.parents) )

(** See interface file *)
let build_cf_graph statement_map =
  let _, _, cf_graph = build_cf_graphs statement_map in
  cf_graph

(** See interface file *)
let build_predecessor_graph ?(flatten_loops = false)
    ?(blocks_after_body = true) statement_map =
  let exits, pred_graph, _ =
    build_cf_graphs ~flatten_loops ~blocks_after_body statement_map
  in
  (exits, pred_graph)
