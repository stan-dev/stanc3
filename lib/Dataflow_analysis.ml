open Core_kernel
open Mir

(***********************************)
(* Basic datatypes                 *)
(***********************************)

(**
   A label is a unique identifier for a node in the dataflow/dependency graph, and
   usually corresponds to one node in the Mir.
*)
type label = int [@@deriving sexp, hash, compare]

(**
   Representation of an expression that can be assigned to. This should also be able to
   represent indexed variables, but we don't support that yet.
*)
type vexpr = VVar of string [@@deriving sexp, hash, compare]

(**
   A 'reaching definition' (or reaching_def or RD) statement (v, l) says that the variable
   v could have been affected at the label l.
*)
type reaching_defn = vexpr * label [@@deriving sexp, hash, compare]

(**
   Description of where a node in the dependency graph came from, where MirNode is the
   location from an Mir.loc_stmt
 *)
type source_loc =
  | MirNode of string
  | StartOfBlock
  | TargetTerm of {term: expr; assignment_label: label}
[@@deriving sexp]

(**
   Information to be collected about each node
   * rd_sets: Information about how the label effects the reaching definition sets
   * possible_previous: The set of nodes that could have immediately preceded this node
     under some execution of the program
   * rhs_set: The 'right hand side' set of variables that affect the value or behavior of
     this node
   * controlflow: The set of control flow nodes that are immediate parents of this node:
     * The most recent nested if/then or loop,
     * or the beginning of the function or block if there are no containing branches,
     * plus the set of relevant continue/return statements,
     * plus, for loops, any break statements they contain
   * loc: The location of the Mir node that this node corresponds to, or a description if
     there is none
*)
type 'rd node_info =
  { rd_sets: 'rd
  ; possible_previous: label Set.Poly.t
  ; rhs_set: vexpr Set.Poly.t
  ; controlflow: label Set.Poly.t
  ; loc: source_loc }
[@@deriving sexp]

(**
   A node_info, where the reaching definition information takes the form of an update
   function that maps from the 'entry' set to the 'exit' set, where the entry set is
   what's true before executing this node and the exit set is true after.
*)
type node_info_update =
  (reaching_defn Set.Poly.t -> reaching_defn Set.Poly.t) node_info

(**
   A node_info where the reaching definition information is explicitly written as the
   entry and exit sets, as after finding the fixed-point solution.
*)
type node_info_fixedpoint =
  (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) node_info
[@@deriving sexp]

(**
   The state that will be maintained throughout the traversal of the Mir
   * label_ix: The next label that's free to use
   * node_info_map: The label information that's been built so far
   * possible_previous: The set of nodes that could have immediately preceded this point
     under some execution of the program
   * target_terms: The set of nodes that correspond to terms added to the target variable
   * continues: A set of the continue nodes that have been encountered since exiting a loop
   * breaks: A set of the break nodes that have been encountered since exiting a loop
   * returns: A set of the return nodes that have been encountered
*)
type traversal_state =
  { label_ix: label
  ; node_info_map: node_info_update Int.Map.t
  ; possible_previous: label Set.Poly.t
  ; target_terms: label Set.Poly.t
  ; continues: label Set.Poly.t
  ; breaks: label Set.Poly.t
  ; returns: label Set.Poly.t
  ; rejects: label Set.Poly.t }

(** The most recently nested control flow (block start, if/then, or loop)

    This isn't included in the traversal_state because it only flows downward through the
    tree, not across and up like everything else
*)
type cf_state = label

(**
   This is a helper function equivalent to List.concat_map but for Sets
*)
let union_map (set : 'a Set.Poly.t) ~(f : 'a -> 'b Set.Poly.t) : 'b Set.Poly.t
    =
  Set.Poly.fold set ~init:Set.Poly.empty ~f:(fun s a -> Set.Poly.union s (f a))

(***********************************)
(* Expression helper functions     *)
(***********************************)

let vexpr_of_expr_exn (ex : expr) : vexpr =
  match ex with
  | Var s -> VVar s
  | _ -> raise (Failure "Non-var expression found, but var expected")

(**
   The set of variables in an expression, including inside an index

   For use in RHS sets, not LHS assignment sets, except in a target term
*)
let rec expr_var_set (ex : expr) : vexpr Set.Poly.t =
  let union_recur exprs =
    Set.Poly.union_list (List.map exprs ~f:expr_var_set)
  in
  match ex with
  | Var s -> Set.Poly.singleton (VVar s)
  | Lit _ -> Set.Poly.empty
  | FunApp (_, exprs) -> union_recur exprs
  | BinOp (expr1, _, expr2) -> union_recur [expr1; expr2]
  | TernaryIf (expr1, expr2, expr3) -> union_recur [expr1; expr2; expr3]
  | Indexed (expr, ix) ->
      Set.Poly.union_list (expr_var_set expr :: List.map ix ~f:index_var_set)

and index_var_set (ix : index) : vexpr Set.Poly.t =
  match ix with
  | All -> Set.Poly.empty
  | Single expr -> expr_var_set expr
  | Upfrom expr -> expr_var_set expr
  | Downfrom expr -> expr_var_set expr
  | Between (expr1, expr2) ->
      Set.Poly.union (expr_var_set expr1) (expr_var_set expr2)
  | MultiIndex expr -> expr_var_set expr

(**
   The variable being assigned to when `ex` is LHS
*)
let expr_assigned_var (ex : expr) : vexpr =
  match ex with
  | Var s -> VVar s
  | Indexed (Var s, _) -> VVar s
  | _ -> raise (Failure "Unimplemented: analysis of assigning to non-var")

(***********************************)
(* Label and RD helper functions   *)
(***********************************)

(** Remove RDs corresponding to a variable *)
let filter_var_defns (defns : reaching_defn Set.Poly.t) (var : vexpr) :
    reaching_defn Set.Poly.t =
  Set.Poly.filter defns ~f:(fun (v, _) -> v <> var)

(** Union label maps, preserving the left element in a collision *)
let merge_label_maps (m1 : 'a Int.Map.t) (m2 : 'a Int.Map.t) : 'a Int.Map.t =
  let f ~key:_ opt =
    match opt with
    | `Left v -> Some v
    | `Right v -> Some v
    | `Both (v1, _) -> Some v1
  in
  Int.Map.merge m1 m2 ~f

(** Get the label of the next node to be assigned *)
let peek_next_label (st : traversal_state) : label = st.label_ix

(** Get a new label and update the state *)
let new_label (st : traversal_state) : label * traversal_state =
  (st.label_ix, {st with label_ix= st.label_ix + 1})

(** The list of terms in expression *)
let rec summation_terms (rhs : expr) : expr list =
  match rhs with
  | BinOp (e1, Plus, e2) ->
      List.append (summation_terms e1) (summation_terms e2)
  | _ as e -> [e]

(** Apply function `f` to node_info for `label` in `trav_st` *)
let modify_node_info (trav_st : traversal_state) (label : label)
    (f : node_info_update -> node_info_update) : traversal_state =
  { trav_st with
    node_info_map=
      Int.Map.change trav_st.node_info_map label ~f:(function
        (*Option.map should exist but doesn't appear to*)
        | None -> None
        | Some info -> Some (f info) ) }

(**
   Right-compose a function with the reaching definition update functions of the possible
   set of previously executed nodes
*)
let compose_last_rd_update
    (alter : reaching_defn Set.Poly.t -> reaching_defn Set.Poly.t)
    (trav_st : traversal_state) : traversal_state =
  let compose_rd_update node_info =
    {node_info with rd_sets= (fun set -> alter (node_info.rd_sets set))}
  in
  Set.Poly.fold trav_st.possible_previous
    ~f:(fun trav_st label -> modify_node_info trav_st label compose_rd_update)
    ~init:trav_st

(***********************************)
(* Mir traversal & node_info making*)
(***********************************)

(**
   Define 'node 0', the node representing the beginning of the block. This node adds
   global variables declared before execution of the block to the RD set, and forwards
   along the effects of the term labels (thought initially there are none. This is
   analogous to the beginning of a loop, where control could have come from before the
   loop or from the end of the loop.
*)
let node_0 (top_vars : vexpr Set.Poly.t) : node_info_update =
  { rd_sets=
      (fun entry ->
        Set.Poly.union entry (Set.Poly.map top_vars ~f:(fun v -> (v, 0))) )
  ; possible_previous= Set.Poly.empty
  ; rhs_set= Set.Poly.empty
  ; controlflow= Set.Poly.empty
  ; loc= StartOfBlock }

(**
   Initialize a traversal state, including node 0 with the top variables
*)
let initial_traversal_state (top_vars : vexpr Set.Poly.t) : traversal_state =
  let node_0_info = node_0 top_vars in
  { label_ix= 1
  ; node_info_map= Int.Map.singleton 0 node_0_info
  ; possible_previous= Set.Poly.singleton 0
  ; target_terms= Set.Poly.empty
  ; continues= Set.Poly.empty
  ; breaks= Set.Poly.empty
  ; returns= Set.Poly.empty
  ; rejects= Set.Poly.empty }

let initial_cf_st = 0

(**
   Append a node to the traversal_state that corresponds to the effect that a target
   term has on the variables it involves.

   Each term node lists every other as a `possible_previous` node, because sampling
   considers them effectively simultaneously. Term nodes list their corresponding target
   increment node's control flow as their own.

   Term nodes are modeled as executing before the start of the block, rather than before
   the next expression in the traversal. Term nodes can't be included in the normal flow
   of the graph, since the effect they have on parameters doesn't 'happen' until in
   between executions of the block. Instead, it works similarly to a while loop, with
   target terms at the 'end' of the loop body.
*)
let add_target_term_node (trav_st : traversal_state) (assignment_node : label)
    (term : expr) : traversal_state =
  let label, trav_st' = new_label trav_st in
  let assgn_info = Int.Map.find_exn trav_st'.node_info_map assignment_node in
  let term_vars = expr_var_set term in
  let info =
    { rd_sets= (fun _ -> Set.Poly.map term_vars ~f:(fun v -> (v, label)))
    ; possible_previous=
        Set.Poly.union assgn_info.possible_previous trav_st.target_terms
    ; rhs_set= term_vars
    ; controlflow= assgn_info.controlflow
    ; loc= TargetTerm {term; assignment_label= assignment_node} }
  in
  let trav_st'' =
    { trav_st' with
      node_info_map=
        merge_label_maps trav_st'.node_info_map (Int.Map.singleton label info)
    ; target_terms= Set.Poly.add trav_st'.target_terms label }
  in
  let add_previous (node_info : node_info_update) : node_info_update =
    { node_info with
      possible_previous= Set.Poly.add node_info.possible_previous label }
  in
  Set.Poly.fold (Set.Poly.add trav_st.target_terms 0) ~init:trav_st''
    ~f:(fun trav_st l -> modify_node_info trav_st l add_previous )

(**
   Traverse the Mir statement `st` to build up a final `traversal_state` value.

   See `traversal_state` and `cf_state` types for descriptions of the state.

   Traversal is done in a syntax-directed order, and builds a node_info values for each
   Mir node that could affect or read a variable.
*)
let rec traverse_mir (trav_st : traversal_state) (cf_st : cf_state)
    (st : stmt_loc) : traversal_state =
  match st.stmt with
  | Assignment (lhs, rhs) ->
      let label, trav_st' = new_label trav_st in
      let info =
        { rd_sets=
            (fun entry ->
              let assigned_var = expr_assigned_var lhs in
              Set.Poly.union
                (filter_var_defns entry assigned_var)
                (Set.Poly.singleton (assigned_var, label)) )
        ; possible_previous= trav_st'.possible_previous
        ; rhs_set= expr_var_set rhs
        ; controlflow=
            Set.Poly.union_list
              [Set.Poly.singleton cf_st; trav_st.continues; trav_st.returns]
        ; loc= MirNode st.sloc }
      in
      let trav_st'' =
        { trav_st' with
          node_info_map=
            merge_label_maps trav_st'.node_info_map
              (Int.Map.singleton label info)
        ; possible_previous= Set.Poly.singleton label }
      in
      if lhs = Var "target" then
        List.fold_left
          (List.filter (summation_terms rhs) ~f:(fun v -> v <> Var "target"))
          ~init:trav_st''
          ~f:(fun trav_st term -> add_target_term_node trav_st label term)
      else trav_st''
  | NRFunApp ("reject", _) ->
      let label, trav_st' = new_label trav_st in
      let info =
        { rd_sets= (fun entry -> entry)
        ; possible_previous= trav_st'.possible_previous
        ; rhs_set= Set.Poly.empty
        ; controlflow=
            Set.Poly.union_list
              [Set.Poly.singleton cf_st; trav_st.continues; trav_st.returns]
        ; loc= MirNode st.sloc }
      in
      let add_cf (node_info : node_info_update) : node_info_update =
        {node_info with controlflow= Set.Poly.add node_info.controlflow label}
      in
      { (modify_node_info trav_st' 0 add_cf) with
        node_info_map=
          merge_label_maps trav_st'.node_info_map
            (Int.Map.singleton label info)
      ; possible_previous= Set.Poly.singleton label
      ; rejects= Set.Poly.add trav_st'.rejects label }
  | NRFunApp (_, exprs) ->
      let label, trav_st' = new_label trav_st in
      let info =
        { rd_sets= (fun entry -> entry)
        ; possible_previous= trav_st'.possible_previous
        ; rhs_set= Set.Poly.union_list (List.map exprs ~f:expr_var_set)
        ; controlflow=
            Set.Poly.union_list
              [Set.Poly.singleton cf_st; trav_st.continues; trav_st.returns]
        ; loc= MirNode st.sloc }
      in
      { trav_st' with
        node_info_map=
          merge_label_maps trav_st'.node_info_map
            (Int.Map.singleton label info)
      ; possible_previous= Set.Poly.singleton label }
  | Check _ -> trav_st
  | MarkLocation _ -> trav_st
  | Break ->
      let label, trav_st' = new_label trav_st in
      {trav_st' with breaks= Set.Poly.add trav_st'.breaks label}
  | Continue ->
      let label, trav_st' = new_label trav_st in
      {trav_st' with continues= Set.Poly.add trav_st'.continues label}
  | Return _ ->
      let label, trav_st' = new_label trav_st in
      {trav_st' with returns= Set.Poly.add trav_st'.returns label}
  | Skip -> trav_st
  | IfElse (pred, then_stmt, else_stmt) -> (
      let label, trav_st' = new_label trav_st in
      let recurse_st =
        {trav_st' with possible_previous= Set.Poly.singleton label}
      in
      let then_st = traverse_mir recurse_st label then_stmt in
      let else_st_opt = Option.map else_stmt ~f:(traverse_mir then_st label) in
      let info =
        { rd_sets= (fun entry -> entry) (* is this correct? *)
        ; possible_previous= trav_st'.possible_previous
        ; rhs_set= expr_var_set pred
        ; controlflow=
            Set.Poly.union_list
              [Set.Poly.singleton cf_st; trav_st.continues; trav_st.returns]
        ; loc= MirNode st.sloc }
      in
      match else_st_opt with
      | Some else_st ->
          { else_st with
            node_info_map=
              merge_label_maps else_st.node_info_map
                (Int.Map.singleton label info)
          ; possible_previous=
              Set.Poly.union then_st.possible_previous
                else_st.possible_previous }
      | None ->
          { then_st with
            node_info_map=
              merge_label_maps then_st.node_info_map
                (Int.Map.singleton label info)
          ; possible_previous=
              Set.Poly.union then_st.possible_previous
                trav_st'.possible_previous } )
  | While (pred, body_stmt) ->
      let label, trav_st' = new_label trav_st in
      let recurse_st =
        {trav_st' with possible_previous= Set.Poly.singleton label}
      in
      let body_st = traverse_mir recurse_st label body_stmt in
      let loop_start_possible_previous =
        Set.Poly.union_list
          [ Set.Poly.singleton label; body_st.possible_previous
          ; body_st.continues ]
      in
      let body_st' =
        modify_node_info body_st (peek_next_label recurse_st) (fun info ->
            {info with possible_previous= loop_start_possible_previous} )
      in
      let info =
        { rd_sets= (fun entry -> entry) (* is this correct? *)
        ; possible_previous= trav_st'.possible_previous
        ; rhs_set= expr_var_set pred
        ; controlflow=
            Set.Poly.union_list
              [ Set.Poly.singleton cf_st; trav_st.continues; trav_st.returns
              ; body_st'.breaks ]
        ; loc= MirNode st.sloc }
      in
      { body_st' with
        node_info_map=
          merge_label_maps body_st'.node_info_map
            (Int.Map.singleton label info)
      ; possible_previous=
          Set.Poly.union body_st'.possible_previous trav_st'.possible_previous
      ; continues= Set.Poly.empty
      ; breaks= Set.Poly.empty }
  | For args ->
      let label, trav_st' = new_label trav_st in
      let recurse_st =
        {trav_st' with possible_previous= Set.Poly.singleton label}
      in
      let body_st = traverse_mir recurse_st label args.body in
      let loop_start_possible_previous =
        Set.Poly.union_list
          [ Set.Poly.singleton label; body_st.possible_previous
          ; body_st.continues ]
      in
      let body_st' =
        modify_node_info body_st (peek_next_label recurse_st) (fun info ->
            {info with possible_previous= loop_start_possible_previous} )
      in
      let alter_fn set =
        Set.Poly.remove set (vexpr_of_expr_exn args.loopvar, label)
      in
      let body_st'' = compose_last_rd_update alter_fn body_st' in
      let info =
        { rd_sets=
            (fun entry ->
              Set.Poly.union entry
                (Set.Poly.singleton (vexpr_of_expr_exn args.loopvar, label)) )
        ; possible_previous= trav_st'.possible_previous
        ; rhs_set=
            Set.Poly.union (expr_var_set args.lower) (expr_var_set args.upper)
        ; controlflow=
            Set.Poly.union_list
              [ Set.Poly.singleton cf_st; trav_st.continues; trav_st.returns
              ; body_st''.breaks ]
        ; loc= MirNode st.sloc }
      in
      { body_st'' with
        node_info_map=
          merge_label_maps body_st''.node_info_map
            (Int.Map.singleton label info)
      ; possible_previous=
          Set.Poly.union body_st''.possible_previous trav_st'.possible_previous
      ; continues= Set.Poly.empty
      ; breaks= Set.Poly.empty }
  | Block stmts ->
      let f state stmt = traverse_mir state cf_st stmt in
      List.fold_left stmts ~init:trav_st ~f
  | SList stmts ->
      let f state stmt = traverse_mir state cf_st stmt in
      List.fold_left stmts ~init:trav_st ~f
  | Decl args ->
      let label, trav_st' = new_label trav_st in
      let info =
        { rd_sets=
            (let assigned_var = VVar args.decl_id in
             let addition = Set.Poly.singleton (assigned_var, label) in
             fun entry ->
               Set.Poly.union addition (filter_var_defns entry assigned_var))
        ; possible_previous= trav_st'.possible_previous
        ; rhs_set= Set.Poly.empty
        ; controlflow=
            Set.Poly.union_list
              [Set.Poly.singleton cf_st; trav_st.continues; trav_st.returns]
        ; loc= MirNode st.sloc }
      in
      { trav_st' with
        node_info_map=
          merge_label_maps trav_st'.node_info_map
            (Int.Map.singleton label info)
      ; possible_previous= Set.Poly.singleton label }
  | FunDef _ -> trav_st

(***********************************)
(* RD fixed-point functions           *)
(***********************************)

(**
   Find the new value of the RD sets in a node_info, given the previous iteration of RD
   sets
 *)
let rd_update_label (node_info : node_info_update)
    (prev : (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) Int.Map.t) :
    reaching_defn Set.Poly.t * reaching_defn Set.Poly.t =
  let get_exit label = snd (Int.Map.find_exn prev label) in
  let from_prev = union_map node_info.possible_previous ~f:get_exit in
  (from_prev, node_info.rd_sets from_prev)

(**
   Find the new values of the RD sets in node_infos, given the previous iteration of RD
   sets
 *)
let rd_apply (node_infos : node_info_update Int.Map.t)
    (prev : (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) Int.Map.t) :
    (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) Int.Map.t =
  let update_label ~key:(label : label) ~data:_ =
    let node_info = Int.Map.find_exn node_infos label in
    rd_update_label node_info prev
  in
  Int.Map.mapi prev ~f:update_label

(** Find the fixed point of a function and an initial value, given definition of equality *)
let rec apply_until_fixed (equal : 'a -> 'a -> bool) (f : 'a -> 'a) (x : 'a) :
    'a =
  let y = f x in
  if equal x y then x else apply_until_fixed equal f y

(**
   Tests RD sets for equality.

   It turns out that doing = or == does not work for these types.
   = actually gives a *runtime* error.
*)
let rd_equal
    (a : (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) Int.Map.t)
    (b : (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) Int.Map.t) :
    bool =
  let equal_set_pairs (a1, a2) (b1, b2) =
    Set.Poly.equal a1 b1 && Set.Poly.equal a2 b2
  in
  Int.Map.equal equal_set_pairs a b

(**
   Find the fixed point of the dataflow update functions. Fixed point should correspond to
   the full, correct dataflow graph.
*)
let rd_fixedpoint (info : node_info_update Int.Map.t) :
    node_info_fixedpoint Int.Map.t =
  let initial_sets =
    Int.Map.map info ~f:(fun _ -> (Set.Poly.empty, Set.Poly.empty))
  in
  let fixed_points = apply_until_fixed rd_equal (rd_apply info) initial_sets in
  Int.Map.mapi fixed_points ~f:(fun ~key:label ~data:fixedpoint ->
      {(Int.Map.find_exn info label) with rd_sets= fixedpoint} )

(***********************************)
(* Dependency analysis & interface *)
(***********************************)

(**
   Everything we need to know to do dependency analysis
   * node_info_map: Collection of node information
   * possible_exits: Set of nodes that could be the last to execute under some execution
   * probabilistic_nodes: Set of nodes corresponding to which can only introduce
     probabilistic dependencies, such as target terms and reject statements, to be
     excluded for non-statistical dependency analysis
*)
type dataflow_graph =
  { node_info_map: node_info_fixedpoint Int.Map.t
  ; possible_exits: label Set.Poly.t
  ; probabilistic_nodes: label Set.Poly.t }
[@@deriving sexp]

(**
   Construct a dataflow graph for the block, given some top (global?) variables
*)
let block_dataflow_graph (body : stmt_loc) (param_vars : vexpr Set.Poly.t) :
    dataflow_graph =
  let initial_trav_st = initial_traversal_state param_vars in
  let trav_st = traverse_mir initial_trav_st initial_cf_st body in
  let node_info_fixedpoint = rd_fixedpoint trav_st.node_info_map in
  { node_info_map= node_info_fixedpoint
  ; possible_exits= Set.Poly.union trav_st.possible_previous trav_st.returns
  ; probabilistic_nodes= Set.Poly.union trav_st.target_terms trav_st.rejects }

(**
   Find the set of labels for nodes that could affect the value or behavior of the node
   with `label`.

   If `statistical_dependence` is off, the nodes corresponding to target terms will not be
   traversed (recursively), and the result will be the same as a classical dataflow
   analysis.
*)
let rec label_dependencies (df_graph : dataflow_graph)
    (statistical_dependence : bool) (so_far : label Set.Poly.t) (label : label)
    : label Set.Poly.t =
  let node_info = Int.Map.find_exn df_graph.node_info_map label in
  let rhs_labels =
    Set.Poly.map
      (Set.Poly.filter (fst node_info.rd_sets) ~f:(fun (v, _) ->
           Set.Poly.mem node_info.rhs_set v ))
      ~f:snd
  in
  let labels = Set.Poly.union rhs_labels node_info.controlflow in
  let filtered_labels =
    if statistical_dependence then labels
    else Set.Poly.diff labels df_graph.probabilistic_nodes
  in
  labels_dependencies df_graph statistical_dependence
    (Set.Poly.add so_far label)
    filtered_labels

(**
   Find the set of labels for nodes that could affect the value or behavior of any of the
   nodes `labels`.

   If `statistical_dependence` is off, the nodes corresponding to target terms will not be
   traversed (recursively), and the result will be the same as a classical dataflow
   analysis.
*)
and labels_dependencies (df_graph : dataflow_graph)
    (statistical_dependence : bool) (so_far : label Set.Poly.t)
    (labels : label Set.Poly.t) : label Set.Poly.t =
  Set.Poly.fold labels ~init:so_far ~f:(fun so_far label ->
      if Set.Poly.mem so_far label then so_far
      else label_dependencies df_graph statistical_dependence so_far label )

(**
   Find the set of labels for nodes that could affect the final value of the variable.

   If `statistical_dependence` is off, the nodes corresponding to target terms will not be
   traversed (recursively), and the result will be the same as a classical dataflow
   analysis.
*)
let final_var_dependencies (df_graph : dataflow_graph)
    (statistical_dependence : bool) (var : vexpr) : label Set.Poly.t =
  let exit_rd_set =
    union_map df_graph.possible_exits ~f:(fun l ->
        let info = Int.Map.find_exn df_graph.node_info_map l in
        snd info.rd_sets )
  in
  let labels =
    Set.Poly.map
      (Set.Poly.filter exit_rd_set ~f:(fun (v, _) -> v = var))
      ~f:snd
  in
  let filtered_labels =
    if statistical_dependence then labels
    else Set.Poly.diff labels df_graph.probabilistic_nodes
  in
  labels_dependencies df_graph statistical_dependence Set.Poly.empty
    filtered_labels

(**
   Find the set of top variables that are dependencies for the set of nodes
   `labels`.
*)
let top_var_dependencies (df_graph : dataflow_graph)
    (labels : label Set.Poly.t) : vexpr Set.Poly.t =
  let rds =
    union_map labels ~f:(fun l ->
        let info = Int.Map.find_exn df_graph.node_info_map l in
        Set.Poly.filter (fst info.rd_sets) ~f:(fun (v, l) ->
            l = 0 && Set.Poly.mem info.rhs_set v ) )
  in
  Set.Poly.map rds ~f:fst

(**
   Find the set of target term nodes which do not depend on any top variables in
   `exprs`. Only non-statistical dependence is considered, otherwise all overlapping terms
   will depend on eachother.
 *)
let exprset_independent_target_terms (df_graph : dataflow_graph)
    (exprs : vexpr Set.Poly.t) : label Set.Poly.t =
  Set.Poly.filter df_graph.probabilistic_nodes ~f:(fun l ->
      let label_deps = label_dependencies df_graph false Set.Poly.empty l in
      Set.Poly.is_empty
        (Set.Poly.inter (top_var_dependencies df_graph label_deps) exprs) )

(**
   Helper function to construct an (variable) expression set from the variable names from
   a top_var_table
*)
let exprset_of_table (table : top_var_table) : vexpr Set.Poly.t =
  Set.Poly.of_list (List.map (Map.Poly.keys table) ~f:(fun s -> VVar s))

(**
   Represents the dataflow graphs for each interesting block in the program MIR.

   See Mir.prog for block descriptions.
*)
type prog_df_graphs =
  {tdatab: dataflow_graph; modelb: dataflow_graph; gqb: dataflow_graph}
[@@deriving sexp]

(**
   Build the dataflow graphs for each interesting block in the program MIR
*)
let program_df_graphs (prog : stmt_loc prog) : prog_df_graphs =
  let data_table = prog.datavars in
  let tdata_table, tdata_block = prog.tdatab in
  let data_vars =
    Set.Poly.union (exprset_of_table data_table) (exprset_of_table tdata_table)
  in
  let parameter_table, model_block = prog.modelb in
  let parameter_vars = exprset_of_table parameter_table in
  let _, gq_block = prog.gqb in
  let top_vars = Set.Poly.union data_vars parameter_vars in
  { tdatab= block_dataflow_graph tdata_block top_vars
  ; modelb= block_dataflow_graph model_block top_vars
  ; gqb= block_dataflow_graph gq_block top_vars }

(**
   Builds a dataflow graph from the model block and evaluates the label and global
   variable dependencies of the "y" variable, printing results to stdout.
*)
let analysis_example (prog : stmt_loc prog) : dataflow_graph =
  let data_table = prog.datavars in
  let tdata_table, _ = prog.tdatab in
  let data_vars =
    Set.Poly.union (exprset_of_table data_table) (exprset_of_table tdata_table)
  in
  let parameter_table, model_block = prog.modelb in
  let parameter_vars = exprset_of_table parameter_table in
  let top_vars = Set.Poly.union data_vars parameter_vars in
  let df_graph = block_dataflow_graph model_block top_vars in
  let var = "y" in
  let label_deps = final_var_dependencies df_graph true (VVar var) in
  let expr_deps = top_var_dependencies df_graph label_deps in
  let prior_term_labels =
    exprset_independent_target_terms df_graph data_vars
  in
  let prior_terms =
    List.map (Set.Poly.to_list prior_term_labels) ~f:(fun l ->
        match (Int.Map.find_exn df_graph.node_info_map l).loc with
        | TargetTerm {term; _} -> term
        | _ -> raise (Failure "Found non-target term in target term list") )
  in
  if false then (
    Sexp.pp_hum Format.std_formatter
      [%sexp (df_graph.node_info_map : node_info_fixedpoint Int.Map.t)] ;
    print_string "\n\n" ;
    print_endline
      ( "Top data variables: "
      ^ Sexp.to_string [%sexp (data_vars : vexpr Set.Poly.t)] ) ;
    print_endline
      ( "Top parameter variables: "
      ^ Sexp.to_string [%sexp (parameter_vars : vexpr Set.Poly.t)] ) ;
    print_endline
      ( "Target term nodes: "
      ^ Sexp.to_string
          [%sexp (df_graph.probabilistic_nodes : label Set.Poly.t)] ) ;
    print_endline
      ( "Possible endpoints: "
      ^ Sexp.to_string [%sexp (df_graph.possible_exits : label Set.Poly.t)] ) ;
    print_endline
      ( "Data-independent target term expressions: "
      ^ Sexp.to_string [%sexp (prior_terms : expr list)] ) ;
    print_endline
      ( "Var " ^ var ^ " depends on labels: "
      ^ Sexp.to_string [%sexp (label_deps : label Set.Poly.t)] ) ;
    print_endline
      ( "Var " ^ var ^ " depends on top variables: "
      ^ Sexp.to_string [%sexp (expr_deps : vexpr Set.Poly.t)] ) ) ;
  df_graph

(***********************************)
(* Tests                           *)
(***********************************)

let%expect_test "bernoulli_logit_glm_performance.stan" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
transformed data {
  int<lower=0> N = 50;
  int<lower=0> M = 100;
  matrix[N,M] x;
  int<lower=0,upper=1> y[N];
  vector[M] beta_true;
  real alpha_true = 1.5;
  for (j in 1:M)
  {
    beta_true[j] = j/M;
  }
  for (i in 1:N)
  {
    for (j in 1:M)
    {
      x[i,j] = normal_rng(0,1);
    }
    y[i] = bernoulli_logit_rng((x * beta_true + alpha_true)[i]);
  }
}
parameters {
  real alpha_inferred;
  vector[M] beta_inferred;
  }
model {
  beta_inferred ~ normal(0, 2);
  alpha_inferred ~ normal(0, 4);

  y ~ bernoulli_logit_glm(x, alpha_inferred, beta_inferred);
}
      |}
  in
  let prog =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let df_graphs = program_df_graphs prog in
  print_s [%sexp (df_graphs : prog_df_graphs)] ;
  [%expect
    {|
      ((tdatab
        ((node_info_map
          ((0
            ((rd_sets
              (()
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar x) 0) ((VVar y) 0))))
             (possible_previous ()) (rhs_set ()) (controlflow ())
             (loc StartOfBlock)))
           (1
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar x) 0) ((VVar y) 0))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar j) 1) ((VVar x) 0) ((VVar y) 0))))
             (possible_previous (0)) (rhs_set ((VVar M))) (controlflow (0))
             (loc (MirNode "\"string\", line 9-12"))))
           (2
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar beta_true) 2) ((VVar j) 1) ((VVar x) 0) ((VVar y) 0))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 2)
                ((VVar x) 0) ((VVar y) 0))))
             (possible_previous (1 2)) (rhs_set ((VVar M) (VVar j)))
             (controlflow (1)) (loc (MirNode "\"string\", line 11-11"))))
           (3
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar beta_true) 2) ((VVar x) 0) ((VVar y) 0))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar beta_true) 2) ((VVar i) 3) ((VVar x) 0) ((VVar y) 0))))
             (possible_previous (0 2)) (rhs_set ((VVar N))) (controlflow (0))
             (loc (MirNode "\"string\", line 13-20"))))
           (4
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar beta_true) 2) ((VVar i) 3) ((VVar x) 0) ((VVar x) 5)
                ((VVar y) 0) ((VVar y) 6))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar beta_true) 2) ((VVar i) 3) ((VVar j) 4) ((VVar x) 0)
                ((VVar x) 5) ((VVar y) 0) ((VVar y) 6))))
             (possible_previous (3 6)) (rhs_set ((VVar M))) (controlflow (3))
             (loc (MirNode "\"string\", line 15-18"))))
           (5
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar beta_true) 2) ((VVar i) 3) ((VVar j) 4) ((VVar x) 0)
                ((VVar x) 5) ((VVar y) 0) ((VVar y) 6))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar beta_true) 2) ((VVar i) 3) ((VVar x) 5) ((VVar y) 0)
                ((VVar y) 6))))
             (possible_previous (4 5)) (rhs_set ()) (controlflow (4))
             (loc (MirNode "\"string\", line 17-17"))))
           (6
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar beta_true) 2) ((VVar i) 3) ((VVar x) 0) ((VVar x) 5)
                ((VVar y) 0) ((VVar y) 6))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar beta_true) 2) ((VVar x) 0) ((VVar x) 5) ((VVar y) 6))))
             (possible_previous (3 5))
             (rhs_set ((VVar alpha_true) (VVar beta_true) (VVar i) (VVar x)))
             (controlflow (3)) (loc (MirNode "\"string\", line 19-19"))))))
         (possible_exits (0 2 6)) (probabilistic_nodes ())))
       (modelb
        ((node_info_map
          ((0
            ((rd_sets
              ((((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6) ((VVar x) 6)
                ((VVar y) 6))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar x) 0) ((VVar x) 6) ((VVar y) 0)
                ((VVar y) 6))))
             (possible_previous (2 4 6)) (rhs_set ()) (controlflow ())
             (loc StartOfBlock)))
           (1
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar x) 0) ((VVar x) 6) ((VVar y) 0)
                ((VVar y) 6))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar target) 1) ((VVar x) 0) ((VVar x) 6)
                ((VVar y) 0) ((VVar y) 6))))
             (possible_previous (0)) (rhs_set ((VVar beta_inferred) (VVar target)))
             (controlflow (0)) (loc (MirNode "\"string\", line 27-27"))))
           (2
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar x) 0) ((VVar x) 6) ((VVar y) 0)
                ((VVar y) 6))
               (((VVar beta_inferred) 2))))
             (possible_previous (0 4 6)) (rhs_set ((VVar beta_inferred)))
             (controlflow (0))
             (loc
              (TargetTerm
               (term (FunApp normal ((Var beta_inferred) (Lit Int 0) (Lit Int 2))))
               (assignment_label 1)))))
           (3
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar target) 1) ((VVar x) 0) ((VVar x) 6)
                ((VVar y) 0) ((VVar y) 6))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar target) 3) ((VVar x) 0) ((VVar x) 6)
                ((VVar y) 0) ((VVar y) 6))))
             (possible_previous (1))
             (rhs_set ((VVar alpha_inferred) (VVar target))) (controlflow (0))
             (loc (MirNode "\"string\", line 28-28"))))
           (4
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar target) 1) ((VVar x) 0) ((VVar x) 6)
                ((VVar y) 0) ((VVar y) 6))
               (((VVar alpha_inferred) 4))))
             (possible_previous (1 2 6)) (rhs_set ((VVar alpha_inferred)))
             (controlflow (0))
             (loc
              (TargetTerm
               (term
                (FunApp normal ((Var alpha_inferred) (Lit Int 0) (Lit Int 4))))
               (assignment_label 3)))))
           (5
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar target) 3) ((VVar x) 0) ((VVar x) 6)
                ((VVar y) 0) ((VVar y) 6))
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar target) 5) ((VVar x) 0) ((VVar x) 6)
                ((VVar y) 0) ((VVar y) 6))))
             (possible_previous (3))
             (rhs_set
              ((VVar alpha_inferred) (VVar beta_inferred) (VVar target) (VVar x)
               (VVar y)))
             (controlflow (0)) (loc (MirNode "\"string\", line 30-30"))))
           (6
            ((rd_sets
              ((((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_inferred) 4) ((VVar alpha_inferred) 6)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0)
                ((VVar beta_inferred) 2) ((VVar beta_inferred) 6)
                ((VVar beta_true) 0) ((VVar target) 3) ((VVar x) 0) ((VVar x) 6)
                ((VVar y) 0) ((VVar y) 6))
               (((VVar alpha_inferred) 6) ((VVar beta_inferred) 6) ((VVar x) 6)
                ((VVar y) 6))))
             (possible_previous (2 3 4))
             (rhs_set
              ((VVar alpha_inferred) (VVar beta_inferred) (VVar x) (VVar y)))
             (controlflow (0))
             (loc
              (TargetTerm
               (term
                (FunApp bernoulli_logit_glm
                 ((Var y) (Var x) (Var alpha_inferred) (Var beta_inferred))))
               (assignment_label 5)))))))
         (possible_exits (5)) (probabilistic_nodes (2 4 6))))
       (gqb
        ((node_info_map
          ((0
            ((rd_sets
              (()
               (((VVar M) 0) ((VVar N) 0) ((VVar alpha_inferred) 0)
                ((VVar alpha_true) 0) ((VVar beta_inferred) 0) ((VVar beta_true) 0)
                ((VVar x) 0) ((VVar y) 0))))
             (possible_previous ()) (rhs_set ()) (controlflow ())
             (loc StartOfBlock)))))
         (possible_exits (0)) (probabilistic_nodes ()))))
    |}]

let%expect_test "Example program" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        model {
          for (i in 1:2)
            for (j in 3:4)
              print("Badger", i + j);
        }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let table, block = mir.modelb in
  let df_graph = block_dataflow_graph block (exprset_of_table table) in
  print_s [%sexp (df_graph : dataflow_graph)] ;
  [%expect
    {|
      ((node_info_map
        ((0
          ((rd_sets (() ())) (possible_previous ()) (rhs_set ()) (controlflow ())
           (loc StartOfBlock)))
         (1
          ((rd_sets (() (((VVar i) 1)))) (possible_previous (0)) (rhs_set ())
           (controlflow (0)) (loc (MirNode "\"string\", line 3-5"))))
         (2
          ((rd_sets ((((VVar i) 1)) (((VVar i) 1) ((VVar j) 2))))
           (possible_previous (1 3)) (rhs_set ()) (controlflow (1))
           (loc (MirNode "\"string\", line 4-5"))))
         (3
          ((rd_sets ((((VVar i) 1) ((VVar j) 2)) ())) (possible_previous (2 3))
           (rhs_set ((VVar i) (VVar j))) (controlflow (2))
           (loc (MirNode "\"string\", line 5-5"))))))
       (possible_exits (0 1 3)) (probabilistic_nodes ()))
    |}]

(**
   ~~~~~ STILL TODO ~~~~~
 * Indexed variables are currently handled as monoliths
 * Need to know which variables are parameters and which are data, since
   * target terms shouldn't introduce dependency to data variables
   * data-independent target terms might be useful
 * Variables declared in blocks should go out of scope
   * This is done already for for-loop index variables
 * Traverse functions that end in st, since they can change the target
 **)
