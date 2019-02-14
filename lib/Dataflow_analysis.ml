open Core_kernel
open Mir

let debug_verbose = false
let demonstration_verbose = false

(***********************************)
(* Basic datatypes                 *)
(***********************************)

(**
   A label is a unique identifier for a node in the dataflow/dependency graph, and
   usually corresponds to one node in the Mir.
 *)
type label = int
[@@deriving sexp]

(**
  A 'reaching dependency' (or reaching_dep or RD) statement (v, l) says that the variable
  v could have been affected at the label l.
 *)
type reaching_dep = (expr * int)
[@@deriving sexp, hash, compare]

(**
  A reaching dependency set holds the set of reaching dependencies that could be true at
  some point.
 *)
module ReachingDepSet = Set.Make(struct
    type t = reaching_dep
    let compare : reaching_dep -> reaching_dep -> int = compare
    let sexp_of_t = sexp_of_reaching_dep
    let t_of_sexp = reaching_dep_of_sexp
  end)

module LabelMap = Map.Make(
  struct
    type t = label
    let compare : int -> int -> int = compare
    let sexp_of_t = sexp_of_int
    let t_of_sexp = int_of_sexp
  end)

module LabelSet = Set.Make(
  struct
    type t = label
    let compare : int -> int -> int = compare
    let sexp_of_t = sexp_of_int
    let t_of_sexp = int_of_sexp
  end)

module ExprSet = Set.Make(
  struct
    type t = expr
    let compare : expr -> expr -> int = compare
    let sexp_of_t = sexp_of_expr
    let t_of_sexp = expr_of_sexp
  end)

(**
   Information we collect about each node
   * dep_sets: Information about how the label effects the dependency set
   * rhs_set: The 'right hand side' set of variables that affect the value or behavior of
     this node
   * possible_previous: The set of nodes that could have immediately preceded this node
     under some execution of the program
   * controlflow: The set of control flow nodes that are immediate parents of this node:
     * The most recent nested if/then or loop,
     * or the beginning of the function or block if there are no containing branches,
     * plus the set of relevant continue/return statements,
     * plus, for loops, any break statements they contain
   * loc: The location of the Mir node that this node corresponds to, or a description if
     there is none
   * target_sum_terms: The list of target term expressions this node adds, if any
 *)
type 'dep label_info =
  {
    dep_sets : 'dep
  ; possible_previous : LabelSet.t
  ; rhs_set : ExprSet.t
  ; controlflow : LabelSet.t
  ; loc : string
  ; target_sum_terms : (expr list) option
  }
[@@deriving sexp]

(**
   A label_info, where the reaching dependency information takes the form of an update
   function that maps from the 'entry' set to the 'exit' set, where the entry set is
   what's true before executing this node and the exit set is true after.
 *)
type label_info_update = (ReachingDepSet.t -> ReachingDepSet.t) label_info

(**
   A label_info where the reaching dependency information is explicitly written as the
   entry and exit sets, as after fixpoint analysis.
 *)
type label_info_fixpoint = (ReachingDepSet.t * ReachingDepSet.t) label_info
[@@deriving sexp]

(**
   The state that will be maintained throughout the traversal of the Mir
   * label_ix: The next label that's free to use
   * label_info_map: The label information that's been built so far
   * possible_previous: The set of nodes that could have immediately preceded this point
     under some execution of the program
   * continues: A set of the continue nodes that have been encountered since exiting a loop
   * breaks: A set of the break nodes that have been encountered since exiting a loop
   * returns: A set of the return nodes that have been encountered
 *)
type traversal_state =
  { label_ix : label
  ; label_info_map : label_info_update LabelMap.t
  ; possible_previous : LabelSet.t
  ; continues : LabelSet.t
  ; breaks : LabelSet.t
  ; returns : LabelSet.t
  }

(**
   Initial traversal state, where 0 label represents the beginning of the function/block
 *)
let initial_trav_st =
  { label_ix = 1
  ; label_info_map = LabelMap.empty
  ; possible_previous = LabelSet.singleton 0
  ; continues = LabelSet.empty
  ; breaks = LabelSet.empty
  ; returns = LabelSet.empty
  }

(** The most recently nested control flow (block start, if/then, or loop)

    This isn't included in the traversal_state because it only flows downward through the
    tree, not across and up like everything else *)
type cf_state = label

let initial_cf_st = 0

(***********************************)
(* Expression helper functions     *)
(***********************************)

(**
   The set of variables in an expression, including inside an index

   For use in RHS sets, not LHS assignment sets, except in a target term
 *)
let rec expr_var_set (ex : expr) : ExprSet.t =
  let union_recur exprs = ExprSet.union_list (List.map exprs ~f:expr_var_set) in
  match ex with
  | Var _ as v -> ExprSet.singleton v
  | Lit _ -> ExprSet.empty
  | FunApp (_, exprs) -> union_recur exprs
  | BinOp (expr1, _, expr2) -> union_recur [expr1; expr2]
  | TernaryIf (expr1, expr2, expr3) -> union_recur [expr1; expr2; expr3]
  | Indexed (expr, ix) -> ExprSet.union_list (expr_var_set expr :: List.map ix ~f:index_var_set) (* TODO handle indices *)
and index_var_set (ix : index) : ExprSet.t =
  match ix with
  | All -> ExprSet.empty
  | Single expr -> expr_var_set expr
  | Upfrom expr -> expr_var_set expr
  | Downfrom expr -> expr_var_set expr
  | Between (expr1, expr2) -> ExprSet.union (expr_var_set expr1) (expr_var_set expr2)
  | MultiIndex expr -> expr_var_set expr

(**
   The variable being assigned to when `ex` is LHS
 *)
let expr_assigned_var (ex : expr) : expr =
  match ex with
  | Var _ as v -> v
  | Indexed (Var _ as v,_) -> v
  | _ -> raise (Failure "Unimplemented: analysis of assigning to non-var")


(***********************************)
(* Label and RD helper functions   *)
(***********************************)

(** Remove RDs corresponding to a variable *)
let filter_var_deps (deps : ReachingDepSet.t) (var : expr) : ReachingDepSet.t =
  ReachingDepSet.filter deps ~f:(fun (v, _) -> v <> var)

(** Union label maps, preserving the left element in a collision *)
let merge_label_maps (m1 : 'a LabelMap.t) (m2 : 'a LabelMap.t) : 'a LabelMap.t =
  let f ~key:_ opt = match opt with
    | `Left v -> Some v
    | `Right v -> Some v
    | `Both (v1, _) -> Some v1
  in LabelMap.merge m1 m2 ~f:f

(** Get the label of the next node to be assigned *)
let peek_next_label (st : traversal_state) : label =
  st.label_ix

(** Get a new label and update the state *)
let new_label (st : traversal_state) : (label * traversal_state) =
  (st.label_ix, {st with label_ix = st.label_ix + 1})

(**
   Modify the reaching dependency update functions of the possible set of previously
   executed nodes
 *)
let alter_last_rd
    (alter : ReachingDepSet.t -> ReachingDepSet.t)
    (trav_st : traversal_state)
  : traversal_state =
  let remove_rd label_info_opt = match label_info_opt with
    | None -> raise (Failure "traversal state's possible_previous refers to nonexistant labels")
    | Some label_info -> { label_info with
                           dep_sets = fun set -> alter (label_info.dep_sets set)
                         }
  in
  let remove_label_rd info_map label = LabelMap.update info_map label ~f:remove_rd in
  let label_info_map' =
    List.fold_left
      (LabelSet.to_list trav_st.possible_previous)
      ~f:remove_label_rd
      ~init:trav_st.label_info_map
  in
  { trav_st with label_info_map = label_info_map' }

(** The list of terms in expression *)
let rec summation_terms (rhs : expr) : expr list =
  match rhs with
  | BinOp (e1, Plus, e2) -> List.append (summation_terms e1) (summation_terms e2)
  | _ as e -> [e]

(** Apply function `f` to label_info for `label` in `trav_st` *)
let modify_label_info
    (trav_st : traversal_state)
    (label : label)
    (f : label_info_update -> label_info_update)
  : traversal_state =
  {trav_st with
   label_info_map =
     LabelMap.change
       trav_st.label_info_map
       label
       ~f:(function (*Option.map should exist but doesn't appear to*)
           | None -> None
           | Some info -> Some (f info))}

(**
   Traverse the Mir statement `st` to build up a final `traversal_state` value.

   See `traversal_state` and `cf_state` types for descriptions of the state.

   Traversal is done in a syntax-directed order, and builds a label_info values for each Mir node that could affect or
   read a variable.
*)
let rec accumulate_label_info
    (trav_st : traversal_state)
    (cf_st : cf_state)
    (st : stmt_loc)
  : traversal_state =
  match st.stmt with
  | Assignment (lhs, rhs) ->
    let (label, trav_st') = new_label trav_st in
    let info =
      { dep_sets =
          (fun entry ->
             let assigned_var = expr_assigned_var lhs
             in let this_assgn = ReachingDepSet.singleton (assigned_var, label)
             in ReachingDepSet.union (filter_var_deps entry assigned_var) this_assgn)
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = expr_var_set rhs
      ; controlflow =
          LabelSet.union_list
            [LabelSet.singleton cf_st; trav_st.continues; trav_st.returns]
      ; loc = st.sloc
      ; target_sum_terms =
          if lhs = Var "target" then
            Some (summation_terms rhs)
          else
            None
      }
    in
    { trav_st' with
      label_info_map =
        merge_label_maps trav_st'.label_info_map (LabelMap.singleton label info)
    ; possible_previous = LabelSet.singleton label
    }
  | NRFunApp _ -> trav_st
  | Check _ -> trav_st
  | MarkLocation _ -> trav_st
  | Break ->
    let (label, trav_st') = new_label trav_st in
    { trav_st' with breaks = LabelSet.add trav_st'.breaks label}
  | Continue ->
    let (label, trav_st') = new_label trav_st in
    { trav_st' with continues = LabelSet.add trav_st'.continues label}
  | Return _ ->
    let (label, trav_st') = new_label trav_st in
    { trav_st' with returns = LabelSet.add trav_st'.returns label}
  | Skip -> trav_st
  | IfElse (pred, then_stmt, else_stmt) ->
    let (label, trav_st') = new_label trav_st in
    let recurse_st = {trav_st' with possible_previous = LabelSet.singleton label} in
    let then_st = accumulate_label_info recurse_st label then_stmt in
    let else_st_opt = Option.map else_stmt ~f:(accumulate_label_info then_st label) in
    let info =
      { dep_sets = (fun entry -> entry) (* is this correct? *)
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = expr_var_set pred
      ; controlflow =
          LabelSet.union_list
            [LabelSet.singleton cf_st; trav_st.continues; trav_st.returns]
      ; loc = st.sloc
      ; target_sum_terms = None
      }
    in
    (match else_st_opt with
     | Some else_st ->
       { else_st with
         label_info_map =
           merge_label_maps else_st.label_info_map (LabelMap.singleton label info)
       ; possible_previous =
           LabelSet.union then_st.possible_previous else_st.possible_previous
       }
     | None ->
       { then_st with
         label_info_map =
           merge_label_maps then_st.label_info_map (LabelMap.singleton label info)
       ; possible_previous =
           LabelSet.union then_st.possible_previous trav_st'.possible_previous
       })
  | While (pred, body_stmt) ->
    let (label, trav_st') = new_label trav_st in
    let recurse_st = {trav_st' with possible_previous = LabelSet.singleton label} in
    let body_st = accumulate_label_info recurse_st label body_stmt in
    let loop_start_possible_previous =
      LabelSet.union_list
        [LabelSet.singleton label; body_st.possible_previous; body_st.continues] in
    let body_st' =
      modify_label_info
        body_st
        (peek_next_label recurse_st)
        (fun info -> {info with possible_previous = loop_start_possible_previous})
    in
    let info =
      { dep_sets = (fun entry -> entry) (* is this correct? *)
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = expr_var_set pred
      ; controlflow =
          LabelSet.union_list
            [ LabelSet.singleton cf_st
            ; trav_st.continues
            ; trav_st.returns
            ; body_st'.breaks]
      ; loc = st.sloc
      ; target_sum_terms = None
      }
    in
    { body_st' with
      label_info_map =
        merge_label_maps
          body_st'.label_info_map
          (LabelMap.singleton label info)
    ; possible_previous =
        LabelSet.union body_st'.possible_previous trav_st'.possible_previous
    ; continues = LabelSet.empty
    ; breaks = LabelSet.empty
    }
  | For args ->
    let (label, trav_st') = new_label trav_st in
    let recurse_st = {trav_st' with possible_previous = LabelSet.singleton label} in
    let body_st = accumulate_label_info recurse_st label args.body in
    let loop_start_possible_previous =
      LabelSet.union_list
        [LabelSet.singleton label; body_st.possible_previous; body_st.continues]
    in
    let body_st' =
      modify_label_info
        body_st
        (peek_next_label recurse_st)
        (fun info -> {info with possible_previous = loop_start_possible_previous})
    in
    let alter_fn = fun set -> ReachingDepSet.remove set (args.loopvar, label) in
    let body_st'' = alter_last_rd alter_fn body_st' in
    let info =
      { dep_sets =
          (fun entry ->
             ReachingDepSet.union entry
               (ReachingDepSet.singleton (args.loopvar, label)))
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = ExprSet.union (expr_var_set args.lower) (expr_var_set args.upper)
      ; controlflow =
          LabelSet.union_list
            [ LabelSet.singleton cf_st
            ; trav_st.continues
            ; trav_st.returns
            ; body_st''.breaks]
      ; loc = st.sloc
      ; target_sum_terms = None
      }
    in
    { body_st'' with
      label_info_map =
        merge_label_maps body_st''.label_info_map (LabelMap.singleton label info)
    ; possible_previous =
        LabelSet.union body_st''.possible_previous trav_st'.possible_previous
    ; continues = LabelSet.empty
    ; breaks = LabelSet.empty
    }
  | Block stmts ->
    let f state stmt = accumulate_label_info state cf_st stmt
    in List.fold_left stmts ~init:trav_st ~f:f
  | SList stmts ->
    let f state stmt = accumulate_label_info state cf_st stmt
    in List.fold_left stmts ~init:trav_st ~f:f
  | Decl args ->
    let (label, trav_st') = new_label trav_st in
    let info =
      { dep_sets =
          (let assigned_var = Var args.decl_id in
           let addition = ReachingDepSet.singleton (assigned_var, label) in
           fun entry ->
             ReachingDepSet.union addition (filter_var_deps entry assigned_var))
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = ExprSet.empty
      ; controlflow =
          LabelSet.union_list
            [LabelSet.singleton cf_st; trav_st.continues; trav_st.returns]
      ; loc = st.sloc
      ; target_sum_terms = None
      }
    in
    { trav_st' with
      label_info_map =
        merge_label_maps trav_st'.label_info_map (LabelMap.singleton label info)
    ; possible_previous = LabelSet.singleton label
    }
  | FunDef _ -> trav_st


(** Find the new value of the RD sets of a label, given the previous RD sets *)
let rd_update_label
    (label_info : label_info_update)
    (prev : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t)
  : (ReachingDepSet.t * ReachingDepSet.t) =
  let get_exit label = snd (LabelMap.find_exn prev label) in
  let from_prev =
    ReachingDepSet.union_list
      (List.map (Set.to_list label_info.possible_previous) ~f:get_exit)
  in
  (from_prev, label_info.dep_sets from_prev)

(** Find the new values of the RD sets, given the previous RD sets *)
let rd_apply
    (label_infos : label_info_update LabelMap.t)
    (prev : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t)
  : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t =
  let update_label ~key:(label : label) ~data:_ =
    let label_info = LabelMap.find_exn label_infos label in
    rd_update_label label_info prev
  in
  (if debug_verbose then
     let _ =
       (LabelMap.mapi prev
          ~f:(fun ~key:l ~data:d ->
              let u = update_label ~key:l ~data:d in
              print_endline ("rd_apply result " ^ (string_of_int l) ^ ": " ^
                             (Sexp.to_string
                                ([%sexp (u: ReachingDepSet.t * ReachingDepSet.t)])))));
     in ());
  LabelMap.mapi prev ~f:update_label

(** Find the fixpoint of a function and an initial value, given definition of equality *)
let rec apply_until_fixed (equal : 'a -> 'a -> bool) (f : 'a -> 'a) (x : 'a) : 'a =
  let y = f x in
  if equal x y
  then x
  else apply_until_fixed equal f y

(**
   Tests RD sets for equality.

   It turns out that doing = or == does not work for these types.
   = actually gives a *runtime* error.
 *)
let rd_equal
    (a : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t)
    (b : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t)
  : bool =
  let equal_set_pairs (a1, a2) (b1, b2) =
    ReachingDepSet.equal a1 b1 && ReachingDepSet.equal a2 b2
  in
  LabelMap.equal equal_set_pairs a b

(**
   Find the fixpoints of the dataflow update functions. Fixpoints should correspond to
   the full, correct dataflow graph.
*)
let rd_fixpoint (info : label_info_update LabelMap.t) : label_info_fixpoint LabelMap.t =
  let initial_sets =
    LabelMap.map info ~f:(fun _ -> (ReachingDepSet.empty, ReachingDepSet.empty))
  in
  let maps = apply_until_fixed rd_equal (rd_apply info) initial_sets in
  LabelMap.mapi
    maps
    ~f:(fun ~key:label ~data:ms ->
        {(LabelMap.find_exn info label) with dep_sets = ms})

let rec var_dependencies
    (label_info : label_info_fixpoint LabelMap.t)
    (visited : LabelSet.t)
    (possible_endpoints : LabelSet.t)
    (var : expr)
  : (ExprSet.t * LabelSet.t) =
  let s = match var with
    | Var s -> s
    | _ -> raise (Failure "Cannot currently check dependency of non-var expression") in
  if debug_verbose then
    (print_endline ("var_deps called on " ^ s ^ " " ^
                    (Sexp.to_string ([%sexp (possible_endpoints : LabelSet.t)]))));
  let endpoints_without_start = LabelSet.remove possible_endpoints 0 in
  let last_infos =
    List.map
      (LabelSet.to_list endpoints_without_start)
      ~f:(fun l -> LabelMap.find_exn label_info l)
  in
  let all_possible_assignments =
    ReachingDepSet.to_list
      (ReachingDepSet.union_list
         (List.map last_infos ~f:(fun i -> snd i.dep_sets)))
  in
  let var_possible_assignments =
    List.filter_map
      all_possible_assignments
      ~f:(fun (v, l) -> if v = var then Some l else None)
  in
  if debug_verbose then
    (print_endline ("calling label_deps on labels " ^
                    (Sexp.to_string ([%sexp (var_possible_assignments : label list)]))));
  let zero_dep =
    not (List.is_empty (List.filter var_possible_assignments ~f:(fun l -> l = 0)))
  in
  let (prev_expr_deps, prev_label_deps) =
    List.unzip
      (List.map
         (List.filter
            var_possible_assignments
            ~f:(fun l -> not (LabelSet.mem visited l)))
         ~f:(label_dependencies label_info visited))
  in
  let this_expr_dep =
    if (LabelSet.mem possible_endpoints 0
        || List.is_empty var_possible_assignments
        || zero_dep) then
      ExprSet.singleton var
    else
      ExprSet.empty
  in
  let result =
    ( ExprSet.union_list (this_expr_dep :: prev_expr_deps)
    , LabelSet.union_list prev_label_deps)
  in
  if debug_verbose then
    (print_endline (Sexp.to_string ([%sexp (result : (ExprSet.t * LabelSet.t))])));
  result

(**
   Find the set of labels for nodes whose RHS could affect the value of `label`.

   If `statistical_dependence` is off, the nodes corresponding to target terms will not be
   traversed, and the result will be the same as a classical dataflow analysis.
*)
and label_dependencies
    (label_info : label_info_fixpoint LabelMap.t)
    (visited : LabelSet.t)
    (label : label)
  : (ExprSet.t * LabelSet.t) =
  let visited' = LabelSet.add visited label in
  if debug_verbose then
    (print_string ("label_deps called on " ^ (string_of_int label) ^ "\n"));
  let this_info = LabelMap.find_exn label_info label in
  let (rhs_exprs, rhs_labels) =
    List.unzip
      (List.map
         (ExprSet.to_list this_info.rhs_set)
         ~f:(var_dependencies label_info visited' this_info.possible_previous))
  in
  let (cf_exprs, cf_labels) =
    List.unzip
      (List.map
         (LabelSet.to_list
            (LabelSet.filter
               this_info.controlflow
               ~f:(fun x -> x <> 0)))
         ~f:(label_dependencies label_info visited'))
  in
  ( ExprSet.union_list (rhs_exprs @ cf_exprs)
  , LabelSet.add (LabelSet.union_list (rhs_labels @ cf_labels)) label)

(**
   Collect all of the target terms, and the labels where they were added.

   It's important to know the labels because the variables in the expression could change
   of the course of execution.
  *)
let target_terms (label_info : ('s label_info) LabelMap.t) : (expr * label) list =
  let terms =
    LabelMap.fold
      label_info
      ~init:[]
      ~f:(fun ~key:l ~data:label_info accum ->
          List.map
            (Option.value ~default:[] label_info.target_sum_terms)
            ~f:(fun t -> (t, l)) @ accum)
  in
  List.rev (List.filter terms ~f:(fun (t,_) -> t <> Var "target"))

(**
   Append new nodes to the traversal_state that correspond to the effect that a target
   term has on the variables it involves.

   Each term node lists every other as a `possible_previous` node, because sampling
   considers them effectively simultaneously. Term nodes list their corresponding target
   increment node's control flow as their own.

   Term nodes can't be included in the normal flow of the graph, since the effect they
   have on parameters doesn't 'happen' until in between executions of the block. Instead,
   it works similarly to a while loop, with target terms at the end of the loop body.
 *)
let add_term_nodes (trav_st : traversal_state) : (traversal_state * LabelSet.t) =
  let add_term_node
      ((trav_st, nodes) : (traversal_state * LabelSet.t))
      ((term, inc_label) : (expr * label))
    : (traversal_state * LabelSet.t) =
    let (label, trav_st') = new_label trav_st in
    let target_inc_info = LabelMap.find_exn trav_st.label_info_map inc_label in
    let term_vars = expr_var_set term in
    let info =
      { dep_sets =
          (fun _ -> ReachingDepSet.of_list
              (List.map
                 (ExprSet.to_list term_vars)
                 ~f:(fun v -> (v, label))))
      ; possible_previous = target_inc_info.possible_previous
      ; rhs_set = term_vars
      ; controlflow = target_inc_info.controlflow
      ; loc = target_inc_info.loc
      ; target_sum_terms = None
      }
    in ( { trav_st' with
           label_info_map =
             merge_label_maps trav_st'.label_info_map (LabelMap.singleton label info) }
       , LabelSet.add nodes label
       )
  in
  let (trav_st', term_nodes) =
    List.fold_left
      (target_terms trav_st.label_info_map)
      ~init:(trav_st, LabelSet.empty)
      ~f:add_term_node
  in
  let add_mutual_dependence trav_st label =
    modify_label_info
      trav_st
      label
      (fun info ->
         {info with
          possible_previous = LabelSet.union info.possible_previous term_nodes})
  in
  (LabelSet.fold term_nodes ~init:trav_st' ~f:add_mutual_dependence, term_nodes)

(**
   Define 'node 0', the node representing the beginning of the block. This node 'assigns'
   global variables declared before execution of the block, and forwards along the
   effects of the term labels. This is analogous to the beginning of a loop, where
   execution could have come from before the loop or from the end of the loop.
 *)
let node_0
    (initial_declared : ExprSet.t)
    (term_labels : LabelSet.t)
  : label_info_update =
  { dep_sets = (fun entry ->
        ReachingDepSet.union entry
          (ReachingDepSet.of_list
             (List.map
                (ExprSet.to_list initial_declared)
                ~f:(fun v -> (v, 0)))))
  ; possible_previous = term_labels
  ; rhs_set = ExprSet.empty
  ; controlflow = LabelSet.empty
  ; loc = "Start of block"
  ; target_sum_terms = None
  }

(**
   Add node 0 to a traversal state, including variables declared outside the block and
   with dependence on all of the target term nodes.
 *)
let add_node_0
    (initial_declared : ExprSet.t)
    (term_labels : LabelSet.t)
    (trav_st : traversal_state)
  : traversal_state =
  let label_info = node_0 initial_declared term_labels in
  { trav_st with
    label_info_map =
      merge_label_maps trav_st.label_info_map (LabelMap.singleton 0 label_info) }

(**
   Add the accumulated set of return labels to the set of possible exit points
 *)
let add_return_exits
    (trav_st : traversal_state)
  : traversal_state =
  {trav_st with
     possible_previous = LabelSet.union trav_st.returns trav_st.possible_previous}

type dataflow_graph =
  {
    (* All of the information for each node *)
    label_info_map : label_info_fixpoint LabelMap.t
    (* The set of nodes that could have been the last to execute *)
  ; possible_exits : LabelSet.t
    (* The set of nodes corresponding to target terms *)
  ; target_term_nodes : LabelSet.t
  }
[@@deriving sexp]

(**
   Construct a dataflow graph for the block, given some preexisting (global?) variables
 *)
let block_dataflow_graph
    (model_block : stmt_loc)
    (preexisting_table : top_var_table)
  : dataflow_graph =
  let preexisting_vars = ExprSet.of_list
      (List.map
         ("target" :: Map.Poly.keys preexisting_table)
         ~f:(fun v -> Var v))
  in
  let accum_info = accumulate_label_info initial_trav_st initial_cf_st model_block in
  let accum_info' = add_return_exits accum_info in
  let possible_exits = accum_info'.possible_previous in
  let (accum_info'', term_labels) = add_term_nodes accum_info' in
  let accum_info''' = add_node_0 preexisting_vars term_labels accum_info'' in
  let label_info = rd_fixpoint accum_info'''.label_info_map in
  { label_info_map = label_info
  ; possible_exits = possible_exits
  ; target_term_nodes = term_labels
  }

(**
   Builds a dataflow graph from the model block and evaluates the label and global
   variable dependencies of the "y" variable, printing results to stdout.
 *)
let analysis_example (mir : stmt_loc prog) : dataflow_graph =
  let (var_table, model_block) = mir.modelb in
  let df_graph =
    block_dataflow_graph
      model_block
      var_table
  in
  let var = "y" in
  let (expr_deps, label_deps) =
    var_dependencies df_graph.label_info_map LabelSet.empty df_graph.possible_exits (Var var)
  in

  if demonstration_verbose then begin
    let preexisting_vars = ExprSet.of_list
        (List.map
           ("target" :: Map.Poly.keys var_table)
           ~f:(fun v -> Var v))
    in
    Sexp.pp_hum Format.std_formatter [%sexp (df_graph.label_info_map : label_info_fixpoint LabelMap.t)];
    print_string "\n\n";
    print_endline
      ("Pre-existing variables: " ^
       (Sexp.to_string ([%sexp (preexisting_vars : ExprSet.t)])));
    print_endline
      ("Possible endpoints: " ^
       (Sexp.to_string ([%sexp (df_graph.possible_exits : LabelSet.t)])));
    print_endline
      (var ^ " depends on labels: " ^
       (Sexp.to_string ([%sexp (label_deps : LabelSet.t)])));
    print_endline
      (var ^ " depends on global variables: " ^
       (Sexp.to_string ([%sexp (expr_deps : ExprSet.t)])));
  end;

  df_graph
let%test _ = 5 = 120
let%test _ = raise (Failure "ran test")

let%expect_test "Example program" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      "      model {\n\
      \              for (i in 1:2)\n\
      \                for (j in 3:4)\n\
      \                  print(\"Badger\");\n\
      \            }\n\
      \            "
  in
  let mir = Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast) in
  let (table, block) = mir.modelb in
  let df_graph = block_dataflow_graph block table in
  print_s [%sexp (df_graph : dataflow_graph)] ;
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      ((((stmt_untyped
          (For (loop_variable ((name i) (id_loc <opaque>)))
           (lower_bound
            ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>)))
           (upper_bound
            ((expr_untyped (IntNumeral 2)) (expr_untyped_loc <opaque>)))
           (loop_body
            ((stmt_untyped
              (For (loop_variable ((name j) (id_loc <opaque>)))
               (lower_bound
                ((expr_untyped (IntNumeral 3)) (expr_untyped_loc <opaque>)))
               (upper_bound
                ((expr_untyped (IntNumeral 4)) (expr_untyped_loc <opaque>)))
               (loop_body
                ((stmt_untyped (Print ((PString "\"Badger\""))))
                 (stmt_untyped_loc <opaque>)))))
             (stmt_untyped_loc <opaque>)))))
         (stmt_untyped_loc <opaque>)))))
     (generatedquantitiesblock ())) |}]


(**
   TODO
 * Alternative formulation for target additions:
   - Target addition statements have two types of contribution: affecting the target as a variable which takes effect inline, and adding a term which takes effect after the model block.
   - Term nodes should:
     - Include the target increment node's possible_previous as its own
     - Be listed as possible_previous for node 0
     - Include all of the term's variables on the RHS
     - Add each of the terms' parameter variables to the RD set
     - Not include any other variables in the RD set
     - Include all other terms as possible_previous?
   - These term nodes can be included in the fixpoint operation
 * Target terms only introduce fully connected dependency graph in some cases:
   x ~ normal(mu, sigma) introduces mu <-> sigma
   does
   theta ~ normal(mu, sigma) introduce mu <-> sigma if theta is not related to data?
   Is this true? Interacting with target might always change how the parameters is sampled..
   - Certainly only introduces dependencies onto parameters, not to anything else
 * Need to find functions in the fdblock, and inline them in the NRFunApp statements - but they should only effect target, everything else is scoped in a block
 * Still need to include target additions as dependencies for normal variables
   - Perform first fixpoint, use normal variable dependency analysis on target, insert additional labels for target sum terms, rerun fixpoint
   - Alternative, since target can only be incremented and not overwritten:
     - Provide a second rd_apply that considers all target_sum_terms as labels, or split the results into classical and statistical dependence
       - target_sum_terms should really always be labels, but ignored in the classic analysis
       - So, back to the original idea of iterating over terms in the traversal?
       - But, should they actually be able to depend on eachother? The dependency is only introduced between evaluations of the model block!
       - Does that mean term dependencies should actually be introduced in the initial rd set, along with the symbol table?
 * Does possible_previous for the beginning of a loop need to include the end of the loop?
   - Yes. I need to add the final label and continue labels to the possible_previous of the first label
   - How do I get access to the first label? Two ways:
     1. Introspect the label iteration process, lookup trav_st.label_ix+1
     2. Run the substatements once just to get the labels you need, then run it again with those added
   - Done #1
 * Indexed variables
 * Shadowing
 * Solve scoping issue in for-loops. Does this apply to declarations within blocks as well?
   - Shadowing won't really work with this scheme, I'm just leaving both local and outside alive
     - Wait, is that actually correct?
     - Not quite correct, instead you could map all shadowed values into a special 'shadowed' token in the exit of the for loop, and map it back in the exit of the body
   - Done for loops, needs to be done for scoped declarations
 * Non-local control flow should be added to the traversal state
   - Done, untested
 * Target assignments need to be split into one label for each summed term
   - Done, collected target terms in label_info of each target assignment, this is sufficient for depends analysis and prior partitioning



 * if function ends in st, it can change the target

 * write expect_test in file, found automatically, use dune runtest
 * use dune format and dune promote to use the actual output as a unit test

   val trans_prog : string -> Ast.typed_program -> Mir.stmt_loc Mir.prog

   val parse_string :
     (Lexing.position -> Ast.untyped_program Parser.MenhirInterpreter.checkpoint)
   -> string
   -> Ast.untyped_program
   (** A helper function to take a parser, a string and produce an AST. Under the
    hood, it takes care of Menhir's custom syntax error messages. *)

   val semantic_check_program : untyped_program -> typed_program
   (** Performs semantic check on AST and returns original AST embellished with type decorations *)
 **)
