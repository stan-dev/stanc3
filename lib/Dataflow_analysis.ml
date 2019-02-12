open Core_kernel
open Mir

type label = int
[@@deriving sexp, hash, compare]

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

let rec expr_var_set (ex : expr) : ExprSet.t =
  let union_recur exprs = ExprSet.union_list (List.map exprs expr_var_set) in
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
  | Between (expr1, expr2) -> ExprSet.union (expr_var_set expr1) (expr_var_set expr1)
  | MultiIndex expr -> expr_var_set expr

let rec expr_assigned_var (ex : expr) : expr =
  match ex with
  | Var _ as v -> v
  | Indexed (Var _ as v,_) -> v
  | _ -> raise (Failure "Unimplemented: analysis of assigning to non-var")

type reaching_dep = (expr * int)
[@@deriving sexp, hash, compare]

module ReachingDepSet = Set.Make(struct
    type t = reaching_dep
    let compare : reaching_dep -> reaching_dep -> int = compare
    let sexp_of_t = sexp_of_reaching_dep
    let t_of_sexp = reaching_dep_of_sexp
  end)

let filter_var_deps (deps : ReachingDepSet.t) (var : expr) : ReachingDepSet.t =
  ReachingDepSet.filter deps ~f:(fun (v, label) -> v <> var)

let merge_label_maps (m1 : 'a LabelMap.t) (m2 : 'a LabelMap.t) : 'a LabelMap.t =
  let f ~key:key opt = match opt with
        | `Left v -> Some v
        | `Right v -> Some v
        | `Both (v1, v2) -> Some v1
  in LabelMap.merge m1 m2 ~f:f

type 'dep label_info =
  { dep_sets : 'dep
  ; possible_previous : LabelSet.t
  ; rhs_set : ExprSet.t
  ; controlflow : LabelSet.t
  ; loc : string
  ; target_sum_terms : (expr list) option
  }
[@@deriving sexp]

type label_info_update = (ReachingDepSet.t -> ReachingDepSet.t) label_info

type label_info_fixpoint = (ReachingDepSet.t * ReachingDepSet.t) label_info
[@@deriving sexp]

(* This is the state that's accumulated forward through the traversal *)
type traversal_state =
  { label_ix : label
  ; label_info_map : label_info_update LabelMap.t
  ; possible_previous : LabelSet.t
  ; continues : LabelSet.t
  ; breaks : LabelSet.t
  ; returns : LabelSet.t
  }

let initial_trav_st =
  { label_ix = 1
  ; label_info_map = LabelMap.empty
  ; possible_previous = LabelSet.singleton 0
  ; continues = LabelSet.empty
  ; breaks = LabelSet.empty
  ; returns = LabelSet.empty
  }

let initial_stack_st = 0

let peek_next_label (st : traversal_state) : label =
  st.label_ix + 1

let new_label (st : traversal_state) : (label * traversal_state) =
  (st.label_ix, {st with label_ix = st.label_ix + 1})

(* This is the state that only flows downward into the tree *)
type stack_state = label

let alter_last_rd (last : LabelSet.t) (alter : ReachingDepSet.t -> ReachingDepSet.t) (trav_st : traversal_state) : traversal_state =
  let remove_rd label_info_opt = match label_info_opt with
    | None -> raise (Failure "traversal state's possible_previous refers to nonexistant labels")
    | Some label_info -> { label_info with
                           dep_sets = fun set -> alter (label_info.dep_sets set)
                         }
  in let remove_label_rd info_map label = LabelMap.update info_map label ~f:remove_rd
  in let label_info_map' = List.fold_left (LabelSet.to_list last) ~f:remove_label_rd ~init:trav_st.label_info_map
  in { trav_st with label_info_map = label_info_map' }

let rec summation_terms (rhs : expr) : expr list =
  match rhs with
  | BinOp (e1, Plus, e2) -> List.append (summation_terms e1) (summation_terms e2)
  | _ as e -> [e]

let rec summation_term_members (rhs : expr) : ExprSet.t list =
  match rhs with
  | Var _ as v -> [ExprSet.singleton v]
  | Lit _ -> [ExprSet.empty]
  | FunApp (_, exprs) -> [ExprSet.union_list (List.map exprs ~f:expr_var_set)]
  | BinOp (e1, Plus, e2) -> List.append (summation_term_members e1) (summation_term_members e2)
  | BinOp (e1, _, e2) -> [ExprSet.union_list (List.map [e1; e2] ~f:expr_var_set)]
  | TernaryIf (e1, e2, e3) -> [ExprSet.union_list (List.map [e1; e2; e3] ~f:expr_var_set)]
  | Indexed (Var _ as v, _) -> [ExprSet.singleton v]
  | Indexed _ -> raise (Failure "Unimplemented: complex indexed expression in summation")

let target_term_node (stack_st : stack_state) (loc : string) (trav_st : traversal_state) (term_members : ExprSet.t) =
  let (label, trav_st') = new_label trav_st in
  let rd_additions = ReachingDepSet.of_list (List.map (ExprSet.to_list term_members) ~f:(fun m -> (m, label))) in
  let info =
    { dep_sets = (fun entry -> ReachingDepSet.union entry rd_additions)
    ; possible_previous = trav_st'.possible_previous
    ; rhs_set = term_members
    ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns]
    ; loc = loc
    ; target_sum_terms = None
    }
  in { trav_st' with
       label_info_map = merge_label_maps trav_st'.label_info_map (LabelMap.singleton label info)
     ; possible_previous = LabelSet.singleton label
     }

(* This is missing from Core.Option (!)*)
let option_map (a : 'a option) (f : 'a -> 'b) : 'b option =
  match a with
  | None -> None
  | Some a -> Some (f a)

let modify_label_info (trav_st : traversal_state) (label : label) (f : label_info_update -> label_info_update) : traversal_state =
  {trav_st with label_info_map = LabelMap.change trav_st.label_info_map label (fun info_opt -> option_map info_opt f)}

(* Ignoring non-local control flow (break, continue, return) for now *)
let rec accumulate_label_info (trav_st : traversal_state) (stack_st : stack_state) (st : stmt_loc) : traversal_state =
  match st.stmt with
  | Assignment (lhs, rhs) ->
    let (label, trav_st') = new_label trav_st in
    let info =
      { dep_sets =
                (* below doesn't work for indexed expressions *)
        (fun entry ->
         let assigned_var = expr_assigned_var lhs
         in let this_assgn = ReachingDepSet.singleton (assigned_var, label)
         in ReachingDepSet.union (filter_var_deps entry assigned_var) this_assgn)
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = expr_var_set rhs
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns]
      ; loc = st.sloc
      ; target_sum_terms = if lhs = Var "target" then Some (summation_terms rhs) else None
      }
    in { trav_st' with
         label_info_map = merge_label_maps trav_st'.label_info_map (LabelMap.singleton label info)
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
    let else_st_opt = Option.map else_stmt (accumulate_label_info then_st label) in
    let info =
      { dep_sets = (fun entry -> entry) (* is this correct? *)
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = expr_var_set pred
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns]
      ; loc = st.sloc
      ; target_sum_terms = None
      }
    in (match else_st_opt with
      | Some else_st ->
        { else_st with
          label_info_map = merge_label_maps else_st.label_info_map (LabelMap.singleton label info)
        ; possible_previous = LabelSet.union then_st.possible_previous else_st.possible_previous
        }
      | None ->
        { then_st with
          label_info_map = merge_label_maps then_st.label_info_map (LabelMap.singleton label info)
        ; possible_previous = LabelSet.union then_st.possible_previous trav_st'.possible_previous
        })
  | While (pred, body_stmt) ->
    let (label, trav_st') = new_label trav_st in
    let recurse_st = {trav_st' with possible_previous = LabelSet.singleton label} in
    let body_st = accumulate_label_info recurse_st label body_stmt in
    let loop_start_possible_previous = LabelSet.union_list [LabelSet.singleton label; body_st.possible_previous; body_st.continues] in
    let body_st' = modify_label_info body_st (peek_next_label recurse_st) (fun info -> {info with possible_previous = loop_start_possible_previous}) in
    let info =
      { dep_sets = (fun entry -> entry) (* is this correct? *)
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = expr_var_set pred
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns; body_st'.breaks]
      ; loc = st.sloc
      ; target_sum_terms = None
      }
    in { body_st' with
         label_info_map = merge_label_maps body_st'.label_info_map (LabelMap.singleton label info)
       ; possible_previous = LabelSet.union body_st'.possible_previous trav_st'.possible_previous
       ; continues = LabelSet.empty
       ; breaks = LabelSet.empty
       }
  | For args ->
    let (label, trav_st') = new_label trav_st in
    let recurse_st = {trav_st' with possible_previous = LabelSet.singleton label} in
    let body_st = accumulate_label_info recurse_st label args.body in
    let loop_start_possible_previous = LabelSet.union_list [LabelSet.singleton label; body_st.possible_previous; body_st.continues] in
    let body_st' = modify_label_info body_st (peek_next_label recurse_st) (fun info -> {info with possible_previous = loop_start_possible_previous}) in
    let alter_fn = fun set -> ReachingDepSet.remove set (args.loopvar, label) in
    let body_st'' = alter_last_rd body_st'.possible_previous alter_fn body_st' in
    let info =
      (** How should limited variable scope be handled? Is it sufficient to note a for loop as control flow effecting its body, instead of including the temporary variable in the set? This introduces questions of correct shadowing.
      SOLUTION:
       1. remove (loopvar, label) from the exit function of the final statement.
       2. at declaration, remove variables that are shadowed by the loopvar. in the exit func, add them back.
      This will be similar to limited-scope declarations within blocks.
       **)
      { dep_sets = (fun entry -> ReachingDepSet.union entry (ReachingDepSet.singleton (args.loopvar, label)))
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = ExprSet.union (expr_var_set args.lower) (expr_var_set args.upper)
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns; body_st''.breaks]
      ; loc = st.sloc
      ; target_sum_terms = None
      }
    in { body_st'' with
         label_info_map = merge_label_maps body_st''.label_info_map (LabelMap.singleton label info)
       ; possible_previous = LabelSet.union body_st''.possible_previous trav_st'.possible_previous
       ; continues = LabelSet.empty
       ; breaks = LabelSet.empty
       }
  | Block stmts ->
    let f state stmt = accumulate_label_info state stack_st stmt
    in List.fold_left stmts ~init:trav_st ~f:f
  | SList stmts ->
    let f state stmt = accumulate_label_info state stack_st stmt
    in List.fold_left stmts ~init:trav_st ~f:f
  | Decl args ->
    let (label, trav_st') = new_label trav_st in
    let info =
      { dep_sets =
        (let assigned_var = Var args.decl_id
         in let addition = ReachingDepSet.singleton (assigned_var, label)
         in fun entry -> ReachingDepSet.union addition (filter_var_deps entry assigned_var))
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = ExprSet.empty
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns]
      ; loc = st.sloc
      ; target_sum_terms = None
      }
    in { trav_st' with
         label_info_map = merge_label_maps trav_st'.label_info_map (LabelMap.singleton label info)
       ; possible_previous = LabelSet.singleton label
       }
  | FunDef args -> trav_st

let rd_update_label (starting_rds : ReachingDepSet.t) (label : label) (label_info : label_info_update) (prev : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t) : (ReachingDepSet.t * ReachingDepSet.t) =
  let get_exit label = match label with
    | 0 -> starting_rds
    | l -> snd (LabelMap.find_exn prev label)
  in let from_prev = ReachingDepSet.union_list (List.map (Set.to_list label_info.possible_previous) get_exit)
  in (from_prev, label_info.dep_sets from_prev)

let rd_apply (starting_rds : ReachingDepSet.t) (label_infos : label_info_update LabelMap.t) (prev : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t) : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t =
  (*let endpoint_rds = ReachingDepSet.union_list (List.map (LabelSet.to_list (LabelSet.remove possible_endpoints 0)) ~f:(fun l -> snd (LabelMap.find_exn prev l))) in*)
  let update_label ~key:(label : label) ~data:_ =
    let label_info = LabelMap.find_exn label_infos label
    in rd_update_label starting_rds label label_info prev
 (* in let _ = print_endline ("rd_apply result " ^ (Sexp.to_string ([%sexp (LabelMap.mapi prev ~f:update_label : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t)])))*)
  in let _ = LabelMap.mapi prev ~f:(fun ~key:l ~data:d -> let u = update_label ~key:l ~data:d in print_endline ("rd_apply result " ^ (string_of_int l) ^ ": " ^ (Sexp.to_string ([%sexp (u: ReachingDepSet.t * ReachingDepSet.t)]))))
  in LabelMap.mapi prev ~f:update_label

(* using y = x instead of compare x y = 0 gives a runtime(!?) error: "compare: functional value"*)
let rec apply_until_fixed (equal : 'a -> 'a -> bool) (f : 'a -> 'a) (x : 'a) : 'a =
  let y = f x
  in if equal x y
     then x
     else apply_until_fixed equal f y

let rd_equal (a : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t) (b : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t) : bool =
  let equal_set_pairs (a1, a2) (b1, b2) = ReachingDepSet.equal a1 b1 && ReachingDepSet.equal a2 b2
  in LabelMap.equal equal_set_pairs a b

let rd_fixpoint (starting_rds : ReachingDepSet.t) (possible_endpoints : LabelSet.t) (info : label_info_update LabelMap.t) : label_info_fixpoint LabelMap.t =
  let initial_sets = LabelMap.map info ~f:(fun _ -> (ReachingDepSet.empty, ReachingDepSet.empty))
  in let maps = apply_until_fixed rd_equal (rd_apply starting_rds info) initial_sets
  in LabelMap.mapi maps ~f:(fun ~key:label ~data:ms -> {(LabelMap.find_exn info label) with dep_sets = ms})

let rec var_dependencies (label_info : label_info_fixpoint LabelMap.t) (possible_endpoints : LabelSet.t) (var : expr) : (ExprSet.t * LabelSet.t) =
  let (Var s) = var in
  let _ = print_endline ("var_deps called on " ^ s ^ " " ^ (Sexp.to_string ([%sexp (possible_endpoints : LabelSet.t)]))) in
  let endpoints_without_start = LabelSet.remove possible_endpoints 0 in
  let last_infos = List.map (LabelSet.to_list endpoints_without_start) ~f:(fun l -> LabelMap.find_exn label_info l) in
  let all_possible_assignments = ReachingDepSet.to_list (ReachingDepSet.union_list (List.map last_infos ~f:(fun i -> snd i.dep_sets))) in
  let var_possible_assignments = List.filter_map all_possible_assignments ~f:(fun (v, l) -> if v = var then Some l else None) in
  let _ = print_endline ("calling label_deps on labels " ^ (Sexp.to_string ([%sexp (var_possible_assignments : label list)]))) in
  let zero_dep = not (List.is_empty (List.filter var_possible_assignments ~f:(fun l -> l = 0))) in
  let (prev_expr_deps, prev_label_deps) = List.unzip (List.map (List.filter var_possible_assignments ~f:(fun l -> l <> 0)) ~f:(label_dependencies label_info)) in
  let this_expr_dep = if (LabelSet.mem possible_endpoints 0 || List.is_empty var_possible_assignments || zero_dep)
                      then ExprSet.singleton var else ExprSet.empty in
  let result = (ExprSet.union_list (this_expr_dep :: prev_expr_deps), LabelSet.union_list prev_label_deps) in
  let _ = print_endline (Sexp.to_string ([%sexp (result : (ExprSet.t * LabelSet.t))])) in
  result

and label_dependencies (label_info : label_info_fixpoint LabelMap.t) (label : label) : (ExprSet.t * LabelSet.t) =
  let _ = print_string ("label_deps called on " ^ (string_of_int label) ^ "\n") in
  let this_info = LabelMap.find_exn label_info label in
  let (rhs_exprs, rhs_labels) = List.unzip (List.map (ExprSet.to_list this_info.rhs_set) ~f:(var_dependencies label_info this_info.possible_previous)) in
  let (cf_exprs, cf_labels) = List.unzip (List.map (LabelSet.to_list (LabelSet.filter this_info.controlflow ~f:(fun x -> x <> 0))) ~f:(label_dependencies label_info)) in
  (ExprSet.union_list (rhs_exprs @ cf_exprs), LabelSet.add (LabelSet.union_list (rhs_labels @ cf_labels)) label)

let target_analysis (label_info : label_info_fixpoint LabelMap.t) (possible_endpoints : LabelSet.t) : expr list =
  let (_, label_deps) = var_dependencies label_info possible_endpoints (Var "target") in
  let term_opts = List.map (LabelSet.to_list label_deps) ~f:(fun l -> let info = LabelMap.find_exn label_info l in info.target_sum_terms) in
  let terms = List.concat (List.concat_map term_opts ~f:(fun term_opt -> Option.value_map term_opt ~default:[] ~f:(fun x -> [x]))) in
  List.filter terms ~f:(fun t -> t <> Var "target")

let target_terms (label_info : label_info_fixpoint LabelMap.t) : (expr * label) list =
  let terms = LabelMap.fold label_info ~init:[] ~f:(fun ~key:l ~data:label_info accum -> List.map (Option.value ~default:[] label_info.target_sum_terms) ~f:(fun t -> (t, l)) @ accum)
  in List.filter terms ~f:(fun (t,_) -> t <> Var "target")

let rec var_statistical_dependencies' (label_info : label_info_fixpoint LabelMap.t) (target_terms : (expr * label) list) (var : expr) (already_explored : ExprSet.t) : (ExprSet.t * LabelSet.t) =
  let other_vars (expr, label) =
    let var_set = expr_var_set expr
    in if ExprSet.mem var_set var
       then List.map (ExprSet.to_list (ExprSet.remove var_set var)) ~f:(fun v -> (v, label))
       else []
  in let var_pairs = List.concat_map target_terms ~f:other_vars
  in let (var_exprs, var_labels) = List.unzip (List.map var_pairs ~f:(fun (v, label) -> label_dependencies label_info label))
  in let (stat_exprs, stat_labels) = List.unzip (List.map var_pairs ~f:(fun (v, label) -> var_statistical_dependencies label_info target_terms v))
  in (ExprSet.union_list (var_exprs @ stat_exprs), LabelSet.union_list (var_labels @ stat_labels))

let rec var_statistical_dependencies (label_info : label_info_fixpoint LabelMap.t) (target_terms : (expr * label) list) (var : expr) : (ExprSet.t * LabelSet.t) =
  let other_vars (expr, label) =
    let var_set = expr_var_set expr
    in if ExprSet.mem var_set var
       then List.map (ExprSet.to_list (ExprSet.remove var_set var)) ~f:(fun v -> (v, label))
       else []
  in let var_pairs = List.concat_map target_terms ~f:other_vars
  in let (var_exprs, var_labels) = List.unzip (List.map var_pairs ~f:(fun (v, label) -> label_dependencies label_info label))
  in let (stat_exprs, stat_labels) = List.unzip (List.map var_pairs ~f:(fun (v, label) -> var_statistical_dependencies label_info target_terms v))
  in (ExprSet.union_list (var_exprs @ stat_exprs), LabelSet.union_list (var_labels @ stat_labels))

(*
type 'dep label_info =
  { dep_sets : 'dep
  ; possible_previous : LabelSet.t
  ; rhs_set : ExprSet.t
  ; controlflow : LabelSet.t
  ; loc : string
  ; target_sum_terms : (expr list) option
  }
[@@deriving sexp]
*)

let add_term_nodes (trav_st : traversal_state) : traversal_state =
  let add_term_node ((trav_st, nodes) : (traversal_state * LabelSet.t)) ((term, label) : (expr * inc_label)) : (traversal_state, LabelSet.t) =
    let (label, trav_st') = new_label trav_st in
    let target_inc_info = LabelMap.find_exn trav_st.label_info_map inc_label in
    let term_vars = expr_var_set term in
    let label_info =
      { dep_sets = (fun entry -> ReachingDepSet.of_list (List.map (ExprSet.to_list term_vars) ~f:(fun v -> (v, label))))
      ; possible_previous = target_inc_info.possible_previous
      ; rhs_set = term_vars
      ; controlflow = target_inc_info.controlflow (* could also be [inc_label] *)
      ; loc = target_inc_info.loc
      ; target_sum_terms = None
      }
    in ( { trav_st' with label_info_map = merge_label_maps trav_st'.label_info_map (LabelMap.singleton label info) }
       , LabelSet.add nodes label
       )
  in let (trav_st', term_nodes) = List.fold_left (target_terms accum_info.label_info) ~init:trav_st ~f:add_term_node
  in LabelSet.fold term_nodes ~init:trav_st ~f:(fun l -> modify_label_info)



let analysis (mir : stmt_loc prog) : label_info_fixpoint LabelMap.t =
  let (var_table, model_block) = mir.modelb
  in let starting_vars = "target" :: Map.Poly.keys var_table
  in let starting_rds = ReachingDepSet.of_list (List.map starting_vars ~f:(fun name -> (Var name, 0)))
  in let accum_info = accumulate_label_info initial_trav_st initial_stack_st model_block
  in let label_info = rd_fixpoint starting_rds accum_info.possible_previous accum_info.label_info_map
  (*in let _ = LabelMap.mapi label_info ~f:(fun ~key:l ~data:i -> print_string ((string_of_int l) ^ ": " ^ i.loc ^ "\n"))*)
  in let _ = Sexp.pp_hum Format.std_formatter [%sexp (label_info : label_info_fixpoint LabelMap.t)];
  in let _ = print_string "\n\n"
  in let deps = var_dependencies label_info accum_info.possible_previous (Var "y")
  in let _ = Sexp.pp_hum Format.std_formatter [%sexp (deps : (ExprSet.t * LabelSet.t))];
  in let t_terms = target_terms label_info
  in let _ = print_endline ("Target terms: " ^ (Sexp.to_string ([%sexp (t_terms : (expr * label) list)])))
  in let stat_deps = var_statistical_dependencies label_info t_terms (Var "y")
  in let _ = print_string "\n\n"
  in let _ = Sexp.pp_hum Format.std_formatter [%sexp (stat_deps : (ExprSet.t * LabelSet.t))];
  in let _ = print_string "\n\n"
  in label_info

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

**)
