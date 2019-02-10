open Core_kernel
open Mir

type label = int

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
  match ex with
  | Var _ as v -> ExprSet.singleton v
  | Lit _ -> ExprSet.empty
  | FunApp (_, exprs) ->
    List.fold_left (List.map exprs expr_var_set) ~init:ExprSet.empty ~f:ExprSet.union
  | BinOp (expr1, _, expr2) ->
    ExprSet.union (expr_var_set expr1) (expr_var_set expr2)
  | TernaryIf (expr1, expr2, expr3) ->
    ExprSet.union (ExprSet.union (expr_var_set expr1) (expr_var_set expr2)) (expr_var_set expr3)
  | Indexed _ as i -> ExprSet.singleton i

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

type label_info =
  { dep_sets_update : ReachingDepSet.t -> ReachingDepSet.t
  ; possible_previous : LabelSet.t
  ; rhs_set : ExprSet.t
  ; controlflow : LabelSet.t
  ; loc : string
  }

(* This is the state that's accumulated forward through the traversal *)
type traversal_state =
  { label_ix : label
  ; label_info_map : label_info LabelMap.t
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

let new_label (st : traversal_state) : (label * traversal_state) =
  (st.label_ix, {st with label_ix = st.label_ix + 1})

(* This is the state that only flows downward into the tree *)
type stack_state = label

let alter_last_rd (last : LabelSet.t) (alter : ReachingDepSet.t -> ReachingDepSet.t) (trav_st : traversal_state) : traversal_state =
  let remove_rd label_info_opt = match label_info_opt with
    | None -> raise (Failure "traversal state's possible_previous refers to nonexistant labels")
    | Some label_info -> { label_info with
                           dep_sets_update = fun set -> alter (label_info.dep_sets_update set)
                         }
  in let remove_label_rd info_map label = LabelMap.update info_map label ~f:remove_rd
  in let label_info_map' = List.fold_left (LabelSet.to_list last) ~f:remove_label_rd ~init:trav_st.label_info_map
  in { trav_st with label_info_map = label_info_map' }

(* Ignoring non-local control flow (break, continue, return) for now *)
let rec accumulate_label_info (trav_st : traversal_state) (stack_st : stack_state) (st : stmt_loc) : traversal_state =
  match st.stmt with
  | Assignment (lhs, rhs) ->
    let (label, trav_st') = new_label trav_st in
    let info =
      { dep_sets_update =
        (let assigned_var = expr_assigned_var lhs
         in let addition = ReachingDepSet.singleton (assigned_var, label)
                (* below doesn't work for indexed expressions *)
         in fun entry -> ReachingDepSet.union addition (filter_var_deps entry assigned_var))
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = expr_var_set rhs
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns]
      ; loc = st.sloc
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
      { dep_sets_update = (fun entry -> entry) (* is this correct? *)
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = expr_var_set pred
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns]
      ; loc = st.sloc
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
    let info =
      { dep_sets_update = (fun entry -> entry) (* is this correct? *)
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = expr_var_set pred
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns; body_st.breaks]
      ; loc = st.sloc
      }
    in { body_st with
         label_info_map = merge_label_maps body_st.label_info_map (LabelMap.singleton label info)
       ; possible_previous = LabelSet.union body_st.possible_previous trav_st'.possible_previous
       ; continues = LabelSet.empty
       ; breaks = LabelSet.empty
       }
  | For args ->
    let (label, trav_st') = new_label trav_st in
    let recurse_st = {trav_st' with possible_previous = LabelSet.singleton label} in
    let body_st = accumulate_label_info recurse_st label args.body in
    let alter_fn = fun set -> ReachingDepSet.remove set (args.loopvar, label) in
    let body_st' = alter_last_rd body_st.possible_previous alter_fn body_st in
    let info =
      (** How should limited variable scope be handled? Is it sufficient to note a for loop as control flow effecting its body, instead of including the temporary variable in the set? This introduces questions of correct shadowing.
      SOLUTION:
       1. remove (loopvar, label) from the exit function of the final statement.
       2. at declaration, remove variables that are shadowed by the loopvar. in the exit func, add them back.
      This will be similar to limited-scope declarations within blocks.
       **)
      { dep_sets_update = (fun entry -> ReachingDepSet.union entry (ReachingDepSet.singleton (args.loopvar, label)))
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = ExprSet.union (expr_var_set args.lower) (expr_var_set args.upper)
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns; body_st.breaks]
      ; loc = st.sloc
      }
    in { body_st' with
         label_info_map = merge_label_maps body_st'.label_info_map (LabelMap.singleton label info)
       ; possible_previous = LabelSet.union body_st'.possible_previous trav_st'.possible_previous
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
      { dep_sets_update =
        (let assigned_var = Var args.decl_id
         in let addition = ReachingDepSet.singleton (assigned_var, label)
         in fun entry -> ReachingDepSet.union addition (filter_var_deps entry assigned_var))
      ; possible_previous = trav_st'.possible_previous
      ; rhs_set = ExprSet.empty
      ; controlflow = LabelSet.union_list [LabelSet.singleton stack_st; trav_st.continues; trav_st.returns]
      ; loc = st.sloc
      }
    in { trav_st' with
         label_info_map = merge_label_maps trav_st'.label_info_map (LabelMap.singleton label info)
       ; possible_previous = LabelSet.singleton label
       }
  | FunDef args -> trav_st

(*
type label_info =
  { dep_sets_update : ReachingDepSet.t -> ReachingDepSet.t
  ; possible_previous : LabelSet.t
  ; rhs_set : ExprSet.t
  ; controlflow : LabelSet.t
  }

(* This is the state that's accumulated forward through the traversal *)
type traversal_state =
  { label_ix : label
  ; label_info_map : label_info LabelMap.t
  ; possible_previous : LabelSet.t
  }
*)

let rd_update_label (label : label) (label_info : label_info) (prev : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t) : (ReachingDepSet.t * ReachingDepSet.t) =
  let get_exit label = match LabelMap.find prev label with
                     | None -> ReachingDepSet.empty
                     | Some (_, exit_set) ->  exit_set
  in let from_prev = ReachingDepSet.union_list (List.map (Set.to_list label_info.possible_previous) get_exit)
  in (from_prev, label_info.dep_sets_update from_prev)

let rd_apply (label_infos : label_info LabelMap.t) (prev : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t) : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t =
  let update_label ~key:(label : label) ~data:_ =
    let label_info = LabelMap.find_exn label_infos label
    in rd_update_label label label_info prev
  in LabelMap.mapi prev ~f:update_label

(* using y = x instead of compare x y = 0 gives a runtime(!?) error: "compare: functional value"*)
let rec apply_until_fixed (f : 'a -> 'a) (x : 'a) : 'a =
  let y = f x
  in if compare y x = 0
     then x
     else apply_until_fixed f y

let rd_fixpoint (info : label_info LabelMap.t) : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t =
  let starting_sets = LabelMap.map info ~f:(fun _ -> (ReachingDepSet.empty, ReachingDepSet.empty))
  in apply_until_fixed (rd_apply info) starting_sets

let analysis (mir : stmt_loc prog) : (ReachingDepSet.t * ReachingDepSet.t) LabelMap.t =
  let model_block = snd mir.datab
  in let accum_info = accumulate_label_info initial_trav_st initial_stack_st model_block
  in let label_info = accum_info.label_info_map
  in let result = rd_fixpoint label_info
  in let _ = LabelMap.mapi label_info ~f:(fun ~key:l ~data:i -> print_string ((string_of_int l) ^ ": " ^ i.loc ^ "\n"))
  in result

(**
TODO
 * Solve scoping issue in for-loops. Does this apply to declarations within blocks as well?
   - Shadowing won't really work with this scheme, I'm just leaving both local and outside alive
   - Done for loops, needs to be done for scoped declarations
 * Non-local control flow should be added to the traversal state
   - Done, untested
 * Target assignments need to be split into one label for each summed term
**)
