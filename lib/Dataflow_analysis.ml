open Core_kernel
open Mir

module LabelMap = Map.Make(
  struct
    type t = int
    let compare : int -> int -> int = compare
    let sexp_of_t = sexp_of_int
    let t_of_sexp = int_of_sexp
  end)

module LabelSet = Set.Make(
  struct
    type t = int
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

type stmt_labeled =
  { slabel: int option
  ; sloc: string sexp_opaque [@compare.ignore]
  ; stmt: stmt_labeled statement
  }
[@@deriving sexp, hash]

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
  ReachingDepSet.filter deps ~f:(fun (v, label) -> v = var)

type label_dep_sets =
  { entry_set : ReachingDepSet.t
  ; exit_set : ReachingDepSet.t
  }
[@@deriving sexp, compare]

type label_info =
  { dep_sets : label_dep_sets
  ; rhs_set : ExprSet.t
  }
[@@deriving sexp, compare]

type dep_sets_map = label_dep_sets LabelMap.t

let merge_label_maps (m1 : 'a LabelMap.t) (m2 : 'a LabelMap.t) : 'a LabelMap.t =
  let f ~key:key opt = match opt with
        | `Left v -> None
        | `Right v -> None
        | `Both (v1, v2) -> None
  in LabelMap.merge m1 m2 ~f:f

type label_accum_info =
  { dep_sets_update : ReachingDepSet.t -> ReachingDepSet.t
  ; transfer_to : LabelSet.t
  ; rhs_set : ExprSet.t
  }

let rec accumulate_label_info (st : stmt_labeled) : label_accum_info LabelMap.t =
  match st.slabel with
  | None -> LabelMap.empty
  | Some label -> match st.stmt with
  | Assignment (lhs, rhs) as stmt ->
    let info =
      { dep_sets_update =
        (let assigned_var = expr_assigned_var lhs
         in let addition = ReachingDepSet.singleton (assigned_var, label)
         in fun entry -> ReachingDepSet.union addition (filter_var_deps entry assigned_var))
      ; transfer_to = LabelSet.empty
      ; rhs_set = expr_var_set rhs
      }
    in LabelMap.singleton label info
  | Break as stmt ->
    let info =
      { (* the LHS of a break should be all of the LHSs of the whole loop *)
        dep_sets_update = (fun entry -> entry)
        (* the beginning of the loop or outside of the loop *)
      ; transfer_to = LabelSet.empty
        (* the RHS of a break should be variables effecting the control flow to get to the break *)
      ; rhs_set = ExprSet.empty
      }
    in LabelMap.singleton label info
  | Continue as stmt ->
    let info =
      { (* the LHS of a continue should be all of the LHSs of the loop after the continue *)
        dep_sets_update = (fun entry -> entry)
        (* the beginning of the loop *)
      ; transfer_to = LabelSet.empty
        (* the RHS of a continue should be variables effecting the control flow to get to the continue *)
      ; rhs_set = ExprSet.empty
      }
    in LabelMap.singleton label info
  | Return expr as stmt ->
    (* what about an optional return?
       does it need to influence all of the statements after it in a function, like continue? *)
    let info =
      { dep_sets_update = (fun entry -> entry)
      ; transfer_to = LabelSet.empty
      ; rhs_set = ExprSet.empty
      }
    in LabelMap.singleton label info
  | IfElse (pred, thens, elses) ->
    let info =
      { dep_sets_update = (fun entry -> entry)
      ; transfer_to = LabelSet.empty
      ; rhs_set = expr_var_set pred
      };
    in merge_label_maps (LabelMap.singleton label info) (accumulate_label_info thens) (* TODO ELSE *)
  | While (pred, body) -> LabelMap.empty
  | For args -> LabelMap.empty
  | Block sts -> LabelMap.empty
  | SList l -> LabelMap.empty
  | Decl args -> LabelMap.empty
  | FunDef args -> LabelMap.empty
  | _ -> raise (Failure "unimplemented constructor in analysis")

let rec label_statements (start_ix : int ref) (st_loc : stmt_loc) : stmt_labeled =
  let transform_statement (apply_label : bool) (stmt : stmt_labeled statement) : stmt_labeled =
    { slabel = if apply_label
               then (let y = !start_ix in
                     let () = (start_ix := !start_ix + 1) in
                     Some y)
               else None
    ; sloc = st_loc.sloc
    ; stmt = stmt}
  in match st_loc.stmt with
  | Assignment _ as stmt ->
    transform_statement true stmt
  | NRFunApp _ as stmt ->
    transform_statement false stmt
  | Check _ as stmt ->
    transform_statement false stmt
  | MarkLocation _ as stmt ->
    transform_statement false stmt
  | Break as stmt ->
    transform_statement true stmt
  | Continue as stmt ->
    transform_statement true stmt
  | Return _ as stmt ->
    transform_statement true stmt
  | Skip as stmt ->
    transform_statement false stmt
  | IfElse (pred, thens, elses) ->
    let stmt = IfElse (pred, label_statements start_ix thens, Option.map elses (label_statements start_ix))
    in transform_statement true stmt
  | While (pred, body) ->
    let stmt = While (pred, label_statements start_ix body)
    in transform_statement true stmt
  | For args ->
    let stmt = For {args with body = label_statements start_ix args.body}
    in transform_statement true stmt
  | Block sts ->
    let stmt = Block (List.map sts (label_statements start_ix))
    in transform_statement false stmt
  | SList l ->
    let stmt = SList (List.map l (label_statements start_ix))
    in transform_statement false stmt
  | Decl args as stmt ->
    transform_statement true stmt
  | FunDef args ->
    let stmt = FunDef {args with fdbody = label_statements start_ix args.fdbody}
    in transform_statement false stmt

let label_prog (start_ix : int ref) (mir : stmt_loc prog) : (stmt_labeled prog) =
  { functionsb = label_statements start_ix mir.functionsb
  ; datab = (let (table, s) = mir.datab in (table, label_statements start_ix s))
  ; modelb = (let (table, s) = mir.modelb in (table, label_statements start_ix s))
  ; gqb = (let (table, s) = mir.gqb in (table, label_statements start_ix s))
  ; prog_name = mir.prog_name
  ; prog_path = mir.prog_path }

(*let analysis (mir : stmt_loc statement) : dataflow =*)
let analysis (mir : stmt_loc prog) : stmt_labeled prog =
  let labeled = label_prog (ref 0) mir
  in labeled
