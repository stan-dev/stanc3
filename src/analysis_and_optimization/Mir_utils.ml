open Core_kernel
open Middle
open Dataflow_types

let rec map_rec_expr f e =
  { e with
    Expr.Fixed.pattern= f @@ Expr.Fixed.Pattern.map (map_rec_expr f) e.pattern
  }

(* (e : expr_typed_located) =
  let recurse = map_rec_expr f in
  {e with expr= f (map_expr recurse e.expr)} *)

let map_rec_expr_state f state e =
  let cur_state = ref state in
  let g e' =
    let e', state = f !cur_state e' in
    cur_state := state ;
    e'
  in
  let e = map_rec_expr g e in
  let state = !cur_state in
  (e, state)

let rec map_rec_stmt_loc
    (f :
         (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement) ({smeta; stmt} : stmt_loc) =
  let recurse = map_rec_stmt_loc f in
  {smeta; stmt= f (map_statement (fun x -> x) recurse stmt)}

let rec top_down_map_rec_stmt_loc
    (f :
         (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement) ({smeta; stmt} : stmt_loc) =
  let recurse = top_down_map_rec_stmt_loc f in
  {smeta; stmt= map_statement (fun x -> x) recurse (f stmt)}

let map_rec_state_stmt_loc
    (f :
         's
      -> (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement * 's) (state : 's)
    ({smeta; stmt} : stmt_loc) : stmt_loc * 's =
  let cur_state = ref state in
  let g stmt =
    let stmt, state = f !cur_state stmt in
    cur_state := state ;
    stmt
  in
  let stmt = map_rec_stmt_loc g {smeta; stmt} in
  let state = !cur_state in
  (stmt, state)

let map_rec_stmt_loc_num (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t)
    (f :
         int
      -> (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement) (s : stmt_loc_num) =
  let rec map_rec_stmt_loc_num' (cur_node : int)
      ({smetan; stmtn} : stmt_loc_num) : stmt_loc =
    let find_node i = Map.find_exn flowgraph_to_mir i in
    let recurse i = map_rec_stmt_loc_num' i (find_node i) in
    {smeta= smetan; stmt= f cur_node (map_statement (fun x -> x) recurse stmtn)}
  in
  map_rec_stmt_loc_num' 1 s

let map_rec_state_stmt_loc_num
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t)
    (f :
         int
      -> 's
      -> (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement * 's) (state : 's)
    (s : stmt_loc_num) : stmt_loc * 's =
  let cur_state = ref state in
  let g i stmt =
    let stmt, state = f i !cur_state stmt in
    cur_state := state ;
    stmt
  in
  let stmt = map_rec_stmt_loc_num flowgraph_to_mir g s in
  let state = !cur_state in
  (stmt, state)

let stmt_loc_of_stmt_loc_num
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) (s : stmt_loc_num) =
  map_rec_stmt_loc_num flowgraph_to_mir (fun _ s' -> s') s

let statement_stmt_loc_of_statement_stmt_loc_num
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) s =
  (stmt_loc_of_stmt_loc_num flowgraph_to_mir {stmtn= s; smetan= no_span}).stmt

(** Forgetful function from numbered to unnumbered programs *)
let unnumbered_prog_of_numbered_prog
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) p =
  map_prog (stmt_loc_of_stmt_loc_num flowgraph_to_mir) p

(** See interface file *)
let fwd_traverse_statement (stmt : ('e, 'a) statement) ~init:(state : 'f)
    ~(f : 'f -> 'a -> 'f * 'c) : 'f * ('e, 'c) statement =
  match stmt with
  | IfElse (pred, then_s, else_s_opt) ->
      let s', c = f state then_s in
      Option.value_map else_s_opt
        ~default:(s', IfElse (pred, c, None))
        ~f:(fun else_s ->
          let s'', c' = f s' else_s in
          (s'', IfElse (pred, c, Some c')) )
  | While (pred, body) ->
      let s', c = f state body in
      (s', While (pred, c))
  | For vars ->
      let s', c = f state vars.body in
      (s', For {vars with body= c})
  | Block stmts ->
      let s', ls =
        List.fold_left stmts
          ~f:(fun (s, l) stmt ->
            let s', c = f s stmt in
            (s', List.cons c l) )
          ~init:(state, [])
      in
      (s', Block (List.rev ls))
  | SList stmts ->
      let s', ls =
        List.fold_left stmts
          ~f:(fun (s, l) stmt ->
            let s', c = f s stmt in
            (s', List.cons c l) )
          ~init:(state, [])
      in
      (s', SList (List.rev ls))
  | Assignment _ as s -> (state, s)
  | TargetPE _ as s -> (state, s)
  | NRFunApp _ as s -> (state, s)
  | Break as s -> (state, s)
  | Continue as s -> (state, s)
  | Return _ as s -> (state, s)
  | Skip as s -> (state, s)
  | Decl _ as s -> (state, s)

(** See interface file *)
let vexpr_of_expr_exn (ex : expr_typed_located) : vexpr =
  match ex.expr with
  | Var s -> VVar s
  | _ -> raise (Failure "Non-var expression found, but var expected")

(** See interface file *)
let rec expr_var_set (ex : expr_typed_located) :
    (vexpr * mtype_loc_ad) Set.Poly.t =
  let union_recur exprs =
    Set.Poly.union_list (List.map exprs ~f:expr_var_set)
  in
  match ex.expr with
  | Var s -> Set.Poly.singleton (VVar s, ex.emeta)
  | Lit _ -> Set.Poly.empty
  | FunApp (_, _, exprs) -> union_recur exprs
  | TernaryIf (expr1, expr2, expr3) -> union_recur [expr1; expr2; expr3]
  | Indexed (expr, ix) ->
      Set.Poly.union_list (expr_var_set expr :: List.map ix ~f:index_var_set)
  | EAnd (expr1, expr2) | EOr (expr1, expr2) -> union_recur [expr1; expr2]

and index_var_set (ix : expr_typed_located index) :
    (vexpr * mtype_loc_ad) Set.Poly.t =
  match ix with
  | All -> Set.Poly.empty
  | Single expr -> expr_var_set expr
  | Upfrom expr -> expr_var_set expr
  | Between (expr1, expr2) ->
      Set.Poly.union (expr_var_set expr1) (expr_var_set expr2)
  | MultiIndex expr -> expr_var_set expr

let stmt_rhs stmt =
  match stmt with
  | For vars -> ExprSet.of_list [vars.lower; vars.upper]
  | NRFunApp (_, _, exprs) -> ExprSet.of_list exprs
  | IfElse (rhs, _, _)
   |While (rhs, _)
   |Assignment (_, rhs)
   |TargetPE rhs
   |Return (Some rhs) ->
      ExprSet.singleton rhs
  | Return None | Break | Continue | Skip | Decl _ | Block _ | SList _ ->
      ExprSet.empty

let union_map (set : ('a, 'c) Set_intf.Set.t) ~(f : 'a -> 'b Set.Poly.t) =
  Set.fold set ~init:Set.Poly.empty ~f:(fun s a -> Set.Poly.union s (f a))

let stmt_rhs_var_set stmt = union_map (stmt_rhs stmt) ~f:expr_var_set

(** See interface file *)
let expr_assigned_var (ex : expr_typed_located) : vexpr =
  match ex.expr with
  | Var s -> VVar s
  | Indexed ({expr= Var s; _}, _) -> VVar s
  | _ -> raise (Failure "Unimplemented: analysis of assigning to non-var")

(** See interface file *)
let rec summation_terms (rhs : expr_typed_located) : expr_typed_located list =
  match rhs.expr with
  | FunApp (_, "Plus__", [e1; e2]) ->
      List.append (summation_terms e1) (summation_terms e2)
  | _ -> [rhs]

(** See interface file *)
let stmt_of_block b = {stmt= SList b; smeta= Middle.no_span}

let rec subst_expr (m : (string, expr_typed_located) Map.Poly.t)
    (e : expr_typed_located) =
  match e.expr with
  | Var s -> ( match Map.find m s with Some e' -> e' | None -> e )
  | x -> {e with expr= map_expr (subst_expr m) x}

let subst_idx m = map_index (subst_expr m)

let subst_stmt_base_helper g h b =
  match b with
  | Assignment ((x, ut, l), e2) -> Assignment ((x, ut, List.map ~f:h l), g e2)
  | x -> map_statement g (fun y -> y) x

let subst_stmt_base m = subst_stmt_base_helper (subst_expr m) (subst_idx m)
let subst_stmt m = map_rec_stmt_loc (subst_stmt_base m)

let rec expr_subst_expr m (e : expr_typed_located) =
  match Map.find m e with
  | Some e' -> e'
  | None -> {e with expr= map_expr (expr_subst_expr m) e.expr}

let expr_subst_idx m = map_index (expr_subst_expr m)

let expr_subst_stmt_base m =
  subst_stmt_base_helper (expr_subst_expr m) (expr_subst_idx m)

let expr_subst_stmt m = map_rec_stmt_loc (expr_subst_stmt_base m)

let rec expr_depth (e : expr_typed_located) : int =
  match e.expr with
  | Var _ | Lit (_, _) -> 0
  | FunApp (_, _, l) ->
      1
      + Option.value ~default:0
          (List.max_elt ~compare:compare_int (List.map ~f:expr_depth l))
  | TernaryIf (e1, e2, e3) ->
      1
      + Option.value ~default:0
          (List.max_elt ~compare:compare_int
             (List.map ~f:expr_depth [e1; e2; e3]))
  | Indexed (e, l) ->
      1
      + max (expr_depth e)
          (Option.value ~default:0
             (List.max_elt ~compare:compare_int (List.map ~f:idx_depth l)))
  | EAnd (e1, e2) | EOr (e1, e2) ->
      1
      + Option.value ~default:0
          (List.max_elt ~compare:compare_int (List.map ~f:expr_depth [e1; e2]))

and idx_depth (i : expr_typed_located index) : int =
  match i with
  | All -> 0
  | Single e | Upfrom e | MultiIndex e -> expr_depth e
  | Between (e1, e2) -> max (expr_depth e1) (expr_depth e2)

let ad_level_sup l =
  if List.exists l ~f:(fun x -> x.emeta.madlevel = AutoDiffable) then
    AutoDiffable
  else DataOnly

let rec update_expr_ad_levels autodiffable_variables e =
  match e.expr with
  | Var x ->
      if Set.Poly.mem autodiffable_variables x then
        {e with emeta= {e.emeta with madlevel= AutoDiffable}}
      else {e with emeta= {e.emeta with madlevel= DataOnly}}
  | Lit (_, _) -> {e with emeta= {e.emeta with madlevel= DataOnly}}
  | FunApp (o, f, l) ->
      let l = List.map ~f:(update_expr_ad_levels autodiffable_variables) l in
      {expr= FunApp (o, f, l); emeta= {e.emeta with madlevel= ad_level_sup l}}
  | TernaryIf (e1, e2, e3) ->
      let e1 = update_expr_ad_levels autodiffable_variables e1 in
      let e2 = update_expr_ad_levels autodiffable_variables e2 in
      let e3 = update_expr_ad_levels autodiffable_variables e3 in
      { expr= TernaryIf (e1, e2, e3)
      ; emeta= {e.emeta with madlevel= ad_level_sup [e1; e2; e3]} }
  | EAnd (e1, e2) ->
      let e1 = update_expr_ad_levels autodiffable_variables e1 in
      let e2 = update_expr_ad_levels autodiffable_variables e2 in
      { expr= EAnd (e1, e2)
      ; emeta= {e.emeta with madlevel= ad_level_sup [e1; e2]} }
  | EOr (e1, e2) ->
      let e1 = update_expr_ad_levels autodiffable_variables e1 in
      let e2 = update_expr_ad_levels autodiffable_variables e2 in
      { expr= EOr (e1, e2)
      ; emeta= {e.emeta with madlevel= ad_level_sup [e1; e2]} }
  | Indexed (e, i_list) ->
      let e = update_expr_ad_levels autodiffable_variables e in
      let i_list =
        List.map ~f:(update_idx_ad_levels autodiffable_variables) i_list
      in
      { expr= Indexed (e, i_list)
      ; emeta=
          { e.emeta with
            madlevel=
              ad_level_sup (e :: List.concat (List.map ~f:expr_from_idx i_list))
          } }

and update_idx_ad_levels autodiffable_variables =
  map_index (update_expr_ad_levels autodiffable_variables)
