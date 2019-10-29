open Core_kernel
open Middle
open Dataflow_types

let rec map_rec_expr f e =
  let recurse = map_rec_expr f in
  Expr.Fixed.{e with pattern= f (Pattern.map recurse e.pattern)}

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

let rec map_rec_stmt_loc f stmt =
  let recurse = map_rec_stmt_loc f in
  Stmt.Fixed.
    {stmt with pattern= f (Pattern.map (fun x -> x) recurse stmt.pattern)}

let rec top_down_map_rec_stmt_loc f stmt =
  let recurse = top_down_map_rec_stmt_loc f in
  Stmt.Fixed.{stmt with pattern= Pattern.map Fn.id recurse (f stmt.pattern)}

let map_rec_state_stmt_loc f state stmt =
  let cur_state = ref state in
  let g stmt =
    let stmt, state = f !cur_state stmt in
    cur_state := state ;
    stmt
  in
  let stmt = map_rec_stmt_loc g stmt in
  let state = !cur_state in
  (stmt, state)

let map_rec_stmt_loc_num flowgraph_to_mir f s =
  let rec map_rec_stmt_loc_num' (cur_node : int)
      (stmt : Stmt.Located.Non_recursive.t) =
    let find_node i = Map.find_exn flowgraph_to_mir i in
    let recurse i = map_rec_stmt_loc_num' i (find_node i) in
    Stmt.Fixed.
      { pattern= f cur_node (Pattern.map Fn.id recurse stmt.pattern)
      ; meta= stmt.meta }
  in
  map_rec_stmt_loc_num' 1 s

let map_rec_state_stmt_loc_num
    (flowgraph_to_mir : (int, Stmt.Located.Non_recursive.t) Map.Poly.t)
    (f :
         int
      -> 's
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t
      -> (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t * 's) (state : 's)
    (s : Stmt.Located.Non_recursive.t) : Stmt.Located.t * 's =
  let cur_state = ref state in
  let g i stmt =
    let stmt, state = f i !cur_state stmt in
    cur_state := state ;
    stmt
  in
  let stmt = map_rec_stmt_loc_num flowgraph_to_mir g s in
  let state = !cur_state in
  (stmt, state)

let stmt_loc_of_stmt_loc_num flowgraph_to_mir s =
  (* (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) (s : stmt_loc_num) = *)
  map_rec_stmt_loc_num flowgraph_to_mir (fun _ s' -> s') s

let statement_stmt_loc_of_statement_stmt_loc_num flowgraph_to_mir pattern =
  (stmt_loc_of_stmt_loc_num flowgraph_to_mir
     Stmt.Located.{Non_recursive.pattern; meta= Meta.empty})
    .pattern

(** Forgetful function from numbered to unnumbered programs *)
let unnumbered_prog_of_numbered_prog flowgraph_to_mir p =
  Program.map (stmt_loc_of_stmt_loc_num flowgraph_to_mir) p

(** See interface file *)
let fwd_traverse_statement stmt ~init ~f =
  Stmt.Fixed.Pattern.(
    match stmt with
    | IfElse (pred, then_s, else_s_opt) ->
        let s', c = f init then_s in
        Option.value_map else_s_opt
          ~default:(s', IfElse (pred, c, None))
          ~f:(fun else_s ->
            let s'', c' = f s' else_s in
            (s'', IfElse (pred, c, Some c')) )
    | While (pred, body) ->
        let s', c = f init body in
        (s', While (pred, c))
    | For vars ->
        let s', c = f init vars.body in
        (s', For {vars with body= c})
    | Block stmts ->
        let s', ls =
          List.fold_left stmts
            ~f:(fun (s, l) stmt ->
              let s', c = f s stmt in
              (s', List.cons c l) )
            ~init:(init, [])
        in
        (s', Block (List.rev ls))
    | SList stmts ->
        let s', ls =
          List.fold_left stmts
            ~f:(fun (s, l) stmt ->
              let s', c = f s stmt in
              (s', List.cons c l) )
            ~init:(init, [])
        in
        (s', SList (List.rev ls))
    | Assignment _ as s -> (init, s)
    | TargetPE _ as s -> (init, s)
    | NRFunApp _ as s -> (init, s)
    | Break as s -> (init, s)
    | Continue as s -> (init, s)
    | Return _ as s -> (init, s)
    | Skip as s -> (init, s)
    | Decl _ as s -> (init, s))

(** See interface file *)
let vexpr_of_expr_exn Expr.Fixed.({pattern; _}) =
  match pattern with
  | Var s -> VVar s
  | _ -> raise (Failure "Non-var expression found, but var expected")

(** See interface file *)
let rec expr_var_set Expr.Fixed.({pattern; meta}) =
  let union_recur exprs =
    Set.Poly.union_list (List.map exprs ~f:expr_var_set)
  in
  match pattern with
  | Var s -> Set.Poly.singleton (VVar s, meta)
  | Lit _ -> Set.Poly.empty
  | FunApp (_, _, exprs) -> union_recur exprs
  | TernaryIf (expr1, expr2, expr3) -> union_recur [expr1; expr2; expr3]
  | Indexed (expr, ix) ->
      Set.Poly.union_list (expr_var_set expr :: List.map ix ~f:index_var_set)
  | EAnd (expr1, expr2) | EOr (expr1, expr2) -> union_recur [expr1; expr2]

and index_var_set ix =
  match ix with
  | All -> Set.Poly.empty
  | Single expr -> expr_var_set expr
  | Upfrom expr -> expr_var_set expr
  | Between (expr1, expr2) ->
      Set.Poly.union (expr_var_set expr1) (expr_var_set expr2)
  | MultiIndex expr -> expr_var_set expr

let stmt_rhs stmt =
  match stmt with
  | Stmt.Fixed.Pattern.For vars ->
      Expr.Typed.Set.of_list [vars.lower; vars.upper]
  | NRFunApp (_, _, exprs) -> Expr.Typed.Set.of_list exprs
  | IfElse (rhs, _, _)
   |While (rhs, _)
   |Assignment (_, rhs)
   |TargetPE rhs
   |Return (Some rhs) ->
      Expr.Typed.Set.singleton rhs
  | Return None | Break | Continue | Skip | Decl _ | Block _ | SList _ ->
      Expr.Typed.Set.empty

let union_map (set : ('a, 'c) Set_intf.Set.t) ~(f : 'a -> 'b Set.Poly.t) =
  Set.fold set ~init:Set.Poly.empty ~f:(fun s a -> Set.Poly.union s (f a))

let stmt_rhs_var_set stmt = union_map (stmt_rhs stmt) ~f:expr_var_set

(** See interface file *)
let expr_assigned_var Expr.Fixed.({pattern; _}) =
  match pattern with
  | Var s -> VVar s
  | Indexed ({pattern= Var s; _}, _) -> VVar s
  | _ -> raise (Failure "Unimplemented: analysis of assigning to non-var")

(** See interface file *)
let rec summation_terms (Expr.Fixed.({pattern; _}) as rhs) =
  match pattern with
  | FunApp (_, "Plus__", [e1; e2]) ->
      List.append (summation_terms e1) (summation_terms e2)
  | _ -> [rhs]

(** See interface file *)
let stmt_of_block b =
  Stmt.Fixed.{pattern= SList b; meta= Stmt.Located.Meta.empty}

let rec subst_expr m (Expr.Fixed.({pattern; _}) as e) =
  match pattern with
  | Var s -> ( match Map.find m s with Some e' -> e' | None -> e )
  | _ -> Expr.Fixed.{e with pattern= Pattern.map (subst_expr m) pattern}

let subst_idx m = Index.map (subst_expr m)

let subst_stmt_base_helper g h b =
  Stmt.Fixed.Pattern.(
    match b with
    | Assignment ((x, ut, l), e2) -> Assignment ((x, ut, List.map ~f:h l), g e2)
    | x -> map g (fun y -> y) x)

let subst_stmt_base m = subst_stmt_base_helper (subst_expr m) (subst_idx m)
let subst_stmt m = map_rec_stmt_loc (subst_stmt_base m)

let rec expr_subst_expr m e =
  match Map.find m e with
  | Some e' -> e'
  | None ->
      Expr.Fixed.{e with pattern= Pattern.map (expr_subst_expr m) e.pattern}

let expr_subst_idx m = Index.map (expr_subst_expr m)

let expr_subst_stmt_base m =
  subst_stmt_base_helper (expr_subst_expr m) (expr_subst_idx m)

let expr_subst_stmt m = map_rec_stmt_loc (expr_subst_stmt_base m)

let rec expr_depth Expr.Fixed.({pattern; _}) =
  match pattern with
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

and idx_depth i =
  match i with
  | All -> 0
  | Single e | Upfrom e | MultiIndex e -> expr_depth e
  | Between (e1, e2) -> max (expr_depth e1) (expr_depth e2)

let ad_level_sup l =
  if List.exists l ~f:(fun x -> Expr.Typed.adlevel_of x = AutoDiffable) then
    UnsizedType.AutoDiffable
  else DataOnly

let rec update_expr_ad_levels autodiffable_variables
    (Expr.Fixed.({pattern; _}) as e) =
  match pattern with
  | Var x ->
      if Set.Poly.mem autodiffable_variables x then
        Expr.Typed.{e with meta= Meta.{e.meta with adlevel= AutoDiffable}}
      else {e with meta= {e.meta with adlevel= DataOnly}}
  | Lit (_, _) -> {e with meta= {e.meta with adlevel= DataOnly}}
  | FunApp (o, f, l) ->
      let l = List.map ~f:(update_expr_ad_levels autodiffable_variables) l in
      {pattern= FunApp (o, f, l); meta= {e.meta with adlevel= ad_level_sup l}}
  | TernaryIf (e1, e2, e3) ->
      let e1 = update_expr_ad_levels autodiffable_variables e1 in
      let e2 = update_expr_ad_levels autodiffable_variables e2 in
      let e3 = update_expr_ad_levels autodiffable_variables e3 in
      { pattern= TernaryIf (e1, e2, e3)
      ; meta= {e.meta with adlevel= ad_level_sup [e1; e2; e3]} }
  | EAnd (e1, e2) ->
      let e1 = update_expr_ad_levels autodiffable_variables e1 in
      let e2 = update_expr_ad_levels autodiffable_variables e2 in
      { pattern= EAnd (e1, e2)
      ; meta= {e.meta with adlevel= ad_level_sup [e1; e2]} }
  | EOr (e1, e2) ->
      let e1 = update_expr_ad_levels autodiffable_variables e1 in
      let e2 = update_expr_ad_levels autodiffable_variables e2 in
      { pattern= EOr (e1, e2)
      ; meta= {e.meta with adlevel= ad_level_sup [e1; e2]} }
  | Indexed (e, i_list) ->
      let e = update_expr_ad_levels autodiffable_variables e in
      let i_list =
        List.map ~f:(update_idx_ad_levels autodiffable_variables) i_list
      in
      { pattern= Indexed (e, i_list)
      ; meta=
          { e.meta with
            adlevel= ad_level_sup (e :: List.concat_map ~f:Index.bounds i_list)
          } }

and update_idx_ad_levels autodiffable_variables =
  Index.map (update_expr_ad_levels autodiffable_variables)

(** [cleanup_stmts statements] will do a few simple transformations like
    removing Skips, collapsing empty blocks and SLists, etc. *)
let cleanup_empty_stmts stmts =
  let open Stmt.Fixed in
  let open Stmt.Fixed.Pattern in
  let cleanup_stmt s =
    let ellide = {s with pattern= Skip} in
    match s.pattern with
    | Block [] | SList [] -> ellide
    | For {body= {pattern= Skip; _}; _} -> ellide
    | While (_, {pattern= Skip; _}) -> ellide
    | Block [{pattern= Skip; _}] | SList [{pattern= Skip; _}] -> ellide
    | _ -> s
  in
  let is_decl = function {pattern= Decl _; _} -> true | _ -> false in
  let flatten_block s =
    match s.pattern with
    | SList ls | Block ls ->
        if List.for_all ~f:(Fn.non is_decl) ls then ls else [s]
    | _ -> [s]
  in
  let ellide_skip s = match s.pattern with Skip -> [] | _ -> [s] in
  List.map stmts ~f:(rewrite_bottom_up ~f:Fn.id ~g:cleanup_stmt)
  |> List.concat_map ~f:flatten_block
  |> List.concat_map ~f:ellide_skip

let%expect_test "cleanup" =
  let open Expr.Helpers in
  let open Stmt.Fixed in
  let open Stmt.Fixed.Pattern in
  let swrap pattern = {pattern; meta= Location_span.empty} in
  let body = Block [Skip |> swrap] |> swrap in
  let s = For {loopvar= "i"; lower= loop_bottom; upper= loop_bottom; body} in
  let res = [s |> swrap] |> cleanup_empty_stmts in
  [%sexp (res : Stmt.Located.t list)] |> print_s ;
  [%expect {|
    () |}]
