open Core_kernel
open Mir
open Dataflow_types

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
  | FunDef vars ->
      let s', c = f state vars.fdbody in
      (s', FunDef {vars with fdbody= c})
  | Assignment _ as s -> (state, s)
  | TargetPE _ as s -> (state, s)
  | NRFunApp _ as s -> (state, s)
  | Check _ as s -> (state, s)
  | Break as s -> (state, s)
  | Continue as s -> (state, s)
  | Return _ as s -> (state, s)
  | Skip as s -> (state, s)
  | Decl _ as s -> (state, s)

(** See interface file *)
let vexpr_of_expr_exn (ex : expr_typed_located) : vexpr =
  match ex.texpr with
  | Var s -> VVar s
  | _ -> raise (Failure "Non-var expression found, but var expected")

(** See interface file *)
let rec expr_var_set (ex : expr_typed_located) : vexpr Set.Poly.t =
  let union_recur exprs =
    Set.Poly.union_list (List.map exprs ~f:expr_var_set)
  in
  match ex.texpr with
  | Var s -> Set.Poly.singleton (VVar s)
  | Lit _ -> Set.Poly.empty
  | FunApp (_, exprs) -> union_recur exprs
  | TernaryIf (expr1, expr2, expr3) -> union_recur [expr1; expr2; expr3]
  | Indexed (expr, ix) ->
      Set.Poly.union_list (expr_var_set expr :: List.map ix ~f:index_var_set)

and index_var_set (ix : expr_typed_located index) : vexpr Set.Poly.t =
  match ix with
  | All -> Set.Poly.empty
  | Single expr -> expr_var_set expr
  | Upfrom expr -> expr_var_set expr
  | Downfrom expr -> expr_var_set expr
  | Between (expr1, expr2) ->
      Set.Poly.union (expr_var_set expr1) (expr_var_set expr2)
  | MultiIndex expr -> expr_var_set expr

(** See interface file *)
let expr_assigned_var (ex : expr_typed_located) : vexpr =
  match ex.texpr with
  | Var s -> VVar s
  | Indexed ({texpr= Var s; _}, _) -> VVar s
  | _ -> raise (Failure "Unimplemented: analysis of assigning to non-var")

(** See interface file *)
let rec summation_terms (rhs : expr_typed_located) : expr_typed_located list =
  match rhs.texpr with
  | FunApp ("Plus__", [e1; e2]) ->
      List.append (summation_terms e1) (summation_terms e2)
  | _ -> [rhs]

(** See interface file *)
let stmt_of_block b = {stmt= SList b; sloc= Mir.no_span}

let rec subst_expr (m : (string, expr_typed_located) Map.Poly.t)
    (e : expr_typed_located) =
  match e.texpr with
  | Var s -> ( match Map.find m s with Some e' -> e' | None -> e )
  | x -> {e with texpr= map_expr (subst_expr m) x}

and subst_idx m = map_index (subst_expr m)

let subst_stmt_base_helper g h b =
  let f = g in
  match b with
  | Assignment ({texpr= Var x; texpr_type; texpr_loc; texpr_adlevel}, e2) ->
      Assignment ({texpr= Var x; texpr_type; texpr_loc; texpr_adlevel}, f e2)
  | Assignment
      ( { texpr=
            Indexed
              ( {texpr= Var x; texpr_type= t2; texpr_loc= l2; texpr_adlevel= a2}
              , l )
        ; texpr_type
        ; texpr_loc
        ; texpr_adlevel }
      , e2 ) ->
      Assignment
        ( { texpr=
              Indexed
                ( { texpr= Var x
                  ; texpr_type= t2
                  ; texpr_loc= l2
                  ; texpr_adlevel= a2 }
                , List.map ~f:h l )
          ; texpr_type
          ; texpr_loc
          ; texpr_adlevel }
        , f e2 )
  | x -> map_statement f (fun y -> y) x

let subst_stmt_base m = subst_stmt_base_helper (subst_expr m) (subst_idx m)
let subst_stmt m = Mir.map_rec_stmt_loc (subst_stmt_base m)

let rec expr_subst_expr m (e : expr_typed_located) =
  match Map.find m e with
  | Some e' -> e'
  | None -> {e with texpr= map_expr (expr_subst_expr m) e.texpr}

and expr_subst_idx m = map_index (expr_subst_expr m)

let expr_subst_stmt_base m =
  subst_stmt_base_helper (expr_subst_expr m) (expr_subst_idx m)

let expr_subst_stmt m = Mir.map_rec_stmt_loc (expr_subst_stmt_base m)
