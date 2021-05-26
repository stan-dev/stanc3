open Core_kernel
open Middle
open Middle.Program
open Middle.Expr
open Dataflow_types

let rec fold_expr ~take_expr ~(init : 'c) (expr : Expr.Typed.t) : 'c =
  Expr.Fixed.Pattern.fold_left
    ~f:(fun a e -> fold_expr ~take_expr ~init:(take_expr a e) e)
    ~init expr.pattern

let fold_stmts ~take_expr ~take_stmt ~(init : 'c)
    (stmts : Stmt.Located.t List.t) : 'c =
  (* let rec fold_expr (state : 'c) (expr : Expr.Typed.Meta.t Expr.Fixed.t) =
   *   Expr.Fixed.Pattern.fold_left
   *     ~f:(fun a e -> fold_expr (take_expr a e) e)
   *     ~init:state
   *     expr.pattern
   * in *)
  let rec fold_stmt (state : 'c) (stmt : Stmt.Located.t) =
    Stmt.Fixed.Pattern.fold_left
      ~f:(fun a e -> fold_expr ~take_expr ~init:(take_expr a e) e)
      ~g:(fun a s -> fold_stmt (take_stmt a s) s)
      ~init:state stmt.pattern
  in
  List.fold ~f:(fun a s -> fold_stmt (take_stmt a s) s) ~init stmts

let rec num_expr_value (v : Expr.Typed.t) : (float * string) option =
  match v with
  | {pattern= Fixed.Pattern.Lit (Real, str); _}
   |{pattern= Fixed.Pattern.Lit (Int, str); _} ->
      Some (float_of_string str, str)
  | {pattern= Fixed.Pattern.FunApp (StanLib ("PMinus__", FnPlain, _), [v]); _}
  -> (
    match num_expr_value v with
    | Some (v, s) -> Some (-.v, "-" ^ s)
    | None -> None )
  | _ -> None

type bound_values =
  { lower: [`None | `Nonlit | `Lit of float]
  ; upper: [`None | `Nonlit | `Lit of float] }

let trans_bounds_values (trans : Expr.Typed.t transformation) : bound_values =
  let bound_value e =
    match num_expr_value e with None -> `Nonlit | Some (f, _) -> `Lit f
  in
  match trans with
  | Lower lower -> {lower= bound_value lower; upper= `None}
  | Upper upper -> {lower= `None; upper= bound_value upper}
  | LowerUpper (lower, upper) ->
      {lower= bound_value lower; upper= bound_value upper}
  | Simplex -> {lower= `Lit 0.; upper= `Lit 1.}
  | PositiveOrdered -> {lower= `Lit 0.; upper= `None}
  | UnitVector -> {lower= `Lit (-1.); upper= `Lit 1.}
  | CholeskyCorr | CholeskyCov | Correlation | Covariance | Ordered
   |Offset _ | Multiplier _ | OffsetMultiplier _ | Identity ->
      {lower= `None; upper= `None}

let chop_dist_name (fname : string) : string Option.t =
  (* Slightly inefficient, would be better to short-circuit *)
  List.fold ~init:None ~f:Option.first_some
    (List.map
       ~f:(fun suffix -> String.chop_suffix ~suffix fname)
       Middle.Utils.distribution_suffices)

let rec top_var_declarations Stmt.Fixed.({pattern; _}) : string Set.Poly.t =
  match pattern with
  | Decl {decl_id; _} -> Set.Poly.singleton decl_id
  | SList l -> Set.Poly.union_list (List.map ~f:top_var_declarations l)
  | _ -> Set.Poly.empty

let data_set ?(exclude_transformed = false) ?(exclude_ints = false)
    (mir : Program.Typed.t) : string Set.Poly.t =
  (* Data are input_vars *)
  let data = Set.Poly.of_list mir.input_vars in
  (* Possibly remove ints from the data set *)
  let filtered_data =
    let remove_ints =
      Set.Poly.filter ~f:(fun (_, st) -> st <> SizedType.SInt)
    in
    Set.Poly.map ~f:fst ((if exclude_ints then remove_ints else ident) data)
  in
  (* Transformed data are declarations in prepare_data but excluding data *)
  if exclude_transformed then filtered_data
  else
    let trans_data =
      Set.Poly.diff
        (Set.Poly.union_list
           (List.map ~f:top_var_declarations mir.prepare_data))
        (Set.Poly.map ~f:fst data)
    in
    Set.Poly.union trans_data filtered_data

let parameter_set ?(include_transformed = false) (mir : Program.Typed.t) =
  Set.Poly.of_list
    (List.map
       ~f:(fun (pname, {out_trans; _}) -> (pname, out_trans))
       (List.filter
          ~f:(fun (_, {out_block; _}) ->
            out_block = Parameters
            || (include_transformed && out_block = TransformedParameters) )
          mir.output_vars))

let parameter_names_set ?(include_transformed = false) (mir : Program.Typed.t)
    =
  Set.Poly.map ~f:fst (parameter_set ~include_transformed mir)

let rec var_declarations Stmt.Fixed.({pattern; _}) : string Set.Poly.t =
  match pattern with
  | Decl {decl_id; _} -> Set.Poly.singleton decl_id
  | IfElse (_, s, None) | While (_, s) | For {body= s; _} -> var_declarations s
  | IfElse (_, s1, Some s2) ->
      Set.Poly.union (var_declarations s1) (var_declarations s2)
  | Block slist | SList slist ->
      Set.Poly.union_list (List.map ~f:var_declarations slist)
  | _ -> Set.Poly.empty

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
    | Profile (_, stmts) | Block stmts ->
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
  | FunApp (_, exprs) -> union_recur exprs
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
  | Stmt.Fixed.Pattern.For vars -> Set.Poly.of_list [vars.lower; vars.upper]
  | NRFunApp (_, exprs) -> Set.Poly.of_list exprs
  | IfElse (rhs, _, _)
   |While (rhs, _)
   |Assignment (_, rhs)
   |TargetPE rhs
   |Return (Some rhs) ->
      Set.Poly.singleton rhs
  | Return None
   |Break | Continue | Skip | Decl _ | Profile _ | Block _ | SList _ ->
      Set.Poly.empty

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
  | FunApp (StanLib ("Plus__", FnPlain, _), [e1; e2]) ->
      List.append (summation_terms e1) (summation_terms e2)
  | _ -> [rhs]

(** See interface file *)
let stmt_of_block b =
  Stmt.Fixed.{pattern= SList b; meta= Stmt.Located.Meta.empty}

let rec fn_subst_expr m e =
  match m e with
  | Some e' ->
      (* let print_expr (e:Expr.Typed.t) = *)
      (* [%sexp (e.pattern : Expr.Typed.Meta.t Expr.Fixed.t Expr.Fixed.Pattern.t)] |> Sexp.to_string *)
      (* in *)
      (* let _ = print_endline ("Replaced expr: " ^ print_expr e ^ " -> " ^ print_expr e') in *)
      e'
  | _ -> Expr.Fixed.{e with pattern= Pattern.map (fn_subst_expr m) e.pattern}

let fn_subst_idx m = Index.map (fn_subst_expr m)

let fn_subst_stmt_base_helper g h b =
  Stmt.Fixed.Pattern.(
    match b with
    | Assignment ((x, ut, l), e2) -> Assignment ((x, ut, List.map ~f:h l), g e2)
    | x -> map g (fun y -> y) x)

let fn_subst_stmt_base m =
  fn_subst_stmt_base_helper (fn_subst_expr m) (fn_subst_idx m)

let fn_subst_stmt m = map_rec_stmt_loc (fn_subst_stmt_base m)

let name_map m (e : Expr.Typed.t) =
  match e.pattern with
  | Var s -> (
    match Map.Poly.find m s with
    | Some s' -> Some {e with pattern= Var s'}
    | None -> None )
  | _ -> None

let name_subst_stmt m = fn_subst_stmt (name_map m)

let var_map m (e : Expr.Typed.t) =
  match e.pattern with Var s -> Map.find m s | _ -> None

let subst_expr m e = fn_subst_expr (var_map m) e
let subst_idx m = Index.map (subst_expr m)
let subst_stmt_base m = fn_subst_stmt_base_helper (subst_expr m) (subst_idx m)
let subst_stmt m = map_rec_stmt_loc (subst_stmt_base m)
let expr_map m (e : Expr.Typed.t) = Map.find m e
let expr_subst_expr m e = fn_subst_expr (expr_map m) e
let expr_subst_idx m = Index.map (expr_subst_expr m)

let expr_subst_stmt_base m =
  fn_subst_stmt_base_helper (expr_subst_expr m) (expr_subst_idx m)

let expr_subst_stmt m = map_rec_stmt_loc (expr_subst_stmt_base m)

let rec expr_depth Expr.Fixed.({pattern; _}) =
  match pattern with
  | Var _ | Lit (_, _) -> 0
  | FunApp (_, l) ->
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
  | FunApp (kind, l) ->
      let l = List.map ~f:(update_expr_ad_levels autodiffable_variables) l in
      {pattern= FunApp (kind, l); meta= {e.meta with adlevel= ad_level_sup l}}
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
  | Indexed (ixed, i_list) ->
      let ixed = update_expr_ad_levels autodiffable_variables ixed in
      let i_list =
        List.map ~f:(update_idx_ad_levels autodiffable_variables) i_list
      in
      { pattern= Indexed (ixed, i_list)
      ; meta=
          { e.meta with
            adlevel= ad_level_sup (e :: List.concat_map ~f:Index.bounds i_list)
          } }

and update_idx_ad_levels autodiffable_variables =
  Index.map (update_expr_ad_levels autodiffable_variables)

(**
 * Modify an index's inner expressions with `op`
 * @param op a functor returning an expression
 * @param ind the Index.t to modify
 *)
let mod_index op ind =
  match ind with
  | Index.All -> Index.All
  | Single ind_expr -> Single (op ind_expr)
  | Upfrom ind_expr -> Upfrom (op ind_expr)
  | Between (expr_top, expr_bottom) -> Between (op expr_top, op expr_bottom)
  | MultiIndex exprs -> MultiIndex (op exprs)

(**
 * Apply an op returning true/false to an index 
 * @param op a functor returning a boolean
 * @param ind the Index.t to modify
 *)
let search_index op ind =
  match ind with
  | Index.All -> false
  | Single ind_expr -> op ind_expr
  | Upfrom ind_expr -> op ind_expr
  | Between (expr_top, expr_bottom) -> op expr_top || op expr_bottom
  | MultiIndex exprs -> op exprs

(**
 * Search through an expression for `Var name` where `name = var_name`
 * @param var_name A string with the name of the obj
 *  we are searching for.
 * @param pattern An expression pattern we recursivly search through 
 *  for the name.
 **)
let rec query_aos_names (var_name : string) Expr.Fixed.({pattern; _}) =
  let query_name = query_aos_names var_name in
  match pattern with
  | FunApp (_, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      List.exists ~f:query_name exprs
  | TernaryIf (predicate, texpr, fexpr) ->
      query_name predicate || query_name texpr || query_name fexpr
  | Indexed (expr, indexed) ->
      let search_index = search_index query_name in
      query_name expr || List.exists ~f:search_index indexed
  | Var (name : string) -> name = var_name
  | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) -> false
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_name lhs || query_name rhs

(**
 * Search through an expression for `Var name` where `name = var_name`
 * @param var_name A Set of strings with the name of the obj
 *  we are searching for.
 * @param pattern An expression pattern we recursivly search through 
 *  for the name.
 **)
let rec query_aos_set_names (var_name : string Set.Poly.t)
    Expr.Fixed.({pattern; _}) =
  let query_name = query_aos_set_names var_name in
  match pattern with
  | FunApp (_, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      List.exists ~f:query_name exprs
  | TernaryIf (predicate, texpr, fexpr) ->
      query_name predicate || query_name texpr || query_name fexpr
  | Indexed (expr, indexed) ->
      let search_index = search_index query_name in
      query_name expr || List.exists ~f:search_index indexed
  | Var (name : string) -> Set.Poly.exists ~f:(fun x -> x = name) var_name
  | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) -> false
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_name lhs || query_name rhs

(** 
 * Modify functions expressions from SoA to AoS
 * TODO: Docs
 * The main issue with this right now is that if we see the failed var_name
 * inside of any StanLib we flip the whole StanLib to AoS, but we only need to 
 * do that if every expression's objs are all AoS. If just one argument 
 * is still an SoA then we can actually keep the functions as SoA.
 *
 * The only real path in the below is on the functions, everything else is 
 * for recursion through expressions of expressions.
 *)
let rec modify_soa_exprs (var_name : string Set.Poly.t)
    Expr.Fixed.({pattern; meta}) =
  let mod_expr = modify_soa_exprs var_name in
  let find_name = query_aos_set_names var_name in
  let new_pattern =
    match pattern with
    | FunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
        let exprs' = List.map ~f:mod_expr exprs in
        let modify_funs kind =
          match kind with
          | Fun_kind.StanLib (name, sfx, Common.Helpers.SoA) as func -> (
            match List.exists ~f:find_name exprs with
            | true -> Fun_kind.StanLib (name, sfx, Common.Helpers.AoS)
            | false -> func )
          | _ -> kind
        in
        Expr.Fixed.Pattern.FunApp (modify_funs kind, exprs')
    | TernaryIf (predicate, texpr, fexpr) ->
        TernaryIf (mod_expr predicate, mod_expr texpr, mod_expr fexpr)
    | Indexed (expr, indexed) ->
        let query_index = mod_index mod_expr in
        Indexed (mod_expr expr, List.map ~f:query_index indexed)
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string))
      ->
        pattern
    | EAnd (lhs, rhs) -> EAnd (mod_expr lhs, mod_expr rhs)
    | EOr (lhs, rhs) -> EOr (mod_expr lhs, mod_expr rhs)
  in
  Expr.Fixed.{pattern= new_pattern; meta}

(*I'm not sure I need this anymore*)
let rec modify_soa_stmts (var_name : string Set.Poly.t)
    Stmt.Fixed.({pattern; meta}) =
  let mod_expr = modify_soa_exprs var_name in
  let mod_stmt = modify_soa_stmts var_name in
  let find_name = query_aos_set_names var_name in
  let mod_pattern pattern =
    match pattern with
    | Stmt.Fixed.Pattern.NRFunApp
        (StanLib (name, kind, Common.Helpers.SoA), exprs) as func -> (
      match List.exists ~f:find_name exprs with
      | true ->
          Stmt.Fixed.Pattern.NRFunApp
            ( StanLib (name, kind, Common.Helpers.AoS)
            , List.map ~f:mod_expr exprs )
      | false -> func )
    (*TODO: User defined functions here*)
    | NRFunApp (fun_kind, expr) ->
        NRFunApp (fun_kind, List.map ~f:mod_expr expr)
    | Assignment (((name : string), (ut : UnsizedType.t), lhs), rhs) ->
        let query_index = mod_index mod_expr in
        Assignment ((name, ut, List.map ~f:query_index lhs), mod_expr rhs)
    | IfElse (predicate, true_stmt, op_false_stmt) ->
        let mod_pred = mod_expr predicate in
        let mod_true = mod_stmt true_stmt in
        let mod_false =
          match op_false_stmt with
          | Some stmt -> Some (mod_stmt stmt)
          | None -> None
        in
        IfElse (mod_pred, mod_true, mod_false)
    | Block stmts -> Block (List.map ~f:mod_stmt stmts)
    | SList stmts -> SList (List.map ~f:mod_stmt stmts)
    | For {loopvar; lower; upper; body= inner_stmt} ->
        Stmt.Fixed.Pattern.For
          { loopvar
          ; lower= mod_expr lower
          ; upper= mod_expr upper
          ; body= mod_stmt inner_stmt }
    | TargetPE expr -> TargetPE (mod_expr expr)
    | Return optional_expr -> (
      match optional_expr with
      | Some expr -> Return (Some (mod_expr expr))
      | None -> Return None )
    | Profile ((a : string), stmt) -> Profile (a, List.map ~f:mod_stmt stmt)
    | Skip | Decl _ | Break | Continue -> pattern
    | While (predicate, body) -> While (mod_expr predicate, mod_stmt body)
  in
  Stmt.Fixed.{pattern= mod_pattern pattern; meta}

(* Look through an expression and find the overall type and adlevel*)
let find_args Expr.Fixed.({meta= Expr.Typed.Meta.({type_; adlevel; _}); _}) =
  (adlevel, type_)

(* Look through an expression to see whether it needs modified from
 * SoA to AoS.
 * TODO: The logic on when to return true/false is wrong.
 * I really want to move from returning true/false to return SoA/AoS
 * which should be a lot easier to read.
 *  I think we want to use this in an "is any" style context
 *
 * Logic for deciding if the object can use SoA is:
 * If we find a FunApp
 *  - If the FunApp (StanLib _) is an SoA, check if the object's name exists 
 *     in the expressions and whether that signature supports 
 *     SoA.
 *  - If we find a FunApp (StanLib _) that is not SoA or is UserDefined,
 *    check that the name, chat that the object name is not used in
 *    the expressions for the 
 * For Assignments
 * If the assignee's name is in the list of known failures 
 *)
let rec query_aos_exprs var_name known_failures Expr.Fixed.({pattern; _}) =
  let query_expr = query_aos_exprs var_name known_failures in
  let query_name = query_aos_names var_name in
  match pattern with
  | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) -> (
    match kind with
    | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), Common.Helpers.SoA)
      -> (
        let does_name_exist = List.exists ~f:query_name exprs in
        (*If name doesn't exist then we are good, else check support*)
        match does_name_exist with
        | false -> false
        | true ->
            let check_fun_support =
              Stan_math_signatures.query_stan_math_mem_pattern_support name
                (List.map ~f:find_args exprs)
            in
            (not check_fun_support) || List.exists ~f:query_expr exprs )
    | CompilerInternal _ -> false
    | Fun_kind.StanLib (_, _, Common.Helpers.AoS) | UserDefined _ ->
        List.exists ~f:query_name exprs )
  | TernaryIf (predicate, texpr, fexpr) ->
      query_expr predicate || query_expr texpr || query_expr fexpr
  | Indexed (expr, indexed) ->
      let query_index = search_index query_expr in
      query_expr expr || List.exists ~f:query_index indexed
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      false
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_expr lhs || query_expr rhs

(* Look through a statement to see whether it needs modified from
 * SoA to AoS. Returns true if object needs changed from SoA to AoS.
 *)
let rec query_aos_stmts (var_name : string)
    (known_failures : string Set.Poly.t)
    (stmt_map : (int, Stmt.Located.Non_recursive.t) Core_kernel.Map.Poly.t)
    Stmt.Located.Non_recursive.({pattern; _}) : bool =
  let query_expr = query_aos_exprs var_name known_failures in
  let query_stmt = query_aos_stmts var_name known_failures stmt_map in
  let query_name = query_aos_names var_name in
  let get_key key = Map.find_exn stmt_map key in
  let find_key blah = List.map ~f:get_key blah in
  let find_pattern (pattern : (Expr.Typed.t, cf_state) Stmt.Fixed.Pattern.t) =
    match pattern with
    | Stmt.Fixed.Pattern.NRFunApp
        ( StanLib
            ((name : string), (_ : bool Fun_kind.suffix), Common.Helpers.SoA)
        , (exprs : Expr.Typed.Meta.t Expr.Fixed.t list) ) -> (
        let does_name_exist = List.for_all ~f:query_name exprs in
        match does_name_exist with
        | false -> false
        | true ->
            let check_fun_support =
              Stan_math_signatures.query_stan_math_mem_pattern_support name
                (List.map ~f:find_args exprs)
            in
            (not check_fun_support) || List.exists ~f:query_expr exprs )
    | NRFunApp (CompilerInternal _, _) -> false
    | NRFunApp (UserDefined _, exprs) -> List.for_all ~f:query_name exprs
    | NRFunApp (StanLib (_, _, AoS), exprs) -> List.for_all ~f:query_name exprs
    | Assignment (((assign_name : string), (_ : UnsizedType.t), lhs), rhs) ->
        let query_index = search_index query_expr in
        let check_name item = assign_name = item in
        Set.Poly.exists ~f:check_name known_failures
        || List.exists ~f:query_index lhs
        || query_expr rhs
    | IfElse (predicate, true_stmt, op_false_stmt) ->
        let pred_query = query_expr predicate in
        let true_query = query_stmt (get_key true_stmt) in
        let false_query =
          match op_false_stmt with
          | Some stmt -> query_stmt (get_key stmt)
          | None -> false
        in
        pred_query || true_query || false_query
    | Block stmts -> List.exists ~f:query_stmt (find_key stmts)
    | SList stmts -> List.exists ~f:query_stmt (find_key stmts)
    | For {lower; upper; body= sub_stmts; _} ->
        query_expr lower || query_expr upper || query_stmt (get_key sub_stmts)
    | TargetPE expr -> query_expr expr
    | Return optional_expr -> (
      match optional_expr with Some expr -> query_expr expr | None -> false )
    | Profile ((_ : string), stmt) -> List.exists ~f:query_stmt (find_key stmt)
    | Skip | Decl _ | Break | Continue -> false
    | While (predicate, body) ->
        query_expr predicate || query_stmt (get_key body)
  in
  find_pattern pattern

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
