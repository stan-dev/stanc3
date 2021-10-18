open Core_kernel
open Middle
open Middle.Expr

(**
 * Return a set of name and meta pairs for all variables in an expression.
 *)
let rec expr_set Expr.Fixed.({pattern; meta}) =
  let union_recur exprs = Set.Poly.union_list (List.map exprs ~f:expr_set) in
  match pattern with
  | Var s -> Set.Poly.singleton (Dataflow_types.VVar s, meta)
  | Lit _ -> Set.Poly.empty
  | FunApp (_, exprs) -> union_recur exprs
  | TernaryIf (expr1, expr2, expr3) -> union_recur [expr1; expr2; expr3]
  | Indexed (expr, ix) ->
      let apply_idx =
        Index.apply ~default:Set.Poly.empty ~merge:Set.Poly.union expr_set
      in
      Set.Poly.union_list (expr_set expr :: List.map ix ~f:apply_idx)
  | EAnd (expr1, expr2) | EOr (expr1, expr2) -> union_recur [expr1; expr2]

let rec var_set Expr.Fixed.({pattern; meta}) =
  let union_recur exprs = Set.Poly.union_list (List.map exprs ~f:var_set) in
  match pattern with
  | Var s -> Set.Poly.singleton (Dataflow_types.VVar s, meta)
  | TernaryIf (_, expr2, expr3) -> union_recur [expr2; expr3]
  | _ -> Set.Poly.empty

(**
 * Return a set of all types containing Eigen matrices in an expression.
 *)
let query_eigen_names (expr : Typed.Meta.t Expr.Fixed.t) : string Set.Poly.t =
  let get_expr_eigen_names
      (Dataflow_types.VVar s, Expr.Typed.Meta.({adlevel; type_; _})) =
    if
      UnsizedType.contains_eigen_type type_
      && adlevel = UnsizedType.AutoDiffable
    then Some s
    else None
  in
  Set.Poly.filter_map ~f:get_expr_eigen_names (expr_set expr)

let query_var_eigen_names (expr : Typed.Meta.t Expr.Fixed.t) :
    string Set.Poly.t =
  let get_expr_eigen_names
      (Dataflow_types.VVar s, Expr.Typed.Meta.({adlevel; type_; _})) =
    if
      UnsizedType.contains_eigen_type type_
      && adlevel = UnsizedType.AutoDiffable
    then Some s
    else None
  in
  Set.Poly.filter_map ~f:get_expr_eigen_names (var_set expr)

(**
 * Check an expression to count how many times we see a single index.
 * @param acc An accumulator from previous folds of multiple expressions.
 * @param pattern The expression patterns to match against
 *)
let rec check_for_single_idx_expr (acc : int) Expr.Fixed.({pattern; _}) : int =
  match pattern with
  | Expr.Fixed.Pattern.FunApp (_, (exprs : Typed.Meta.t Expr.Fixed.t list)) ->
      List.fold_left ~init:acc ~f:check_for_single_idx_expr exprs
  | TernaryIf (predicate, texpr, fexpr) ->
      acc
      + check_for_single_idx_expr 0 predicate
      + check_for_single_idx_expr 0 texpr
      + check_for_single_idx_expr 0 fexpr
  | Indexed (idx_expr, indexed) ->
      acc
      + check_for_single_idx_expr 0 idx_expr
      + List.fold_left ~init:0 ~f:count_single_idx indexed
  | EAnd (lhs, rhs) ->
      acc + check_for_single_idx_expr 0 lhs + check_for_single_idx_expr 0 rhs
  | EOr (lhs, rhs) ->
      acc + check_for_single_idx_expr 0 lhs + check_for_single_idx_expr 0 rhs
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      acc

(**
 * Check an Index to count how many times we see a single index.
 * @param acc An accumulator from previous folds of multiple expressions.
 * @param idx An Index to match. For Single types this adds 1 to the 
 *  acc. For Upfrom and MultiIndex types we check the inner expression 
 *  for a Single index. All and Between cannot be Single cell access 
 *  and so pass acc along.
 *)
and count_single_idx (acc : int) (idx : Expr.Typed.Meta.t Expr.Fixed.t Index.t)
    =
  match idx with
  | Index.All -> acc
  | Between
      ( (_ : Expr.Typed.Meta.t Expr.Fixed.t)
      , (_ : Expr.Typed.Meta.t Expr.Fixed.t) ) ->
      acc
  | Single _ -> acc + 1
  | Upfrom up -> check_for_single_idx_expr acc up
  | MultiIndex multi -> check_for_single_idx_expr acc multi

(**
 * Find indices on Matrix and Vector types that perform single 
 *  cell access. Returns true if it finds
 * a vector, row vector, matrix, or matrix with single cell access
 * as well as an array of any of the above that is accessing the 
 * inner matrix types cell.
 * @param ut An UnsizedType to match against.
 * @param index This list is checked for Single cell access 
 *  either at the top level or within the `Index` types of the list.
 *)
let rec is_uni_eigen_loop_indexing in_loop (ut : UnsizedType.t)
    (index : Typed.Meta.t Expr.Fixed.t Index.t list) =
  match in_loop with
  | false -> false
  | true -> (
      let contains_single_idx =
        List.fold_left ~init:0 ~f:count_single_idx index
      in
      match (ut, index) with
      | (UnsizedType.UVector | URowVector), _ when contains_single_idx > 0 ->
          true
      | UMatrix, _ when contains_single_idx > 1 -> true
      | (UArray t | UFun (_, ReturnType t, _, _)), index -> (
        match List.tl index with
        | Some cut_list -> is_uni_eigen_loop_indexing in_loop t cut_list
        | None -> is_uni_eigen_loop_indexing in_loop t [Index.All] )
      (* None of the below contain single cell access*)
      | ( ( UReal | UComplex | UInt | UMathLibraryFunction
          | UFun (_, Void, _, _) )
        , _ ) ->
          false
      | (UVector | URowVector | UMatrix), _ -> false )

(*Validate whether a function can support SoA matrices*)
let is_fun_soa_supported name exprs =
  let fun_args = List.map ~f:Expr.Typed.fun_arg exprs in
  Stan_math_signatures.query_stan_math_mem_pattern_support name fun_args

(**
 * Query to find the initial set of objects that cannot be SoA.
 *  This is mostly recursing over expressions, with the exceptions
 *  being functions and indexing expressions.
 * @param in_loop a boolean to signify if the expression exists inside
 *  of a loop. If so, the names of matrix and vector like objects
 *   will be returned if the matrix or vector is accessed by single 
 *    cell indexing.
 *)
let rec query_initial_demotable_expr (in_loop : bool) Expr.Fixed.({pattern; _})
    =
  let query_expr = query_initial_demotable_expr in_loop in
  match pattern with
  | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      query_initial_demotable_funs in_loop kind exprs
  | Indexed ((Expr.Fixed.({meta= {type_; _}; _}) as expr), indexed) ->
      let index_set =
        Set.Poly.union_list
          (List.map
             ~f:
               (Index.apply ~default:Set.Poly.empty ~merge:Set.Poly.union
                  query_expr)
             indexed)
      in
      if is_uni_eigen_loop_indexing in_loop type_ indexed then
        Set.Poly.union (query_var_eigen_names expr) index_set
      else Set.Poly.union (query_expr expr) index_set
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      Set.Poly.empty
  | TernaryIf (predicate, texpr, fexpr) ->
      let true_fails = query_expr texpr in
      let false_fails = query_expr fexpr in
      let is_eigen_return =
        UnsizedType.contains_eigen_type fexpr.meta.type_
        || UnsizedType.contains_eigen_type texpr.meta.type_
      in
      let does_either_side_fail =
        is_eigen_return
        && (Set.Poly.length true_fails > 0 || Set.Poly.length false_fails > 0)
      in
      (*If either side fails we need to force both sides to match*)
      if does_either_side_fail then
        Set.Poly.union
          (Set.Poly.union (query_expr predicate) (query_eigen_names texpr))
          (query_eigen_names fexpr)
      else
        Set.Poly.union
          (Set.Poly.union (query_expr predicate) true_fails)
          false_fails
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      Set.Poly.union (query_expr lhs) (query_expr rhs)

(**
 * Query a function to detect if it or any of its used 
 *  expression's objects or expressions should be demoted to AoS.
 *
 * The logic here demotes the expressions in a function to AoS if:
 * (1) The function is user defined.
 * (2) The Stan math function cannot support AoS
 * @param in_loop A boolean to specify the logic of indexing expressions. See
 *  `query_initial_demotable_expr` for an explanation of the logic.
 * @param kind The function type, for StanLib functions we check if the 
 *  function supports SoA and for UserDefined functions we always fail 
 *  and return back all of the names of the objects passed in expressions
 *  to the UDF.
 * exprs The expression list passed to the functions.
 *)
and query_initial_demotable_funs (in_loop : bool) (kind : 'a Fun_kind.t)
    (exprs : Typed.Meta.t Expr.Fixed.t list) : string Set.Poly.t =
  let query_expr = query_initial_demotable_expr in_loop in
  let top_level_eigen_names =
    Set.Poly.union_list (List.map ~f:query_var_eigen_names exprs)
  in
  let demoted_eigen_names =
    Set.Poly.union_list (List.map ~f:query_expr exprs)
  in
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
    match name with
    | "check_matching_dims" -> Set.Poly.empty
    | name -> (
      match is_fun_soa_supported name exprs with
      | true -> demoted_eigen_names
      | false -> Set.Poly.union demoted_eigen_names top_level_eigen_names ) )
  | CompilerInternal (Internal_fun.FnMakeArray | FnMakeRowVec) ->
      Set.Poly.union demoted_eigen_names top_level_eigen_names
  | CompilerInternal (_ : 'a Internal_fun.t) -> Set.Poly.empty
  | UserDefined ((_ : string), (_ : bool Fun_kind.suffix)) ->
      Set.Poly.union demoted_eigen_names top_level_eigen_names

(**
 * Check whether right hand side
 *)
let rec query_soa_supported_assign_expr
    Expr.Fixed.({pattern; meta= Expr.Typed.Meta.({adlevel; _})}) : bool =
  if adlevel = UnsizedType.DataOnly then true
  else
    let query_expr = query_soa_supported_assign_expr in
    match pattern with
    | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
        query_soa_supported_assign_fun kind exprs
    | Indexed (expr, (_ : Typed.Meta.t Fixed.t Index.t sexp_list)) ->
        query_expr expr
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string))
      ->
        true
    | TernaryIf _ -> false
    (*I think we can just return true for this?*)
    | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_expr lhs && query_expr rhs

(**
 * Return false if all of an assignments rhs expressions do not support SoA.
 *)
and query_soa_supported_assign_fun (kind : 'a Fun_kind.t)
    (exprs : Typed.Meta.t Expr.Fixed.t list) : bool =
  match kind with
  | CompilerInternal (Internal_fun.FnMakeArray | FnMakeRowVec) -> false
  | UserDefined ((_ : string), (_ : bool Fun_kind.suffix)) -> false
  | CompilerInternal (_ : 'a Internal_fun.t) -> true
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
    match name with
    | "check_matching_dims" -> true
    | _ ->
        is_fun_soa_supported name exprs
        && List.exists ~f:query_soa_supported_assign_expr exprs )

(**
 * Return true if the rhs expression of an assignment contains only
 *  combinations of AutoDiffable Reals and Data Matrices
 *)
let rec query_ad_real_data_matrix_assign
    Expr.Fixed.({pattern; meta= Expr.Typed.Meta.({adlevel; _})}) : bool =
  if adlevel = UnsizedType.DataOnly then false
  else
    match pattern with
    | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
        query_ad_real_data_matrix_assign_fun kind exprs
    | Indexed (expr, _) -> query_ad_real_data_matrix_assign expr
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string))
      ->
        false
    | TernaryIf (_, texpr, fexpr) ->
        query_ad_real_data_matrix_assign texpr
        || query_ad_real_data_matrix_assign fexpr
    | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
        query_ad_real_data_matrix_assign lhs
        && query_ad_real_data_matrix_assign rhs

(**
 * Return true if the expressions in a function call are all
 *  combinations of AutoDiffable Reals and Data Matrices
 *)
and query_ad_real_data_matrix_assign_fun (kind : 'a Fun_kind.t)
    (exprs : Typed.Meta.t Expr.Fixed.t list) : bool =
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
    match name with
    | "check_matching_dims" -> false
    | _ -> (
        let fun_args = List.map ~f:Expr.Typed.fun_arg exprs in
        (*Right now we can't handle AD real and data matrix funcs
           that return a matrix :-/*)
        let is_args_autodiff_real_data_matrix =
          (*If there are any autodiffable vars*)
          List.exists
            ~f:(fun (x, y) ->
              match (x, y) with
              | UnsizedType.AutoDiffable, UnsizedType.UReal -> true
              | _ -> false )
            fun_args
          (*And there are any data matrices*)
          && List.exists
               ~f:(fun (x, y) ->
                 match (x, UnsizedType.is_container y) with
                 | UnsizedType.DataOnly, true -> true
                 | _ -> false )
               fun_args
          (*And there are no Autodiffable matrices*)
          && List.exists
               ~f:(fun (x, y) ->
                 match (x, UnsizedType.contains_eigen_type y) with
                 | UnsizedType.AutoDiffable, true -> false
                 | _ -> true )
               fun_args
        in
        match is_args_autodiff_real_data_matrix with
        | true -> true
        | false -> List.exists ~f:query_ad_real_data_matrix_assign exprs ) )
  | CompilerInternal (Internal_fun.FnMakeArray | FnMakeRowVec) -> true
  | CompilerInternal (_ : 'a Internal_fun.t) -> false
  | UserDefined ((_ : string), (_ : bool Fun_kind.suffix)) -> false

(**
 * Query to find the initial set of objects in statements that cannot be SoA.
 * This is mostly recursive over expressions and statements, with the exception of 
 * functions and Assignments.
 *
 * For assignments:
 *  We demote the LHS variable if any of the following are true:
 *  1. None of the RHS's functions are able to accept SoA matrices
 *  2. A single cell of the LHS is being assigned within a loop.
 *  3. The top level expression on the RHS is a combination of only 
 *   data matrices and scalar types. Operations on data matrix and
 *   scalar values in Stan math will return a AoS matrix. We currently
 *   have no way to tell Stan math to return a SoA matrix.
 *
 *  We demote RHS variables if any of the following are true:
 *  1. The LHS variable has previously or through this iteration
 *   been marked AoS.
 *
 * For functions see the documentation for `query_initial_demotable_funs` for
 *  the logic on demotion rules.
 * @param in_loop A boolean to specify the logic of indexing expressions. See
 *  `query_initial_demotable_expr` for an explanation of the logic.
 *)
let rec query_initial_demotable_stmt (in_loop : bool)
    (Stmt.Fixed.({pattern; _}) :
      (Expr.Typed.Meta.t, Stmt.Located.Meta.t) Stmt.Fixed.t) :
    string Set.Poly.t =
  let query_expr = query_initial_demotable_expr in_loop in
  match pattern with
  | Stmt.Fixed.Pattern.Assignment
      ( ((name : string), (ut : UnsizedType.t), idx)
      , (Expr.Fixed.({meta= Expr.Typed.Meta.({type_; adlevel; _}); _}) as rhs)
      ) ->
      let idx_demotable =
        let idx_list =
          Set.Poly.union_list
            (List.map
               ~f:
                 (Index.apply ~default:Set.Poly.empty ~merge:Set.Poly.union
                    query_expr)
               idx)
        in
        (* RHS (2)*)
        match is_uni_eigen_loop_indexing in_loop ut idx with
        | true -> Set.Poly.add idx_list name
        | false -> idx_list
      in
      let rhs_demotable = query_expr rhs in
      (* RHS (3)*)
      let check_bad_assign =
        match (UnsizedType.contains_eigen_type type_, adlevel) with
        | true, UnsizedType.AutoDiffable ->
            query_ad_real_data_matrix_assign rhs
            || not (query_soa_supported_assign_expr rhs)
        | _ -> false
      in
      (* RHS (1)*)
      let is_all_rhs_aos =
        Set.Poly.length rhs_demotable = Set.Poly.length (query_eigen_names rhs)
        && Set.Poly.length rhs_demotable <> 0
      in
      if is_all_rhs_aos || check_bad_assign then
        let base_set = Set.Poly.union idx_demotable rhs_demotable in
        Set.Poly.add (Set.Poly.union base_set (query_var_eigen_names rhs)) name
      else Set.Poly.union idx_demotable rhs_demotable
  | NRFunApp (kind, exprs) -> query_initial_demotable_funs in_loop kind exprs
  | IfElse (predicate, lhs, rhs) ->
      let query_rhs =
        Option.value_map
          ~f:(query_initial_demotable_stmt in_loop)
          ~default:Set.Poly.empty rhs
      in
      Set.Poly.union_list
        [ query_expr predicate
        ; query_initial_demotable_stmt in_loop lhs
        ; query_rhs ]
  | Return optional_expr ->
      Option.value_map ~f:query_expr ~default:Set.Poly.empty optional_expr
  | SList lst | Profile (_, lst) | Block lst ->
      Set.Poly.union_list
        (List.map ~f:(query_initial_demotable_stmt in_loop) lst)
  | TargetPE expr -> query_expr expr
  | For {lower; upper; body; _} ->
      Set.Poly.union
        (Set.Poly.union (query_expr lower) (query_expr upper))
        (query_initial_demotable_stmt true body)
  | While (predicate, body) ->
      Set.Poly.union (query_expr predicate)
        (query_initial_demotable_stmt true body)
  | Skip | Break | Continue | Decl _ -> Set.Poly.empty

(* Look through a statement to see whether it needs modified from
 * SoA to AoS. Returns the set of object names that need demoted
 * in a statement, if any.
 * @param aos_exits A set of variables that can be demoted.
 * @param pattern The Stmt pattern to query.
 *)
let query_demotable_stmt (aos_exits : string Set.Poly.t)
    (pattern : (Typed.t, int) Stmt.Fixed.Pattern.t) : string Set.Poly.t =
  match pattern with
  | Stmt.Fixed.Pattern.Assignment
      ( ( (assign_name : string)
        , (_ : UnsizedType.t)
        , (_ : Expr.Typed.t Index.t list) )
      , rhs ) ->
      let all_rhs_eigen_names = query_eigen_names rhs in
      let rhs_set =
        match Set.Poly.mem aos_exits assign_name with
        | true -> all_rhs_eigen_names
        | false -> Set.Poly.empty
      in
      let lhs_set =
        match
          Set.Poly.is_subset ~of_:aos_exits all_rhs_eigen_names
          && (not (Set.Poly.is_empty aos_exits))
          && not (Set.Poly.is_empty all_rhs_eigen_names)
        with
        | true -> Set.Poly.singleton assign_name
        | false -> Set.Poly.empty
      in
      Set.Poly.union rhs_set lhs_set
  (* All other statements do not need logic here*)
  | _ -> Set.Poly.empty

(**
 * Search through an expression for the names of all types that hold matrices 
 *  and vectors.
 **)
let query_names (expr : Typed.Meta.t Expr.Fixed.t) : string Set.Poly.t =
  let get_expr_names (Dataflow_types.VVar s, _) = Some s in
  Set.Poly.filter_map ~f:get_expr_names (expr_set expr)

(**
 * Modify a function and it's subexpressions from SoA <-> AoS and vice versa.
 * @param modifiable_set The set of eigen names that are either demotable 
 *  to AoS or promotable to SoA.
 * @param mem_pattern If AoS, checks if all names in the functions
 *  expressions are in the modifiable set and demotes the function to SoA.
 *  Else if SoA, checks if any of the names in the modifiable set
 *   and if so then promotes the function to SoA.
 * @param exprs A list of expressions going into the function.
 **)
let rec modify_kind ?force_demotion:(force = false)
    (modifiable_set : string Set.Poly.t) (kind : 'a Fun_kind.t)
    (exprs : Expr.Typed.Meta.t Expr.Fixed.t list) =
  let expr_names = Set.Poly.union_list (List.map ~f:query_names exprs) in
  let is_all_in_list =
    Set.Poly.is_subset ~of_:modifiable_set expr_names
    && (not (Set.Poly.is_empty modifiable_set))
    && not (Set.Poly.is_empty expr_names)
  in
  match kind with
  | Fun_kind.StanLib (name, sfx, (_ : Common.Helpers.mem_pattern)) ->
      if is_all_in_list || (not (is_fun_soa_supported name exprs)) || force
      then
        (*Force demotion of all subexprs*)
        let exprs' =
          List.map ~f:(modify_expr ~force_demotion:true expr_names) exprs
        in
        (Fun_kind.StanLib (name, sfx, Common.Helpers.AoS), exprs')
      else
        ( Fun_kind.StanLib (name, sfx, SoA)
        , List.map ~f:(modify_expr ~force_demotion:force modifiable_set) exprs
        )
  | UserDefined _ as udf ->
      (udf, List.map ~f:(modify_expr ~force_demotion:true modifiable_set) exprs)
  | (_ : 'a Fun_kind.t) ->
      ( kind
      , List.map ~f:(modify_expr ~force_demotion:force modifiable_set) exprs )

(** 
* Modify the expressions to demote/promote from AoS <-> SoA and vice versa
* The only real path in the below is on the functions.
*
* For AoS, we check that *all* of the matrix and vector names
*  in the list of expressions are in the `modifiable_set` and if so
*  make the function AoS. 
* For SoA, we check that *any* of the matrix and vector names
*  in the list of expressions are in the `modifiable_set` and if so
*  make the function SoA. 
* @param mem_pattern The memory pattern to change functions to.
* @param modifiable_set The name of the variables whose
*  associated expressions we want to modify.
* @param pattern The expression to modify.
*)
and modify_expr_pattern ?force_demotion:(force = false)
    (modifiable_set : string Set.Poly.t)
    (pattern : Expr.Typed.Meta.t Expr.Fixed.t Expr.Fixed.Pattern.t) =
  let mod_expr ?force_demotion:(forced = false) =
    modify_expr ~force_demotion:forced modifiable_set
  in
  match pattern with
  | Expr.Fixed.Pattern.FunApp
      (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      let kind', expr' =
        modify_kind ~force_demotion:force modifiable_set kind exprs
      in
      Expr.Fixed.Pattern.FunApp (kind', expr')
  | TernaryIf (predicate, texpr, fexpr) ->
      let is_eigen_return =
        UnsizedType.contains_eigen_type fexpr.meta.type_
        || UnsizedType.contains_eigen_type texpr.meta.type_
      in
      if is_eigen_return then
        (*If either side fails we need to force both sides to match*)
        TernaryIf
          ( mod_expr ~force_demotion:force predicate
          , mod_expr ~force_demotion:true texpr
          , mod_expr ~force_demotion:true fexpr )
      else
        TernaryIf
          ( mod_expr ~force_demotion:force predicate
          , mod_expr ~force_demotion:force texpr
          , mod_expr ~force_demotion:force fexpr )
  | Indexed (idx_expr, indexed) ->
      Indexed (mod_expr idx_expr, List.map ~f:(Index.map mod_expr) indexed)
  | EAnd (lhs, rhs) -> EAnd (mod_expr lhs, mod_expr rhs)
  | EOr (lhs, rhs) -> EOr (mod_expr lhs, mod_expr rhs)
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      pattern

(** 
* Given a Set of strings containing the names of objects that can be 
* modified from AoS <-> SoA and vice versa, modify them within the expression.
* @param mem_pattern The memory pattern to change expressions to.
* @param modifiable_set The name of the variables whose
*  associated expressions we want to modify.
* @param expr the expression to modify.
*)
and modify_expr ?force_demotion:(force = false)
    (modifiable_set : string Set.Poly.t) (Expr.Fixed.({pattern; _}) as expr) =
  { expr with
    pattern= modify_expr_pattern ~force_demotion:force modifiable_set pattern
  }

(**
* Modify statement patterns in the MIR from AoS <-> SoA and vice versa 
* @param mem_pattern A mem_pattern to modify expressions to. For the 
*  given memory pattern, this modifies 
*  statement patterns and expressions to it.
* @param pattern The statement pattern to modify 
* @param modifiable_set The name of the variable we are searching for.
*)
let rec modify_stmt_pattern
    (pattern :
      ( Expr.Typed.Meta.t Expr.Fixed.t
      , (Expr.Typed.Meta.t, Stmt.Located.Meta.t) Stmt.Fixed.t )
      Stmt.Fixed.Pattern.t) (modifiable_set : string Core_kernel.Set.Poly.t) =
  let mod_expr = Mir_utils.map_rec_expr (modify_expr_pattern modifiable_set) in
  let mod_stmt stmt = modify_stmt stmt modifiable_set in
  match pattern with
  | Stmt.Fixed.Pattern.Decl
      ({decl_id; decl_type= Type.Sized sized_type; _} as decl) ->
      if Set.Poly.mem modifiable_set decl_id then
        Stmt.Fixed.Pattern.Decl
          { decl with
            decl_type=
              Type.Sized (SizedType.modify_sizedtype_mem AoS sized_type) }
      else
        Decl
          { decl with
            decl_type=
              Type.Sized (SizedType.modify_sizedtype_mem SoA sized_type) }
  | NRFunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      let kind', exprs' = modify_kind modifiable_set kind exprs in
      NRFunApp (kind', exprs')
  | Assignment
      ( (name, ut, lhs)
      , ( {pattern= FunApp (CompilerInternal (FnReadParam read_param), args); _}
        as assigner ) ) ->
      if Set.Poly.mem modifiable_set name then
        Assignment
          ( (name, ut, List.map ~f:(Index.map mod_expr) lhs)
          , { assigner with
              pattern=
                FunApp
                  ( CompilerInternal
                      (FnReadParam {read_param with mem_pattern= AoS})
                  , List.map ~f:mod_expr args ) } )
      else
        Assignment
          ( (name, ut, List.map ~f:(Index.map mod_expr) lhs)
          , { assigner with
              pattern=
                FunApp
                  ( CompilerInternal
                      (FnReadParam {read_param with mem_pattern= SoA})
                  , List.map ~f:mod_expr args ) } )
  | Assignment (((name : string), (ut : UnsizedType.t), idx), rhs) ->
      if Set.Poly.mem modifiable_set name then
        (*If assignee is in bad set, force demotion of rhs functions*)
        let all_expr_var_names = query_names rhs in
        let mod_assign_expr =
          Mir_utils.map_rec_expr (modify_expr_pattern all_expr_var_names)
        in
        Assignment
          ( (name, ut, List.map ~f:(Index.map mod_assign_expr) idx)
          , mod_assign_expr rhs )
      else
        Assignment
          ((name, ut, List.map ~f:(Index.map mod_expr) idx), mod_expr rhs)
  | IfElse (predicate, true_stmt, op_false_stmt) ->
      IfElse
        ( mod_expr predicate
        , mod_stmt true_stmt
        , Option.map ~f:mod_stmt op_false_stmt )
  | Block stmts -> Block (List.map ~f:mod_stmt stmts)
  | SList stmts -> SList (List.map ~f:mod_stmt stmts)
  | For ({lower; upper; body; _} as loop) ->
      Stmt.Fixed.Pattern.For
        { loop with
          lower= mod_expr lower; upper= mod_expr upper; body= mod_stmt body }
  | TargetPE expr -> TargetPE (mod_expr expr)
  | Return optional_expr -> Return (Option.map ~f:mod_expr optional_expr)
  | Profile ((p_name : string), stmt) ->
      Profile (p_name, List.map ~f:mod_stmt stmt)
  | While (predicate, body) -> While (mod_expr predicate, mod_stmt body)
  | Skip | Break | Continue | Decl _ -> pattern

(**
* Modify statement patterns in the MIR from AoS <-> SoA and vice versa 
* @param mem_pattern A mem_pattern to modify expressions to. For the 
*  given memory pattern, this modifies 
*  statement patterns and expressions to it.
* @param stmt The statement to modify. 
* @param modifiable_set The name of the variable we are searching for.
*)
and modify_stmt (Stmt.Fixed.({pattern; _}) as stmt)
    (modifiable_set : string Set.Poly.t) =
  {stmt with pattern= modify_stmt_pattern pattern modifiable_set}
