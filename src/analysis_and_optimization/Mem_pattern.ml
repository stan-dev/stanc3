open Core_kernel
open Core_kernel.Poly
open Middle
open Middle.Expr

(**
 * Return a Var expression of the name for each type
 *  containing an eigen matrix
 *)
let rec matrix_set Expr.Fixed.{pattern; meta= Expr.Typed.Meta.{type_; _} as meta}
    =
  let union_recur exprs = Set.Poly.union_list (List.map exprs ~f:matrix_set) in
  if UnsizedType.contains_eigen_type type_ then
    match pattern with
    | Var s -> Set.Poly.singleton (Dataflow_types.VVar s, meta)
    | Lit _ -> Set.Poly.empty
    | FunApp (_, exprs) ->
        if UnsizedType.contains_eigen_type type_ then union_recur exprs
        else Set.Poly.empty
    | TernaryIf (_, expr2, expr3) -> union_recur [expr2; expr3]
    | Indexed (expr, _) | Promotion (expr, _, _) -> matrix_set expr
    | EAnd (expr1, expr2) | EOr (expr1, expr2) -> union_recur [expr1; expr2]
  else Set.Poly.empty

(**
 * Return a set of all types containing autodiffable Eigen matrices
 *  in an expression.
 *)
let query_var_eigen_names (expr : Typed.Meta.t Expr.Fixed.t) : string Set.Poly.t
    =
  let get_expr_eigen_names
      (Dataflow_types.VVar s, Expr.Typed.Meta.{adlevel; type_; _}) =
    if
      UnsizedType.contains_eigen_type type_
      && UnsizedType.is_autodifftype adlevel
    then Some s
    else None in
  Set.Poly.filter_map ~f:get_expr_eigen_names (matrix_set expr)

(**
 * Check whether one set is a nonzero subset of another set.
 *)
let is_nonzero_subset ~set ~subset =
  Set.Poly.is_subset subset ~of_:set
  && (not (Set.Poly.is_empty set))
  && not (Set.Poly.is_empty subset)

(**
 * Check an expression to count how many times we see a single index.
 * @param acc An accumulator from previous folds of multiple expressions.
 * @param pattern The expression patterns to match against
 *)
let rec count_single_idx_exprs (acc : int) Expr.Fixed.{pattern; _} : int =
  match pattern with
  | Expr.Fixed.Pattern.FunApp (_, (exprs : Typed.Meta.t Expr.Fixed.t list)) ->
      List.fold_left ~init:acc ~f:count_single_idx_exprs exprs
  | TernaryIf (predicate, texpr, fexpr) ->
      acc
      + count_single_idx_exprs 0 predicate
      + count_single_idx_exprs 0 texpr
      + count_single_idx_exprs 0 fexpr
  | Indexed (idx_expr, indexed) ->
      acc
      + count_single_idx_exprs 0 idx_expr
      + List.fold_left ~init:0 ~f:count_single_idx indexed
  | Promotion (expr, _, _) -> count_single_idx_exprs acc expr
  | EAnd (lhs, rhs) ->
      acc + count_single_idx_exprs 0 lhs + count_single_idx_exprs 0 rhs
  | EOr (lhs, rhs) ->
      acc + count_single_idx_exprs 0 lhs + count_single_idx_exprs 0 rhs
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
  | Index.All | Between _ | Upfrom _ | MultiIndex _ -> acc
  | Single _ -> acc + 1

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
        List.fold_left ~init:0 ~f:count_single_idx index in
      match (ut, index) with
      | (UnsizedType.UVector | URowVector), _ when contains_single_idx > 0 ->
          true
      | UMatrix, _ when contains_single_idx > 1 -> true
      | (UArray t | UFun (_, ReturnType t, _, _)), index -> (
        match List.tl index with
        | Some cut_list -> is_uni_eigen_loop_indexing in_loop t cut_list
        | None -> false )
      | _ -> false )

let query_stan_math_mem_pattern_support (name : string)
    (args : (UnsizedType.autodifftype * UnsizedType.t) list) =
  let open Stan_math_signatures in
  match name with
  | x when is_reduce_sum_fn x -> false
  | x when is_variadic_ode_fn x -> false
  | x when is_variadic_dae_fn x -> false
  | _ ->
      let name =
        string_operator_to_stan_math_fns (Utils.stdlib_distribution_name name)
      in
      let namematches = Hashtbl.find_multi stan_math_signatures name in
      let filteredmatches =
        List.filter
          ~f:(fun x ->
            Frontend.SignatureMismatch.check_compatible_arguments_mod_conv
              (snd3 x) args
            |> Result.is_ok )
          namematches in
      let is_soa ((_ : UnsizedType.returntype), _, mem) =
        mem = Common.Helpers.SoA in
      List.exists ~f:is_soa filteredmatches

(*Validate whether a function can support SoA matrices*)
let is_fun_soa_supported name exprs =
  let fun_args = List.map ~f:Expr.Typed.fun_arg exprs in
  query_stan_math_mem_pattern_support name fun_args

(**
 * Query to find the initial set of objects that cannot be SoA.
 *  This is mostly recursing over expressions, with the exceptions
 *  being functions and indexing expressions. For the logic on functions
 *  see the docs for `query_initial_demotable_funs`.
 * @param in_loop a boolean to signify if the expression exists inside
 *  of a loop. If so, the names of matrix and vector like objects
 *   will be returned if the matrix or vector is accessed by single
 *    cell indexing.
 *)
let rec query_initial_demotable_expr (in_loop : bool) ~(acc : string Set.Poly.t)
    Expr.Fixed.{pattern; _} : string Set.Poly.t =
  let query_expr (accum : string Set.Poly.t) =
    query_initial_demotable_expr in_loop ~acc:accum in
  match pattern with
  | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      query_initial_demotable_funs in_loop acc kind exprs
  | Indexed ((Expr.Fixed.{meta= {type_; _}; _} as expr), indexed) ->
      let index_set =
        Set.Poly.union_list
          (List.map
             ~f:
               (Index.apply ~default:Set.Poly.empty ~merge:Set.Poly.union
                  (query_expr acc) )
             indexed ) in
      let index_demotes =
        if is_uni_eigen_loop_indexing in_loop type_ indexed then
          Set.Poly.union (query_var_eigen_names expr) index_set
        else Set.Poly.union (query_expr acc expr) index_set in
      Set.Poly.union acc index_demotes
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      acc
  | Promotion (expr, _, _) -> query_expr acc expr
  | TernaryIf (predicate, texpr, fexpr) ->
      let predicate_demotes = query_expr acc predicate in
      Set.Poly.union
        (Set.Poly.union predicate_demotes (query_var_eigen_names texpr))
        (query_var_eigen_names fexpr)
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      (*We need to get the demotes from both sides*)
      let full_lhs_rhs =
        Set.Poly.union (query_expr acc lhs) (query_expr acc rhs) in
      Set.Poly.union (query_expr full_lhs_rhs lhs) (query_expr full_lhs_rhs rhs)

(**
 * Query a function to detect if it or any of its used
 *  expression's objects or expressions should be demoted to AoS.
 *
 * The logic here demotes the expressions in a function to AoS if
 * the function's inner expression returns has a meta type containing a matrix
 * and either of :
 * (1) The function is user defined and the UDFs inputs are matrices.
 * (2) The Stan math function cannot support AoS
 * @param in_loop A boolean to specify the logic of indexing expressions. See
 *  `query_initial_demotable_expr` for an explanation of the logic.
 * @param kind The function type, for StanLib functions we check if the
 *  function supports SoA and for UserDefined functions we always fail
 *  and return back all of the names of the objects passed in expressions
 *  to the UDF.
 * exprs The expression list passed to the functions.
 *)
and query_initial_demotable_funs (in_loop : bool) (acc : string Set.Poly.t)
    (kind : 'a Fun_kind.t) (exprs : Typed.Meta.t Expr.Fixed.t list) :
    string Set.Poly.t =
  let query_expr accum = query_initial_demotable_expr in_loop ~acc:accum in
  let top_level_eigen_names =
    Set.Poly.union_list (List.map ~f:query_var_eigen_names exprs) in
  let demoted_eigen_names = List.fold ~init:acc ~f:query_expr exprs in
  let demoted_and_top_level_names =
    Set.Poly.union demoted_eigen_names top_level_eigen_names in
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
    match name with
    | "check_matching_dims" -> acc
    | name -> (
      match is_fun_soa_supported name exprs with
      | true -> Set.Poly.union acc demoted_eigen_names
      | false -> Set.Poly.union acc demoted_and_top_level_names ) )
  | CompilerInternal (Internal_fun.FnMakeArray | FnMakeRowVec) ->
      Set.Poly.union acc demoted_and_top_level_names
  | CompilerInternal (_ : 'a Internal_fun.t) -> acc
  | UserDefined ((_ : string), (_ : bool Fun_kind.suffix)) ->
      Set.Poly.union acc demoted_and_top_level_names

(**
 * Check whether any functions in the right hand side expression of an assignment
 * support SoA. If so then return true, otherwise return false.
 *)
let rec is_any_soa_supported_expr
    Expr.Fixed.{pattern; meta= Expr.Typed.Meta.{adlevel; type_; _}} : bool =
  if
    UnsizedType.is_dataonlytype adlevel
    || not (UnsizedType.contains_eigen_type type_)
  then true
  else
    match pattern with
    | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
        is_any_soa_supported_fun_expr kind exprs
    | Indexed (expr, (_ : Typed.Meta.t Fixed.t Index.t list))
     |Promotion (expr, _, _) ->
        is_any_soa_supported_expr expr
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
        true
    | TernaryIf (_, texpr, fexpr) ->
        is_any_soa_supported_expr texpr && is_any_soa_supported_expr fexpr
    | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
        is_any_soa_supported_expr lhs && is_any_soa_supported_expr rhs

(**
 * Return false if the `Fun_kind.t` does not support `SoA`
 *)
and is_any_soa_supported_fun_expr (kind : 'a Fun_kind.t)
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
        && List.exists ~f:is_any_soa_supported_expr exprs )

(**
 * Return true if the rhs expression of an assignment contains only
 *  combinations of AutoDiffable Reals and Data Matrices
 *)
let rec is_any_ad_real_data_matrix_expr
    Expr.Fixed.{pattern; meta= Expr.Typed.Meta.{adlevel; _}} : bool =
  if UnsizedType.is_dataonlytype adlevel then false
  else
    match pattern with
    | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
        is_any_ad_real_data_matrix_expr_fun kind exprs
    | Indexed (expr, _) | Promotion (expr, _, _) ->
        is_any_ad_real_data_matrix_expr expr
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
        false
    | TernaryIf (_, texpr, fexpr) ->
        is_any_ad_real_data_matrix_expr texpr
        || is_any_ad_real_data_matrix_expr fexpr
    | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
        is_any_ad_real_data_matrix_expr lhs
        && is_any_ad_real_data_matrix_expr rhs

(**
 * Return true if the expressions in a function call are all
 *  combinations of AutoDiffable Reals and Data Matrices
 *)
and is_any_ad_real_data_matrix_expr_fun (kind : 'a Fun_kind.t)
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
               fun_args in
        match is_args_autodiff_real_data_matrix with
        | true -> true
        | false -> List.exists ~f:is_any_ad_real_data_matrix_expr exprs ) )
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
 *   and the rhs is not an internal compiler function.
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
let rec query_initial_demotable_stmt (in_loop : bool) (acc : string Set.Poly.t)
    (Stmt.Fixed.{pattern; _} :
      (Expr.Typed.Meta.t, Stmt.Located.Meta.t) Stmt.Fixed.t ) :
    string Set.Poly.t =
  let query_expr (accum : string Set.Poly.t) =
    query_initial_demotable_expr in_loop ~acc:accum in
  match pattern with
  | Stmt.Fixed.Pattern.Assignment
      ( ((name : string), (ut : UnsizedType.t), idx)
      , (Expr.Fixed.{meta= Expr.Typed.Meta.{type_; adlevel; _}; _} as rhs) ) ->
      let idx_list =
        List.fold ~init:acc
          ~f:(fun accum x ->
            Index.folder accum
              (fun acc -> query_initial_demotable_expr in_loop ~acc)
              x )
          idx in
      let idx_demotable =
        (* RHS (2)*)
        match is_uni_eigen_loop_indexing in_loop ut idx with
        | true -> Set.Poly.add idx_list name
        | false -> idx_list in
      let rhs_demotable_names = query_expr acc rhs in
      (* RHS (3)*)
      let check_if_rhs_ad_real_data_matrix_expr =
        match (UnsizedType.contains_eigen_type type_, adlevel) with
        | true, UnsizedType.AutoDiffable ->
            is_any_ad_real_data_matrix_expr rhs
            || not (is_any_soa_supported_expr rhs)
        | _ -> false in
      (* RHS (1)*)
      let is_all_rhs_aos =
        let all_rhs_eigen_names = query_var_eigen_names rhs in
        is_nonzero_subset ~subset:all_rhs_eigen_names ~set:rhs_demotable_names
      in
      let is_not_supported_func =
        match rhs.pattern with
        | FunApp (CompilerInternal _, _) -> false
        | FunApp (UserDefined _, _) -> true
        | _ -> false in
      let is_eigen_stmt = UnsizedType.contains_eigen_type rhs.meta.type_ in
      let assign_demotes =
        if
          is_eigen_stmt
          && ( is_all_rhs_aos || check_if_rhs_ad_real_data_matrix_expr
             || is_not_supported_func )
        then
          let base_set = Set.Poly.union idx_demotable rhs_demotable_names in
          Set.Poly.add
            (Set.Poly.union base_set (query_var_eigen_names rhs))
            name
        else Set.Poly.union idx_demotable rhs_demotable_names in
      Set.Poly.union acc assign_demotes
  | NRFunApp (kind, exprs) ->
      query_initial_demotable_funs in_loop acc kind exprs
  | IfElse (predicate, true_stmt, op_false_stmt) ->
      let predicate_acc = query_expr acc predicate in
      Set.Poly.union acc
        (Set.Poly.union_list
           [ predicate_acc
           ; query_initial_demotable_stmt in_loop predicate_acc true_stmt
           ; Option.value_map
               ~f:(query_initial_demotable_stmt in_loop predicate_acc)
               ~default:Set.Poly.empty op_false_stmt ] )
  | Return optional_expr ->
      Option.value_map ~f:(query_expr acc) ~default:Set.Poly.empty optional_expr
  | SList lst | Profile (_, lst) | Block lst ->
      Set.Poly.union_list
        (List.map ~f:(query_initial_demotable_stmt in_loop acc) lst)
  | TargetPE expr -> query_expr acc expr
  | For {lower; upper; body; _} ->
      Set.Poly.union
        (Set.Poly.union (query_expr acc lower) (query_expr acc upper))
        (query_initial_demotable_stmt true acc body)
  | While (predicate, body) ->
      Set.Poly.union_list
        [ acc; query_expr acc predicate
        ; query_initial_demotable_stmt true acc body ]
  | Skip | Break | Continue | Decl _ -> acc

(** Look through a statement to see whether the objects used in it need to be
 *  modified from SoA to AoS. Returns the set of object names that need demoted
 * in a statement, if any.
 * This function looks at Assignment statements, and returns back the
 *  set of top level object names given:
 * 1. If the name of the lhs assignee is in the `aos_exits`, all the names
 *  of the expressions with a type containing a matrix are returned.
 * 2. If the names of the rhs objects containing matrix types are in the subset of
 *  aos_exits.
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
      , (rhs : Expr.Typed.t) ) -> (
      let all_rhs_eigen_names = query_var_eigen_names rhs in
      if Set.Poly.mem aos_exits assign_name then
        Set.Poly.add all_rhs_eigen_names assign_name
      else
        match is_nonzero_subset ~set:aos_exits ~subset:all_rhs_eigen_names with
        | true -> Set.Poly.add all_rhs_eigen_names assign_name
        | false -> Set.Poly.empty )
  (* All other statements do not need logic here*)
  | _ -> Set.Poly.empty

(**
 * Modify a function and it's subexpressions from SoA <-> AoS and vice versa.
 * This performs demotion for sub expressions recursively. The top level
 *  expression and it's sub expressions are demoted to SoA if
 *  1. The names of the variables in the subexpressions returning
 *   objects holding matrices are all in the modifiable set.
 *  2. The function does not support SoA
 *  3. The `force` argument is `true`
 * @param force_demotion If true, forces an expression and it's sub-expressions
 *  to be AoS.
 * @param modifiable_set The set of names that are either demotable
 *  to AoS or promotable to SoA.
 * @param kind A `Fun_kind.t`
 * @param exprs A list of expressions going into the function.
 **)
let rec modify_kind ?force_demotion:(force = false)
    (modifiable_set : string Set.Poly.t) (kind : 'a Fun_kind.t)
    (exprs : Expr.Typed.Meta.t Expr.Fixed.t list) =
  let expr_names =
    Set.Poly.union_list (List.map ~f:query_var_eigen_names exprs) in
  let is_all_in_list =
    is_nonzero_subset ~set:modifiable_set ~subset:expr_names in
  match kind with
  | Fun_kind.StanLib (name, sfx, (_ : Common.Helpers.mem_pattern)) ->
      if is_all_in_list || (not (is_fun_soa_supported name exprs)) || force then
        (*Force demotion of all subexprs*)
        let exprs' =
          List.map ~f:(modify_expr ~force_demotion:true expr_names) exprs in
        (Fun_kind.StanLib (name, sfx, Common.Helpers.AoS), exprs')
      else
        ( Fun_kind.StanLib (name, sfx, SoA)
        , List.map ~f:(modify_expr ~force_demotion:force modifiable_set) exprs
        )
  | UserDefined _ as udf ->
      (udf, List.map ~f:(modify_expr ~force_demotion:force modifiable_set) exprs)
  | (_ : 'a Fun_kind.t) ->
      ( kind
      , List.map ~f:(modify_expr ~force_demotion:force modifiable_set) exprs )

(**
 * Modify an expression and it's subexpressions from SoA <-> AoS
 *  and vice versa. The only real paths in the below is on the
 *  functions and ternary expressions.
 *
 * The logic for functions is defined in `modify_kind`.
 * `TernaryIf` is forcefully demoted to AoS if the type of the expression
 *  contains a matrix.
 * @param force_demotion If true, forces an expression and it's sub-expressions
 *  to be AoS.
 * @param modifiable_set The name of the variables whose
 *  associated expressions we want to modify.
 * @param pattern The expression to modify.
 *)
and modify_expr_pattern ?force_demotion:(force = false)
    (modifiable_set : string Set.Poly.t)
    (pattern : Expr.Typed.Meta.t Expr.Fixed.t Expr.Fixed.Pattern.t) =
  let mod_expr ?force_demotion:(forced = false) =
    modify_expr ~force_demotion:forced modifiable_set in
  match pattern with
  | Expr.Fixed.Pattern.FunApp
      (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      let kind', expr' =
        modify_kind ~force_demotion:force modifiable_set kind exprs in
      Expr.Fixed.Pattern.FunApp (kind', expr')
  | TernaryIf (predicate, texpr, fexpr) ->
      let is_eigen_return =
        UnsizedType.contains_eigen_type fexpr.meta.type_
        || UnsizedType.contains_eigen_type texpr.meta.type_ in
      if is_eigen_return then
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
      Indexed
        ( mod_expr idx_expr
        , List.map ~f:(Index.map (mod_expr ~force_demotion:force)) indexed )
  | EAnd (lhs, rhs) -> EAnd (mod_expr lhs, mod_expr rhs)
  | EOr (lhs, rhs) -> EOr (mod_expr lhs, mod_expr rhs)
  | Promotion (expr, type_, ad_level) ->
      Promotion (mod_expr expr, type_, ad_level)
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
    (modifiable_set : string Set.Poly.t) (Expr.Fixed.{pattern; _} as expr) =
  { expr with
    pattern= modify_expr_pattern ~force_demotion:force modifiable_set pattern }

(**
* Modify statement patterns in the MIR from AoS <-> SoA and vice versa
* For `Decl` and `Assignment`'s reading in parameters, we demote to AoS
*  if the `decl_id` (or assign name) is in the modifiable set and
* otherwise promote the statement to `SoA`.
* For general `Assignment` statements, we check if the assignee is in
* the demotable set. If so, we force demotion of all of the rhs expressions.
* All other statements recurse over their statements and expressions.
*
* @param pattern The statement pattern to modify
* @param modifiable_set The name of the variable we are searching for.
*)
let rec modify_stmt_pattern
    (pattern :
      ( Expr.Typed.Meta.t Expr.Fixed.t
      , (Expr.Typed.Meta.t, Stmt.Located.Meta.t) Stmt.Fixed.t )
      Stmt.Fixed.Pattern.t ) (modifiable_set : string Core_kernel.Set.Poly.t) =
  let mod_expr force = modify_expr ~force_demotion:force modifiable_set in
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
          ( (name, ut, List.map ~f:(Index.map (mod_expr false)) lhs)
          , { assigner with
              pattern=
                FunApp
                  ( CompilerInternal
                      (FnReadParam {read_param with mem_pattern= AoS})
                  , List.map ~f:(mod_expr true) args ) } )
      else
        Assignment
          ( (name, ut, List.map ~f:(Index.map (mod_expr false)) lhs)
          , { assigner with
              pattern=
                FunApp
                  ( CompilerInternal
                      (FnReadParam {read_param with mem_pattern= SoA})
                  , List.map ~f:(mod_expr false) args ) } )
  | Assignment (((name : string), (ut : UnsizedType.t), idx), rhs) ->
      if Set.Poly.mem modifiable_set name then
        (*If assignee is in bad set, force demotion of rhs functions*)
        Assignment
          ( (name, ut, List.map ~f:(Index.map (mod_expr false)) idx)
          , mod_expr true rhs )
      else
        Assignment
          ( (name, ut, List.map ~f:(Index.map (mod_expr false)) idx)
          , (mod_expr false) rhs )
  | IfElse (predicate, true_stmt, op_false_stmt) ->
      IfElse
        ( (mod_expr false) predicate
        , mod_stmt true_stmt
        , Option.map ~f:mod_stmt op_false_stmt )
  | Block stmts -> Block (List.map ~f:mod_stmt stmts)
  | SList stmts -> SList (List.map ~f:mod_stmt stmts)
  | For ({lower; upper; body; _} as loop) ->
      Stmt.Fixed.Pattern.For
        { loop with
          lower= mod_expr false lower
        ; upper= mod_expr false upper
        ; body= mod_stmt body }
  | TargetPE expr -> TargetPE ((mod_expr false) expr)
  | Return optional_expr ->
      Return (Option.map ~f:(mod_expr false) optional_expr)
  | Profile ((p_name : string), stmt) ->
      Profile (p_name, List.map ~f:mod_stmt stmt)
  | While (predicate, body) -> While ((mod_expr false) predicate, mod_stmt body)
  | Skip | Break | Continue | Decl _ -> pattern

(**
* Modify statement patterns in the MIR from AoS <-> SoA and vice versa
* @param mem_pattern A mem_pattern to modify expressions to. For the
*  given memory pattern, this modifies
*  statement patterns and expressions to it.
* @param stmt The statement to modify.
* @param modifiable_set The name of the variable we are searching for.
*)
and modify_stmt (Stmt.Fixed.{pattern; _} as stmt)
    (modifiable_set : string Set.Poly.t) =
  {stmt with pattern= modify_stmt_pattern pattern modifiable_set}
