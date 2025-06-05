open Core
open Core.Poly
open Middle

(**
  Return a Var expression of the name for each type
   containing an eigen matrix
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
    | Indexed (expr, _) | Promotion (expr, _, _) | TupleProjection (expr, _) ->
        matrix_set expr
    | EAnd (expr1, expr2) | EOr (expr1, expr2) -> union_recur [expr1; expr2]
  else Set.Poly.empty

(**
  Return a set of all types containing autodiffable Eigen matrices
   in an expression.
 *)
let query_var_eigen_names (expr : Expr.Typed.t) : string Set.Poly.t =
  let get_expr_eigen_names
      (Dataflow_types.VVar s, Expr.Typed.Meta.{adlevel; type_; _}) =
    if
      UnsizedType.contains_eigen_type type_
      && UnsizedType.is_autodifftype adlevel
    then Some s
    else None in
  Set.Poly.filter_map ~f:get_expr_eigen_names (matrix_set expr)

(**
  Check whether one set is a nonzero subset of another set.
 *)
let is_nonzero_subset ~set ~subset =
  Set.is_subset subset ~of_:set
  && (not (Set.is_empty set))
  && not (Set.is_empty subset)

(**
 Check an Index to count how many times we see a single index.
 @param acc An accumulator from previous folds of multiple expressions.
 @param idx An Index to match. For Single types this adds 1 to the
   acc. For Upfrom and MultiIndex types we check the inner expression
   for a Single index. All and Between cannot be Single cell access
   and so pass acc along.
 *)
and count_single_idx (acc : int) (idx : Expr.Typed.t Index.t) =
  match idx with
  | Index.All | Between _ | Upfrom _ | MultiIndex _ -> acc
  | Single _ -> acc + 1

(**
  Find indices on Matrix and Vector types that perform single
   cell access. Returns true if it finds
  a vector, row vector, matrix, or matrix with single cell access
  as well as an array of any of the above that is accessing the
  inner matrix types cell.
  @param ut An UnsizedType to match against.
  @param index This list is checked for Single cell access
   either at the top level or within the [Index] types of the list.
 *)
let rec is_uni_eigen_loop_indexing in_loop (ut : UnsizedType.t)
    (index : Expr.Typed.t Index.t list) =
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
          | None -> false)
      | _ -> false)

let query_stan_math_mem_pattern_support (name : string)
    (args : UnsizedType.argumentlist) =
  let open Stan_math_signatures in
  if is_special_function_name name then false
  else
    let name =
      string_operator_to_stan_math_fns (Utils.stdlib_distribution_name name)
    in
    let namematches = lookup_stan_math_function name in
    let filteredmatches =
      List.filter
        ~f:(fun (x, _, _, _) ->
          Frontend.SignatureMismatch.check_compatible_arguments_mod_conv x args
          |> Result.is_ok)
        namematches in
    let is_soa = function _, _, _, Mem_pattern.SoA -> true | _ -> false in
    List.exists ~f:is_soa filteredmatches

(*Validate whether a function can support SoA matrices*)
let is_fun_soa_supported name exprs =
  let fun_args = List.map ~f:Expr.Typed.fun_arg exprs in
  query_stan_math_mem_pattern_support name fun_args

(**
  Query to find the initial set of objects that cannot be SoA.
   This is mostly recursing over expressions, with the exceptions
   being functions and indexing expressions. For the logic on functions
   see the docs for [query_initial_demotable_funs].
  @param in_loop a boolean to signify if the expression exists inside
   of a loop. If so, the names of matrix and vector like objects
    will be returned if the matrix or vector is accessed by single
     cell indexing.
 *)
let rec query_initial_demotable_expr (in_loop : bool) ~(acc : string Set.Poly.t)
    Expr.Fixed.{pattern; _} : string Set.Poly.t =
  let query_expr (accum : string Set.Poly.t) =
    query_initial_demotable_expr in_loop ~acc:accum in
  match pattern with
  | FunApp (kind, (exprs : Expr.Typed.t list)) ->
      query_initial_demotable_funs in_loop acc kind exprs
  | Indexed ((Expr.Fixed.{meta= {type_; _}; _} as expr), indexed) ->
      let index_set =
        Set.Poly.union_list
          (List.map
             ~f:
               (Index.apply ~default:Set.Poly.empty ~merge:Set.union
                  (query_expr acc))
             indexed) in
      let index_demotes =
        if is_uni_eigen_loop_indexing in_loop type_ indexed then
          Set.union (query_var_eigen_names expr) index_set
        else Set.union (query_expr acc expr) index_set in
      Set.union acc index_demotes
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      acc
  | Promotion (expr, _, _) -> query_expr acc expr
  | TupleProjection (expr, _) -> query_expr acc expr
  | TernaryIf (predicate, texpr, fexpr) ->
      let predicate_demotes = query_expr acc predicate in
      Set.union
        (Set.union predicate_demotes (query_var_eigen_names texpr))
        (query_var_eigen_names fexpr)
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      (*We need to get the demotes from both sides*)
      let full_lhs_rhs = Set.union (query_expr acc lhs) (query_expr acc rhs) in
      Set.union (query_expr full_lhs_rhs lhs) (query_expr full_lhs_rhs rhs)

(**
  Query a function to detect if it or any of its used
   expression's objects or expressions should be demoted to AoS.
 *
  The logic here demotes the expressions in a function to AoS if
  the function's inner expression returns has a meta type containing a matrix
  and either of :
  (1) The function is user defined and the UDFs inputs are matrices.
  (2) The Stan math function cannot support AoS
  @param in_loop A boolean to specify the logic of indexing expressions. See
   [query_initial_demotable_expr] for an explanation of the logic.
  @param kind The function type, for StanLib functions we check if the
   function supports SoA and for UserDefined functions we always fail
   and return back all of the names of the objects passed in expressions
   to the UDF.
  exprs The expression list passed to the functions.
 *)
and query_initial_demotable_funs (in_loop : bool) (acc : string Set.Poly.t)
    (kind : 'a Fun_kind.t) (exprs : Expr.Typed.t list) : string Set.Poly.t =
  let query_expr accum = query_initial_demotable_expr in_loop ~acc:accum in
  let top_level_eigen_names =
    Set.Poly.union_list (List.map ~f:query_var_eigen_names exprs) in
  let demoted_eigen_names = List.fold ~init:acc ~f:query_expr exprs in
  let demoted_and_top_level_names =
    Set.union demoted_eigen_names top_level_eigen_names in
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
      match name with
      | "check_matching_dims" -> acc
      | name -> (
          match is_fun_soa_supported name exprs with
          | true -> Set.union acc demoted_eigen_names
          | false -> Set.union acc demoted_and_top_level_names))
  | CompilerInternal (Internal_fun.FnMakeArray | FnMakeRowVec | FnMakeTuple) ->
      Set.union acc demoted_and_top_level_names
  | CompilerInternal (_ : 'a Internal_fun.t) -> acc
  | UserDefined ((_ : string), (_ : bool Fun_kind.suffix)) ->
      Set.union acc demoted_and_top_level_names

(**
  * Recurse through subexpressions and return a list of Unsized types.
  * Recursion continues until
  * 1. A non-autodiffable type is found
  * 2. An autodiffable scalar is found
  * 3. A `Var` type is found that is an autodiffable matrix
  *)
let rec extract_nonderived_admatrix_types
    Expr.Fixed.{pattern; meta= Expr.Typed.Meta.{adlevel; type_; _}} =
  if
    UnsizedType.is_autodifftype adlevel && UnsizedType.contains_eigen_type type_
  then
    match pattern with
    | FunApp (kind, (exprs : Expr.Typed.t list)) ->
        extract_nonderived_admatrix_types_fun kind exprs
    | Indexed (expr, _) | Promotion (expr, _, _) | TupleProjection (expr, _) ->
        extract_nonderived_admatrix_types expr
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
        [(adlevel, type_)]
    | TernaryIf (_, texpr, fexpr) ->
        List.concat
          [ extract_nonderived_admatrix_types texpr
          ; extract_nonderived_admatrix_types fexpr ]
    | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
        List.concat
          [ extract_nonderived_admatrix_types lhs
          ; extract_nonderived_admatrix_types rhs ]
  else [(adlevel, type_)]

(**
 * Recurse through functions to find nonderived ad matrix types.
 * Special cases for StanLib functions are for
 * - `check_matching_dims`: compiler function that has no effect on optimization
 * - `rep_*vector` These are templated in the C++ to cast up to `Var<Matrix>` types
 * - `rep_matrix`. When it's only a scalar being propogated an math library overload can upcast to `Var<Matrix>`
 *)
and extract_nonderived_admatrix_types_fun (kind : 'a Fun_kind.t)
    (exprs : Expr.Typed.t list) =
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
      match name with
      | "check_matching_dims" -> []
      | "rep_vector" -> [(UnsizedType.AutoDiffable, UnsizedType.UVector)]
      | "rep_row_vector" -> [(UnsizedType.AutoDiffable, UnsizedType.URowVector)]
      | "rep_matrix"
        when match List.map ~f:Expr.Typed.fun_arg exprs with
             | [(_, UnsizedType.UReal); _; _] -> true
             | _ -> false ->
          [(UnsizedType.AutoDiffable, UnsizedType.UMatrix)]
      | _ -> List.concat_map ~f:extract_nonderived_admatrix_types exprs)
  (*While not "true", we need to tell the optimizer these are danger functions*)
  | CompilerInternal Internal_fun.FnMakeArray ->
      [(AutoDiffable, UReal); (DataOnly, UArray UReal)]
  | CompilerInternal Internal_fun.FnMakeRowVec ->
      [(AutoDiffable, UReal); (DataOnly, URowVector)]
  | CompilerInternal (_ : 'a Internal_fun.t) -> []
  | UserDefined ((_ : string), (_ : bool Fun_kind.suffix)) -> []

(**Checks if a list of types contains at least on ad matrix or if everything is derived from data*)
let contains_at_least_one_ad_matrix_or_all_data
    (fun_args : UnsizedType.argumentlist) =
  List.is_empty fun_args
  || List.exists
       ~f:(fun x ->
         UnsizedType.is_autodifftype (fst x)
         && UnsizedType.is_eigen_type (snd x))
       fun_args
  || List.for_all ~f:(fun x -> UnsizedType.is_dataonlytype (fst x)) fun_args

(**
  Query to find the initial set of objects in statements that cannot be SoA.
  This is mostly recursive over expressions and statements, with the exception of
  functions and Assignments.
 *
  For assignments:
   We demote the LHS variable if any of the following are true:
   1. A single cell of the LHS is being assigned within a loop.
   2. The top level expression on the RHS is a combination of only
    data matrices and scalar types. Operations on data matrix and
    scalar values in Stan math will return a AoS matrix. We currently
    have no way to tell Stan math to return a SoA matrix.
   3. None of the RHS's functions are able to accept SoA matrices
    and the rhs is not an internal compiler function.
 *
   We demote RHS variables if any of the following are true:
   1. The LHS variable has previously or through this iteration
    been marked AoS.
   2. The LHS is a tuple projection
 *
  For functions see the documentation for [query_initial_demotable_funs] for
   the logic on demotion rules.
  @param in_loop A boolean to specify the logic of indexing expressions. See
   [query_initial_demotable_expr] for an explanation of the logic.
 *)
let rec query_initial_demotable_stmt (in_loop : bool) (acc : string Set.Poly.t)
    (Stmt.Fixed.{pattern; _} : Stmt.Located.t) : string Set.Poly.t =
  let query_expr (accum : string Set.Poly.t) =
    query_initial_demotable_expr in_loop ~acc:accum in
  match pattern with
  | Stmt.Fixed.Pattern.Assignment
      ( lval
      , (ut : UnsizedType.t)
      , (Expr.Fixed.{meta= Expr.Typed.Meta.{type_; adlevel; _}; _} as rhs) ) ->
      let name = Stmt.Helpers.lhs_variable lval in
      (* LHS (1)*)
      let idx_demotable =
        let idx = Stmt.Helpers.lhs_indices lval in
        let idx_list =
          List.fold ~init:acc
            ~f:(fun accum x ->
              Index.folder accum
                (fun acc -> query_initial_demotable_expr in_loop ~acc)
                x)
            idx in
        match is_uni_eigen_loop_indexing in_loop ut idx with
        | true -> Set.add idx_list name
        | false -> idx_list in
      let rhs_demotable_names = query_expr acc rhs in
      let rhs_and_idx_demotions = Set.union idx_demotable rhs_demotable_names in
      (* RHS (1)*)
      let tuple_demotions =
        match lval with
        | LTupleProjection _, _ ->
            Set.add
              (Set.union rhs_and_idx_demotions (query_var_eigen_names rhs))
              name
        | _ -> rhs_and_idx_demotions in
      let assign_demotions =
        let is_eigen_stmt = UnsizedType.contains_eigen_type rhs.meta.type_ in
        if is_eigen_stmt then
          (* LHS (2)*)
          let is_rhs_not_promoteable_to_soa =
            match (UnsizedType.contains_eigen_type type_, adlevel) with
            | true, UnsizedType.AutoDiffable ->
                not
                  (contains_at_least_one_ad_matrix_or_all_data
                     (extract_nonderived_admatrix_types rhs))
            | _ -> false in
          (* LHS (3) rhs unsupported function*)
          let is_not_supported_func =
            match rhs.pattern with
            | FunApp (UserDefined _, _) -> true
            | FunApp (CompilerInternal _, _) -> false
            | FunApp (StanLib (name, _, _), exprs) ->
                not
                  (query_stan_math_mem_pattern_support name
                     (List.map ~f:Expr.Typed.fun_arg exprs))
            | _ -> false in
          (* LHS (3) all rhs aos*)
          let is_all_rhs_aos =
            is_nonzero_subset
              ~subset:(query_var_eigen_names rhs)
              ~set:rhs_demotable_names in
          if
            is_all_rhs_aos || is_rhs_not_promoteable_to_soa
            || is_not_supported_func
          then
            Set.add (Set.union tuple_demotions (query_var_eigen_names rhs)) name
          else tuple_demotions
        else tuple_demotions in
      Set.union acc assign_demotions
  | NRFunApp (kind, exprs) ->
      query_initial_demotable_funs in_loop acc kind exprs
  | IfElse (predicate, true_stmt, op_false_stmt) ->
      let predicate_acc = query_expr acc predicate in
      Set.union acc
        (Set.Poly.union_list
           [ predicate_acc
           ; query_initial_demotable_stmt in_loop predicate_acc true_stmt
           ; Option.value_map
               ~f:(query_initial_demotable_stmt in_loop predicate_acc)
               ~default:Set.Poly.empty op_false_stmt ])
  | Return optional_expr ->
      Option.value_map ~f:(query_expr acc) ~default:Set.Poly.empty optional_expr
  | SList lst | Profile (_, lst) | Block lst ->
      Set.Poly.union_list
        (List.map ~f:(query_initial_demotable_stmt in_loop acc) lst)
  | TargetPE expr | JacobianPE expr -> query_expr acc expr
  (* NOTE: loops generated by inlining are not actually loops;
     we do not unconditionally set "in_loop" *)
  | For
      { lower= Expr.Fixed.{pattern= Lit (Int, lb); _}
      ; upper= Expr.Fixed.{pattern= Lit (Int, ub); _}
      ; body
      ; _ }
    when lb = "1" && ub = "1" ->
      query_initial_demotable_stmt in_loop acc body
  | For {lower; upper; body; _} ->
      Set.union
        (Set.union (query_expr acc lower) (query_expr acc upper))
        (query_initial_demotable_stmt true acc body)
  | While (predicate, body) ->
      Set.Poly.union_list
        [ acc; query_expr acc predicate
        ; query_initial_demotable_stmt true acc body ]
  | Decl {decl_type= Type.Sized st; decl_id; initialize; _} ->
      let complex_name =
        match SizedType.is_complex_type st with
        | true -> Set.Poly.singleton decl_id
        | false -> Set.Poly.empty in
      let init_names =
        match initialize with
        | Assign e -> query_expr acc e
        | _ -> Set.Poly.empty in
      Set.union acc (Set.union complex_name init_names)
  | Skip | Break | Continue | Decl _ -> acc

(** Look through a statement to see whether the objects used in it need to be
   modified from SoA to AoS. Returns the set of object names that need demoted
  in a statement, if any.
  This function looks at Assignment statements, and returns back the
   set of top level object names given:
  1. If the name of the lhs assignee is in the [aos_exits], all the names
   of the expressions with a type containing a matrix are returned.
  2. If the names of the rhs objects containing matrix types are in the subset of
   aos_exits.
  @param aos_exits A set of variables that can be demoted.
  @param pattern The Stmt pattern to query.
 *)
let query_demotable_stmt (aos_exits : string Set.Poly.t)
    (pattern : (Expr.Typed.t, int) Stmt.Fixed.Pattern.t) : string Set.Poly.t =
  match pattern with
  | Stmt.Fixed.Pattern.Assignment
      (lval, (_ : UnsizedType.t), (rhs : Expr.Typed.t)) -> (
      let assign_name = Stmt.Helpers.lhs_variable lval in
      let all_rhs_eigen_names = query_var_eigen_names rhs in
      if Set.mem aos_exits assign_name then
        Set.add all_rhs_eigen_names assign_name
      else
        match is_nonzero_subset ~set:aos_exits ~subset:all_rhs_eigen_names with
        | true -> Set.add all_rhs_eigen_names assign_name
        | false -> Set.Poly.empty)
  | Decl {decl_id; initialize= Assign e; _} -> (
      let all_rhs_eigen_names = query_var_eigen_names e in
      if Set.mem aos_exits decl_id then Set.add all_rhs_eigen_names decl_id
      else
        match is_nonzero_subset ~set:aos_exits ~subset:all_rhs_eigen_names with
        | true -> Set.add all_rhs_eigen_names decl_id
        | false -> Set.Poly.empty)
  (* All other statements do not need logic here*)
  | _ -> Set.Poly.empty

(**
  Modify a function and it's subexpressions from SoA <-> AoS and vice versa.
  This performs demotion for sub expressions recursively. The top level
   expression and it's sub expressions are demoted to SoA if
   1. The names of the variables in the subexpressions returning
    objects holding matrices are all in the modifiable set.
   2. The function does not support SoA
   3. The [force] argument is [true]
  @param force_demotion If true, forces an expression and it's sub-expressions
   to be AoS.
  @param modifiable_set The set of names that are either demotable
   to AoS or promotable to SoA.
  @param kind A [Fun_kind.t]
  @param exprs A list of expressions going into the function.
 **)
let rec modify_kind ?force_demotion:(force = false)
    (modifiable_set : string Set.Poly.t) (kind : 'a Fun_kind.t)
    (exprs : Expr.Typed.t list) =
  let expr_names =
    Set.Poly.union_list (List.map ~f:query_var_eigen_names exprs) in
  let is_all_in_list =
    is_nonzero_subset ~set:modifiable_set ~subset:expr_names in
  match kind with
  | Fun_kind.StanLib (name, sfx, (_ : Mem_pattern.t)) ->
      if is_all_in_list || (not (is_fun_soa_supported name exprs)) || force then
        (*Force demotion of all subexprs*)
        let exprs' =
          List.map ~f:(modify_expr ~force_demotion:true expr_names) exprs in
        (Fun_kind.StanLib (name, sfx, Mem_pattern.AoS), exprs')
      else
        ( Fun_kind.StanLib (name, sfx, SoA)
        , List.map ~f:(modify_expr ~force_demotion:false modifiable_set) exprs
        )
  | UserDefined _ as udf ->
      (udf, List.map ~f:(modify_expr ~force_demotion:force modifiable_set) exprs)
  | (_ : 'a Fun_kind.t) ->
      ( kind
      , List.map ~f:(modify_expr ~force_demotion:force modifiable_set) exprs )

(**
  Modify an expression and it's subexpressions from SoA <-> AoS
   and vice versa. The only real paths in the below is on the
   functions and ternary expressions.
 *
  The logic for functions is defined in [modify_kind].
  [TernaryIf] is forcefully demoted to AoS if the type of the expression
   contains a matrix.
  @param force_demotion If true, forces an expression and it's sub-expressions
   to be AoS.
  @param modifiable_set The name of the variables whose
   associated expressions we want to modify.
  @param pattern The expression to modify.
 *)
and modify_expr_pattern ?force_demotion:(force = false)
    (modifiable_set : string Set.Poly.t)
    (pattern : Expr.Typed.t Expr.Fixed.Pattern.t) =
  let mod_expr ?force_demotion:(forced = false) =
    modify_expr ~force_demotion:forced modifiable_set in
  match pattern with
  | Expr.Fixed.Pattern.FunApp (kind, (exprs : Expr.Typed.t list)) ->
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
  | TupleProjection (idx_expr, idx) -> TupleProjection (mod_expr idx_expr, idx)
  | EAnd (lhs, rhs) -> EAnd (mod_expr lhs, mod_expr rhs)
  | EOr (lhs, rhs) -> EOr (mod_expr lhs, mod_expr rhs)
  | Promotion (expr, type_, ad_level) ->
      Promotion (mod_expr expr, type_, ad_level)
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      pattern

(**
  Given a Set of strings containing the names of objects that can be
  modified from AoS <-> SoA and vice versa, modify them within the expression.
  @param mem_pattern The memory pattern to change expressions to.
  @param modifiable_set The name of the variables whose
   associated expressions we want to modify.
  @param expr the expression to modify.
*)
and modify_expr ?force_demotion:(force = false)
    (modifiable_set : string Set.Poly.t) (Expr.Fixed.{pattern; _} as expr) =
  { expr with
    pattern= modify_expr_pattern ~force_demotion:force modifiable_set pattern }

(**
  Modify statement patterns in the MIR from AoS <-> SoA and vice versa
  For [Decl] and [Assignment]'s reading in parameters, we demote to AoS
   if the [decl_id] (or assign name) is in the modifiable set and
  otherwise promote the statement to [SoA].
  For general [Assignment] statements, we check if the assignee is in
  the demotable set. If so, we force demotion of all of the rhs expressions.
  All other statements recurse over their statements and expressions.
*
  @param pattern The statement pattern to modify
  @param modifiable_set The name of the variable we are searching for.
*)
let rec modify_stmt_pattern
    (pattern : (Expr.Typed.t, Stmt.Located.t) Stmt.Fixed.Pattern.t)
    (modifiable_set : string Core.Set.Poly.t) =
  let mod_expr force = modify_expr ~force_demotion:force modifiable_set in
  let mod_stmt stmt = modify_stmt stmt modifiable_set in
  match pattern with
  | Stmt.Fixed.Pattern.Decl
      { decl_id
      ; decl_adtype
      ; decl_type= Type.Sized sized_type
      ; initialize=
          Assign
            ({ pattern= FunApp (CompilerInternal (FnReadParam read_param), args)
             ; _ } as assigner) } ->
      let name = decl_id in
      if Set.mem modifiable_set name then
        Stmt.Fixed.Pattern.Decl
          { decl_id
          ; decl_adtype
          ; decl_type=
              Type.Sized (SizedType.modify_sizedtype_mem AoS sized_type)
          ; initialize=
              Assign
                { assigner with
                  pattern=
                    FunApp
                      ( CompilerInternal
                          (FnReadParam {read_param with mem_pattern= AoS})
                      , List.map ~f:(mod_expr true) args ) } }
      else
        Stmt.Fixed.Pattern.Decl
          { decl_id
          ; decl_adtype
          ; decl_type=
              Type.Sized (SizedType.modify_sizedtype_mem SoA sized_type)
          ; initialize=
              Assign
                { assigner with
                  pattern=
                    FunApp
                      ( CompilerInternal
                          (FnReadParam {read_param with mem_pattern= SoA})
                      , List.map ~f:(mod_expr false) args ) } }
  | Stmt.Fixed.Pattern.Decl
      ({decl_id; decl_type= Type.Sized sized_type; initialize; _} as decl) ->
      if Set.mem modifiable_set decl_id then
        let init_expr =
          match initialize with
          | Stmt.Fixed.Pattern.Assign e ->
              Stmt.Fixed.Pattern.Assign (mod_expr false e)
          | Default -> Default
          | Uninit -> Uninit in
        Stmt.Fixed.Pattern.Decl
          { decl with
            decl_type=
              Type.Sized (SizedType.modify_sizedtype_mem AoS sized_type)
          ; initialize= init_expr }
      else
        Decl
          { decl with
            decl_type=
              Type.Sized (SizedType.modify_sizedtype_mem SoA sized_type) }
  | NRFunApp (kind, (exprs : Expr.Typed.t list)) ->
      let kind', exprs' = modify_kind modifiable_set kind exprs in
      NRFunApp (kind', exprs')
  | Assignment
      ( lval
      , ut
      , ({pattern= FunApp (CompilerInternal (FnReadParam read_param), args); _}
         as assigner) ) ->
      let name = Stmt.Helpers.lhs_variable lval in
      if Set.mem modifiable_set name then
        Assignment
          ( lval
          , ut
          , { assigner with
              pattern=
                FunApp
                  ( CompilerInternal
                      (FnReadParam {read_param with mem_pattern= AoS})
                  , List.map ~f:(mod_expr true) args ) } )
      else
        Assignment
          ( lval
          , ut
          , { assigner with
              pattern=
                FunApp
                  ( CompilerInternal
                      (FnReadParam {read_param with mem_pattern= SoA})
                  , List.map ~f:(mod_expr false) args ) } )
  | Assignment (lval, (ut : UnsizedType.t), rhs) ->
      let name = Stmt.Helpers.lhs_variable lval in
      if Set.mem modifiable_set name then
        (*If assignee is in bad set, force demotion of rhs functions*)
        Assignment (lval, ut, mod_expr true rhs)
      else Assignment (lval, ut, (mod_expr false) rhs)
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
  | JacobianPE expr -> JacobianPE ((mod_expr false) expr)
  | Return optional_expr ->
      Return (Option.map ~f:(mod_expr false) optional_expr)
  | Profile ((p_name : string), stmt) ->
      Profile (p_name, List.map ~f:mod_stmt stmt)
  | While (predicate, body) -> While ((mod_expr false) predicate, mod_stmt body)
  | Skip | Break | Continue | Decl _ -> pattern

(**
  Modify statement patterns in the MIR from AoS <-> SoA and vice versa
  @param mem_pattern A mem_pattern to modify expressions to. For the
   given memory pattern, this modifies
   statement patterns and expressions to it.
  @param stmt The statement to modify.
  @param modifiable_set The name of the variable we are searching for.
*)
and modify_stmt (Stmt.Fixed.{pattern; _} as stmt)
    (modifiable_set : string Set.Poly.t) =
  {stmt with pattern= modify_stmt_pattern pattern modifiable_set}

let collect_mem_pattern_variables stmts =
  let take_stmt acc = function
    | Stmt.Fixed.{pattern= Decl {decl_id; decl_type= Type.Sized stype; _}; _}
      when SizedType.has_mem_pattern stype ->
        (decl_id, stype) :: acc
    | _ -> acc in
  Mir_utils.fold_stmts ~take_expr:(fun acc _ -> acc) ~take_stmt ~init:[] stmts
  |> List.rev

let pp_mem_patterns ppf (Program.{reverse_mode_log_prob; _} : Program.Typed.t) =
  let pp_var ppf (name, stype) =
    Fmt.pf ppf "%a %s: %a"
      (SizedType.pp Expr.Fixed.pp)
      stype name Middle.Mem_pattern.pp
      (SizedType.get_mem_pattern stype) in
  let mem_vars =
    (* Collect all the sizedtypes which have a mem pattern *)
    collect_mem_pattern_variables reverse_mode_log_prob in
  Fmt.(pf ppf "@[<v>%a@.@]" (list pp_var)) mem_vars
