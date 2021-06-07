open Core_kernel
open Middle
open Middle.Expr

(**
 * Internal to the module, used when looking recursivly
 *  in statement patterns that hold statements
 *)
let get_eigen_decls Stmt.Fixed.({pattern; _}) =
  match pattern with
  | Stmt.Fixed.Pattern.Decl
      { decl_adtype= UnsizedType.AutoDiffable
      ; decl_id
      ; decl_type= Type.Sized sized_type }
    when SizedType.contains_eigen_type sized_type ->
      Set.Poly.singleton decl_id
  | _ -> Set.Poly.empty

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
  | Index.All -> Set.Poly.empty
  | Single ind_expr -> op ind_expr
  | Upfrom ind_expr -> op ind_expr
  | Between (expr_top, expr_bottom) ->
      Set.Poly.union (op expr_top) (op expr_bottom)
  | MultiIndex exprs -> op exprs

let name_exists_in_index op ind =
  match ind with
  | Index.All -> false
  | Single ind_expr -> op ind_expr
  | Upfrom ind_expr -> op ind_expr
  | Between (expr_top, expr_bottom) -> op expr_top || op expr_bottom
  | MultiIndex exprs -> op exprs

let rec find_single_index_exprs Expr.Fixed.({pattern; _}) =
  let query_expr = find_single_index_exprs in
  match pattern with
  | FunApp (_, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      List.exists ~f:query_expr exprs
  | TernaryIf (predicate, texpr, fexpr) ->
      query_expr predicate || query_expr texpr || query_expr fexpr
  | Indexed (expr, indexed) ->
      let index_constains_single ind =
        match ind with
        | Index.All -> false
        | Single _ -> true
        | Upfrom ind_expr -> find_single_index_exprs ind_expr
        | Between (expr_top, expr_bottom) ->
            find_single_index_exprs expr_top
            || find_single_index_exprs expr_bottom
        | MultiIndex exprs -> find_single_index_exprs exprs
      in
      query_expr expr || List.exists ~f:index_constains_single indexed
  | Var (_ : string) -> false
  | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) -> false
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_expr lhs || query_expr rhs

let index_constains_single ind =
  match ind with
  | Index.All -> false
  | Single _ -> true
  | Upfrom ind_expr -> find_single_index_exprs ind_expr
  | Between (expr_top, expr_bottom) ->
      find_single_index_exprs expr_top || find_single_index_exprs expr_bottom
  | MultiIndex exprs -> find_single_index_exprs exprs

(**
 * Search through an expression for `Var name` where `name = soa_exits`
 * @param soa_exits A Set of strings with the name of the obj
 *  we are searching for.
 * @param pattern An expression pattern we recursivly search through 
 *  for the name.
 **)
let rec query_mem_pattern_set_names (soa_exits : string Set.Poly.t)
    Expr.Fixed.({pattern; _}) =
  let query_name = query_mem_pattern_set_names soa_exits in
  match pattern with
  | FunApp (_, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      List.exists ~f:query_name exprs
  | TernaryIf (predicate, texpr, fexpr) ->
      query_name predicate || query_name texpr || query_name fexpr
  | Indexed (expr, indexed) ->
      let find_name_in_index = name_exists_in_index query_name in
      query_name expr || List.exists ~f:find_name_in_index indexed
  | Var (name : string) -> Set.Poly.exists ~f:(fun x -> x = name) soa_exits
  | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) -> false
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_name lhs || query_name rhs

let rec find_mem_pattern_set_names Expr.Fixed.({pattern; meta=Expr.Typed.Meta.({type_;_})}) =
  let find_name = find_mem_pattern_set_names in
  match pattern with
  | FunApp (_, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      Set.Poly.union_list (List.map ~f:find_name exprs)
  | TernaryIf (predicate, texpr, fexpr) ->
      Set.Poly.union_list
        [find_name predicate; find_name texpr; find_name fexpr]
  | Indexed (expr, indexed) ->
      let find_in_index = search_index find_name in
      Set.Poly.union (find_name expr)
        (Set.Poly.union_list (List.map ~f:find_in_index indexed))
  | Var (name : string) when UnsizedType.contains_eigen_type type_ -> Set.Poly.singleton name
  | Var (_ : string) -> Set.Poly.empty
  | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) -> Set.Poly.empty
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      Set.Poly.union (find_name lhs) (find_name rhs)

(** 
 * Modify functions expressions from SoA to AoS
 * TODO: Docs
 * The main issue with this right now is that if we see the failed soa_exits
 * inside of any StanLib we flip the whole StanLib to AoS, but we only need to 
 * do that if every expression's objs are all AoS. If just one argument 
 * is still an SoA then we can actually keep the functions as SoA.
 *
 * The only real path in the below is on the functions, everything else is 
 * for recursion through expressions of expressions.
 *
 * @param soa_exits The name of the variable whose
 *  associated expressions we want to modify.
 * @param expr The expression to modify
 *)
let rec modify_soa_exprs (soa_exits : string Set.Poly.t)
    (Expr.Fixed.({pattern; _}) as expr) =
  let mod_expr = modify_soa_exprs soa_exits in
  let find_name = query_mem_pattern_set_names soa_exits in
  let new_pattern =
    match pattern with
    | FunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
        let exprs' = List.map ~f:mod_expr exprs in
        let modify_funs kind =
          match kind with
          | Fun_kind.StanLib (name, sfx, _) as func -> (
            match List.exists ~f:find_name exprs with
            | true -> Fun_kind.StanLib (name, sfx, Common.Helpers.AoS)
            | false -> func )
          | _ -> kind
        in
        Expr.Fixed.Pattern.FunApp (modify_funs kind, exprs')
    | TernaryIf (predicate, texpr, fexpr) ->
        TernaryIf (mod_expr predicate, mod_expr texpr, mod_expr fexpr)
    | Indexed (idx_expr, indexed) ->
        let query_index = mod_index mod_expr in
        Indexed (mod_expr idx_expr, List.map ~f:query_index indexed)
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string))
      ->
        pattern
    | EAnd (lhs, rhs) -> EAnd (mod_expr lhs, mod_expr rhs)
    | EOr (lhs, rhs) -> EOr (mod_expr lhs, mod_expr rhs)
  in
  {expr with pattern= new_pattern}

(**
 * Internal to the module, used when looking recursivly
 *  in statement patterns that hold statements
 *)
let modify_soa_stmts (soa_exits : string Set.Poly.t) pattern_fn
    (Stmt.Fixed.({pattern; _}) as stmt) =
  {stmt with pattern= pattern_fn pattern soa_exits}

(*Given a sizedtype, promote it's mem pattern from AoS to SoA*)
let rec promote_sizedtype_mem st =
  match st with
  | (SizedType.SInt | SReal) as ret -> ret
  | SVector (AoS, dim) -> SVector (SoA, dim)
  | SRowVector (AoS, dim) -> SRowVector (SoA, dim)
  | SMatrix (AoS, dim1, dim2) -> SMatrix (SoA, dim1, dim2)
  | SArray (inner_type, dim) -> SArray (promote_sizedtype_mem inner_type, dim)
  | _ -> st

(*Given a sizedtype, promote it's mem pattern from AoS to SoA*)
let rec demote_sizedtype_mem st =
  match st with
  | (SizedType.SInt | SReal) as ret -> ret
  | SVector (SoA, dim) -> SVector (AoS, dim)
  | SRowVector (SoA, dim) -> SRowVector (AoS, dim)
  | SMatrix (SoA, dim1, dim2) -> SMatrix (AoS, dim1, dim2)
  | SArray (inner_type, dim) -> SArray (demote_sizedtype_mem inner_type, dim)
  | _ -> st

(**
 * Promote statements in the MIR so that they go from Array of Structs (AoS)
 * to Struct of Arrays (SoA) .
 * @param pattern The statement to modify 
 * @param soa_exits The name of the variable we are searching for.
 *)
let rec modify_soa_stmt_pattern
    (pattern : ('a Fixed.t, ('a, 'b) Stmt.Fixed.t) Stmt.Fixed.Pattern.t)
    (soa_exits : string Core_kernel.Set.Poly.t) =
  let mod_expr = modify_soa_exprs soa_exits in
  let mod_stmt = modify_soa_stmts soa_exits modify_soa_stmt_pattern in
  let find_name = query_mem_pattern_set_names soa_exits in
  match pattern with
  | Decl {decl_adtype; decl_id; decl_type= Type.Sized sized_type}
    when Set.Poly.exists ~f:(fun x -> x = decl_id) soa_exits ->
      Decl
        { decl_adtype
        ; decl_id
        ; decl_type= Type.Sized (demote_sizedtype_mem sized_type) }
  | Decl _ -> pattern
  | NRFunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
      let modify_funs kind =
        match kind with
        | Fun_kind.StanLib (name, sfx, _) as func -> (
          match List.exists ~f:find_name exprs with
          | true -> Fun_kind.StanLib (name, sfx, Common.Helpers.AoS)
          | false -> func )
        | _ -> kind
      in
      NRFunApp (modify_funs kind, List.map ~f:mod_expr exprs)
  | Assignment
      ( ((name : string), (ut : UnsizedType.t), lhs)
      , { pattern=
            FunApp (CompilerInternal (FnReadParam (constrain_op, _)), args)
        ; meta= emeta } ) ->
      if Set.Poly.exists ~f:(fun x -> x = name) soa_exits then
        let query_index = mod_index mod_expr in
        Assignment
          ( (name, ut, List.map ~f:query_index lhs)
          , { pattern=
                FunApp
                  ( CompilerInternal (FnReadParam (constrain_op, AoS))
                  , List.map ~f:mod_expr args )
            ; meta= emeta } )
      else
        Assignment
          ( (name, ut, lhs)
          , { pattern=
                FunApp
                  (CompilerInternal (FnReadParam (constrain_op, SoA)), args)
            ; meta= emeta } )
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
  | Skip | Break | Continue -> pattern
  | While (predicate, body) -> While (mod_expr predicate, mod_stmt body)

(* Look through an expression and find the overall type and adlevel*)
let find_args Expr.Fixed.({meta= Expr.Typed.Meta.({type_; adlevel; _}); _}) =
  (adlevel, type_)
  

let query_mem_pattern_funkinds query_expr kind exprs =
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
    match name with
    | x when Stan_math_signatures.is_reduce_sum_fn x ->
        Set.Poly.union_list (List.map ~f:find_mem_pattern_set_names exprs)
    | x when Stan_math_signatures.is_variadic_ode_fn x ->
        Set.Poly.union_list (List.map ~f:find_mem_pattern_set_names exprs)
    | _ ->
        let is_fun_support =
          Stan_math_signatures.query_stan_math_mem_pattern_support name
            (List.map ~f:find_args exprs)
        in
        if is_fun_support then
          Set.Poly.union_list (List.map ~f:query_expr exprs)
        else Set.Poly.union_list (List.map ~f:find_mem_pattern_set_names exprs)
    )
  | CompilerInternal _ -> Set.Poly.empty
  | UserDefined _ ->
      Set.Poly.union_list (List.map ~f:find_mem_pattern_set_names exprs)

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
let rec query_mem_pattern_exprs (success_set : string Set.Poly.t)
    Expr.Fixed.({pattern; _}) =
  let query_expr = query_mem_pattern_exprs success_set in
  match pattern with
  | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      query_mem_pattern_funkinds
        (query_mem_pattern_exprs success_set)
        kind exprs
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      Set.Poly.empty
  | TernaryIf (predicate, texpr, fexpr) ->
      Set.Poly.union
        (Set.Poly.union (query_expr predicate) (query_expr texpr))
        (query_expr fexpr)
  | Indexed (expr, indexed) ->
      let query_index = search_index query_expr in
      Set.Poly.union (query_expr expr)
        (Set.Poly.union_list (List.map ~f:query_index indexed))
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      Set.Poly.union (query_expr lhs) (query_expr rhs)

(* Look through a statement to see whether it needs modified from
 * SoA to AoS. Returns true if object needs changed from SoA to AoS.
 *
 * Rules:
 * 1. Decl is data only or contains and int or real type, return true
 * 2. StanLib functions return true if any of
 *   - The name exists in the exprs of the function
 *   - The function does not support SoA
 *   - The exprs satisfies the query_mem_pattern_exprs checks
 * 3. (TODO: UserDefined)
 *  - I'm not sure how to do these yet
 *  - For these we need to lookup the body of the user defined function,
 *   check all these rules for the UDF, and if it passes tag it as SoA,
 *   and if not tag it as AoS. But then we run into this problem where
 *  we need to have multiple function definitions when some input types can
 *  or cannot be AoS or SoA.
 *  For now I think it's better to just check if the variable name
 *  is in the list of expressions to the user defined function and fail
 *  the variable if we see it enter a UDF.
 * 4. Assignment return true if
 *     - The object being assigned to is in the list of known failures and
 *    - any of
 *     - The expression query from the indexed expression returns true 
 *     - The expression query from the rhs of the assignment returns true
 *   -  I think I also need to return true if the assigned to object is
 *      in the list of known failures the variable exists 
 *      on the right hand side or in the indexed type?
 *
 * 4. (TODO: For loops)
 *  Single indexing is fine outside of loops, but in loops we need to return true
 *
 * All other statements simply recurse into their lists of expressions 
 *  or statements repeating the checks from above.
 *)
let query_mem_pattern_stmt (success_set : string Set.Poly.t) pattern =
  let query_expr = query_mem_pattern_exprs success_set in
  match pattern with
  | Stmt.Fixed.Pattern.Decl _ -> Set.Poly.empty
  | Assignment (((assign_name : string), (_ : UnsizedType.t), lhs), rhs) ->
      (*let query_index = search_index query_expr in*)
      (*let check_name item = assign_name = item in*)
      (*NOTE: This logic is 100% wrong*)
      let check_lhs =
        Set.Poly.union_list (List.map ~f:(search_index query_expr) lhs)
      in
      let check_rhs = query_expr rhs in
      let is_aos_rhs = (Set.Poly.length (check_rhs) > 0) in
      let is_aos_assigned = (match is_aos_rhs with
      | true -> Set.Poly.singleton assign_name 
      | false -> Set.Poly.empty) in
      let is_aos_assignee = (match (Set.Poly.mem success_set assign_name) with 
      | true -> find_mem_pattern_set_names rhs
      | false -> check_rhs) in
        Set.Poly.union_list [is_aos_assigned; check_lhs; is_aos_assignee]
  | NRFunApp (kind, exprs) ->
      query_mem_pattern_funkinds
        (query_mem_pattern_exprs success_set)
        kind exprs
  (*For the below, we just want to look at expressions*)
  | IfElse (predicate, (_ : int), (_ : int option)) -> query_expr predicate
  | Return optional_expr -> (
    match optional_expr with
    | Some expr -> query_expr expr
    | None -> Set.Poly.empty )
  | TargetPE expr -> query_expr expr
  (*Need handling for body in for and while*)
  | For {lower; upper; _} ->
      Set.Poly.union (query_expr lower) (query_expr upper)
      (* query_stmt (get_key sub_stmts)*)
  | While (predicate, _) -> query_expr predicate
  (*Sub stmts of statements handled by Monotone Framework*)
  | Skip | Break | Continue | SList _ | Block _ | Profile _ -> Set.Poly.empty

(** 
 * This is just a copy of the above with forced promotion
 *)
let rec promote_soa_exprs eigen_types (Expr.Fixed.({pattern; _}) as expr) =
  let mod_expr = promote_soa_exprs eigen_types in
  let new_pattern =
    match pattern with
    | FunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
        let expr_names =
          Set.Poly.union_list (List.map ~f:find_mem_pattern_set_names exprs)
        in
        let name_diff =
          Set.Poly.length (Set.Poly.inter eigen_types expr_names)
        in
        let exprs' = List.map ~f:mod_expr exprs in
        let modify_funs kind =
          match kind with
          | Fun_kind.StanLib (name, sfx, _) when name_diff > 0 ->
              Fun_kind.StanLib (name, sfx, Common.Helpers.SoA)
          | _ -> kind
        in
        Expr.Fixed.Pattern.FunApp (modify_funs kind, exprs')
    | TernaryIf (predicate, texpr, fexpr) ->
        TernaryIf (mod_expr predicate, mod_expr texpr, mod_expr fexpr)
    | Indexed (idx_expr, indexed) ->
        let query_index = mod_index mod_expr in
        Indexed (mod_expr idx_expr, List.map ~f:query_index indexed)
    | EAnd (lhs, rhs) -> EAnd (mod_expr lhs, mod_expr rhs)
    | EOr (lhs, rhs) -> EOr (mod_expr lhs, mod_expr rhs)
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string))
      ->
        pattern
  in
  {expr with pattern= new_pattern}

(**
 * Internal to the module, used when looking recursivly
 *  in statement patterns that hold statements
 *)
let promote_soa_stmts eigen_types pattern_fn (Stmt.Fixed.({pattern; _}) as stmt)
    =
  {stmt with pattern= (pattern_fn eigen_types) pattern}

(**
* Forced promotion
*)
let rec promote_soa_stmt_pattern eigen_types
    (pattern : ('a Fixed.t, ('a, 'b) Stmt.Fixed.t) Stmt.Fixed.Pattern.t) =
  let mod_expr = promote_soa_exprs eigen_types in
  let mod_stmt = promote_soa_stmts eigen_types promote_soa_stmt_pattern in
  match pattern with
  | Decl {decl_adtype; decl_id; decl_type= Type.Sized sized_type}
    when SizedType.contains_eigen_type sized_type ->
      Decl
        { decl_adtype
        ; decl_id
        ; decl_type= Type.Sized (promote_sizedtype_mem sized_type) }
  | Decl _ -> pattern
  | NRFunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
      let modify_funs kind =
        match kind with
        | Fun_kind.StanLib (name, sfx, _) ->
            Fun_kind.StanLib (name, sfx, Common.Helpers.SoA)
        | _ -> kind
      in
      NRFunApp (modify_funs kind, List.map ~f:mod_expr exprs)
  | Assignment
      ( ((name : string), (ut : UnsizedType.t), lhs)
      , { pattern=
            FunApp (CompilerInternal (FnReadParam (constrain_op, _)), args)
        ; meta= emeta } )
    when Set.Poly.mem eigen_types name ->
      let query_index = mod_index mod_expr in
      Assignment
        ( (name, ut, List.map ~f:query_index lhs)
        , { pattern=
              FunApp
                ( CompilerInternal (FnReadParam (constrain_op, SoA))
                , List.map ~f:mod_expr args )
          ; meta= emeta } )
  | Assignment (((name : string), (ut : UnsizedType.t), lhs), rhs) ->
      let mod_index = mod_index mod_expr in
      Assignment ((name, ut, List.map ~f:mod_index lhs), mod_expr rhs)
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
  | Skip | Break | Continue -> pattern
  | While (predicate, body) -> While (mod_expr predicate, mod_stmt body)

let query_mem_pattern_funkinds_loop query_expr_loop in_loop kind exprs =
  let query_expr_loop2 = query_expr_loop in_loop in
  (*let printer intro s = Set.Poly.iter ~f:(printf intro) s in*)
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
    match name with
    | x
      when Stan_math_signatures.is_reduce_sum_fn x
           || Stan_math_signatures.is_variadic_ode_fn x ->
        Set.Poly.union_list (List.map ~f:find_mem_pattern_set_names exprs)
    | _ ->
        let is_fun_support =
          Stan_math_signatures.query_stan_math_mem_pattern_support name
            (List.map ~f:find_args exprs)
        in
        (*
        if is_fun_support then 
        printer "\n%s is supported\n" (Set.Poly.singleton name)
        else 
        printer "\n%s is not supported\n" (Set.Poly.singleton name);
*)
        (*Still needs to look for single indexing*)
        if is_fun_support then
          Set.Poly.union_list (List.map ~f:query_expr_loop2 exprs)
        else Set.Poly.union_list (List.map ~f:find_mem_pattern_set_names exprs)
    )
  | CompilerInternal _ -> Set.Poly.empty
  | UserDefined _ ->
      Set.Poly.union_list (List.map ~f:find_mem_pattern_set_names exprs)

let rec is_uni_loop_indexing ut index =
  match (ut, index) with
  | (UnsizedType.UArray t | UFun (_, ReturnType t, _, _)), index -> (
    match List.tl index with
    | Some cut_list -> is_uni_loop_indexing t cut_list
    | None -> is_uni_loop_indexing t [Index.All] )
  | (UVector | URowVector), [Index.Single _] -> true
  | UMatrix, [Single _; Single _] -> true
  | (UReal | UInt | UMathLibraryFunction | UFun (_, Void, _, _)), _ -> false
  | (UVector | URowVector), _ -> false
  | UMatrix, _ -> false

let rec query_mem_pattern_exprs_loop in_loop Expr.Fixed.({pattern; _}) =
  let query_expr = query_mem_pattern_exprs_loop in_loop in
  match pattern with
  | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      query_mem_pattern_funkinds_loop query_mem_pattern_exprs_loop in_loop kind
        exprs
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      Set.Poly.empty
  | TernaryIf (predicate, texpr, fexpr) ->
      Set.Poly.union
        (Set.Poly.union (query_expr predicate) (query_expr texpr))
        (query_expr fexpr)
  | Indexed (Expr.Fixed.({pattern= Var name; meta= {type_; _}}), indexed) ->
      let query_index = search_index query_expr in
      if in_loop && is_uni_loop_indexing type_ indexed then
        Set.Poly.union (Set.Poly.singleton name)
          (Set.Poly.union_list (List.map ~f:query_index indexed))
      else Set.Poly.union_list (List.map ~f:query_index indexed)
  | Indexed (expr, indexed) ->
      let query_index = search_index query_expr in
      Set.Poly.union (query_expr expr)
        (Set.Poly.union_list (List.map ~f:query_index indexed))
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      Set.Poly.union (query_expr lhs) (query_expr rhs)

(**
 * Internal to the module, used when looking recursivly
 *  in statement patterns that hold statements
 *)
let query_soa_stmts_loop in_loop pattern_fn Stmt.Fixed.({pattern; _}) =
  pattern_fn in_loop pattern

let rec query_mem_pattern_stmt_loops in_loop pattern =
  let query_expr = query_mem_pattern_exprs_loop in_loop in
  let query_inner_stmt checking_loop =
    query_soa_stmts_loop checking_loop query_mem_pattern_stmt_loops
  in
  match pattern with
  | Stmt.Fixed.Pattern.Decl _ -> Set.Poly.empty
  | Assignment (((name : string), (_ : UnsizedType.t), lhs), rhs) ->
      let check_lhs =
        Set.Poly.union_list (List.map ~f:(search_index query_expr) lhs)
      in
      let check_rhs = query_expr rhs in
      if (Set.Poly.length check_rhs) > 0 then
      let sub_set = Set.Poly.union check_lhs check_rhs in 
      Set.Poly.add sub_set name
      else 
      Set.Poly.union check_lhs check_rhs
  | NRFunApp (kind, exprs) ->
      query_mem_pattern_funkinds_loop query_mem_pattern_exprs_loop in_loop kind
        exprs
  (*For the below, we just want to look at expressions*)
  | IfElse (predicate, lhs, rhs) -> (
    match rhs with
    | Some inner_rhs ->
        Set.Poly.union_list
          [ query_expr predicate
          ; query_inner_stmt in_loop lhs
          ; query_inner_stmt in_loop inner_rhs ]
    | None ->
        Set.Poly.union_list [query_expr predicate; query_inner_stmt in_loop lhs]
    )
  | Return optional_expr -> (
    match optional_expr with
    | Some expr -> query_expr expr
    | None -> Set.Poly.empty )
  | SList lst | Profile (_, lst) | Block lst ->
      Set.Poly.union_list (List.map ~f:(query_inner_stmt in_loop) lst)
  | TargetPE expr -> query_expr expr
  (*Need handling for body in for and while*)
  | For {lower; upper; body; _} ->
      Set.Poly.union
        (Set.Poly.union (query_expr lower) (query_expr upper))
        (query_inner_stmt true body)
      (* query_stmt (get_key sub_stmts)*)
  | While (predicate, body) ->
      Set.Poly.union (query_expr predicate) (query_inner_stmt true body)
  (*Sub stmts of statements handled by Monotone Framework*)
  | Skip | Break | Continue -> Set.Poly.empty
