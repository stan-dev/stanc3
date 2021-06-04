open Core_kernel
open Middle
open Middle.Expr

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
      Set.Poly.inter (op expr_top) (op expr_bottom)
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
 * Search through an expression for `Var name` where `name = var_name`
 * @param var_name A string with the name of the obj
 *  we are searching for.
 * @param pattern An expression pattern we recursivly search through 
 *  for the name.
let rec query_mem_pattern_names (var_name : string) Expr.Fixed.({pattern; _}) =
  let query_name = query_mem_pattern_names var_name in
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
 **)

(**
 * Search through an expression for `Var name` where `name = var_name`
 * @param var_name A Set of strings with the name of the obj
 *  we are searching for.
 * @param pattern An expression pattern we recursivly search through 
 *  for the name.
 **)
let rec query_mem_pattern_set_names (var_name : string Set.Poly.t)
    Expr.Fixed.({pattern; _}) =
  let query_name = query_mem_pattern_set_names var_name in
  match pattern with
  | FunApp (_, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      List.exists ~f:query_name exprs
  | TernaryIf (predicate, texpr, fexpr) ->
      query_name predicate || query_name texpr || query_name fexpr
  | Indexed (expr, indexed) ->
      let find_name_in_index = name_exists_in_index query_name in
      query_name expr || List.exists ~f:find_name_in_index indexed
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
 *
 * @param var_name The name of the variable whose
 *  associated expressions we want to modify.
 * @param expr The expression to modify
 *)
let rec modify_soa_exprs (var_name : string Set.Poly.t)
    (Expr.Fixed.({pattern; _}) as expr) =
  let mod_expr = modify_soa_exprs var_name in
  let find_name = query_mem_pattern_set_names var_name in
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
 *  in statment patterns that have hold statements
 *)
let modify_soa_stmts (var_name : string Set.Poly.t) pattern_fn
    (Stmt.Fixed.({pattern; _}) as stmt) =
  {stmt with pattern= pattern_fn pattern var_name}

(*Given a sizedtype, promote it's mem pattern from AoS to SoA*)
let rec promote_sizedtype_mem st =
  match st with
  | (SizedType.SInt | SReal) as ret -> ret
  | SVector (AoS, dim) -> SVector (SoA, dim)
  | SRowVector (AoS, dim) -> SRowVector (SoA, dim)
  | SMatrix (AoS, dim1, dim2) -> SMatrix (SoA, dim1, dim2)
  | SArray (inner_type, dim) -> SArray (promote_sizedtype_mem inner_type, dim)
  | _ -> st

(**
 * Promote statements in the MIR so that they go from Array of Structs (AoS)
 * to Struct of Arrays (SoA) .
 * @param pattern The statement to modify 
 * @param var_name The name of the variable we are searching for.
 *)
let rec mod_soa_stmt_pattern
    (pattern : ('a Fixed.t, ('a, 'b) Stmt.Fixed.t) Stmt.Fixed.Pattern.t)
    (var_name : string Core_kernel.Set.Poly.t) =
  let mod_expr = modify_soa_exprs var_name in
  let mod_stmt = modify_soa_stmts var_name mod_soa_stmt_pattern in
  let find_name = query_mem_pattern_set_names var_name in
  match pattern with
  | Decl {decl_adtype; decl_id; decl_type= Type.Sized sized_type}
    when Set.Poly.exists ~f:(fun x -> x = decl_id) var_name ->
      Decl
        { decl_adtype
        ; decl_id
        ; decl_type= Type.Sized (promote_sizedtype_mem sized_type) }
  | Decl _ -> pattern
  | NRFunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
      let modify_funs kind =
        match kind with
        | Fun_kind.StanLib (name, sfx, _) as func -> (
          match List.exists ~f:find_name exprs with
          | true -> Fun_kind.StanLib (name, sfx, Common.Helpers.SoA)
          | false -> func )
        | _ -> kind
      in
      NRFunApp (modify_funs kind, List.map ~f:mod_expr exprs)
  | Assignment
      ( ((name : string), (ut : UnsizedType.t), lhs)
      , { pattern=
            FunApp (CompilerInternal (FnReadParam (constrain_op, SoA)), args)
        ; meta= emeta } ) ->
      if Set.Poly.exists ~f:(fun x -> x = name) var_name then
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

let query_mem_pattern_funkinds query_expr success_set kind exprs =
  let query_args = query_expr success_set in
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
    match name with
    | x when Stan_math_signatures.is_reduce_sum_fn x -> Set.Poly.empty
    | x when Stan_math_signatures.is_variadic_ode_fn x -> Set.Poly.empty
    | _ ->
        let is_fun_support =
          Stan_math_signatures.query_stan_math_mem_pattern_support name
            (List.map ~f:find_args exprs)
        in
        if is_fun_support then
          Set.Poly.union_list (List.map ~f:query_args exprs)
        else Set.Poly.empty )
  | CompilerInternal _ -> Set.Poly.empty
  | UserDefined _ -> Set.Poly.empty

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
      query_mem_pattern_funkinds query_mem_pattern_exprs success_set kind exprs
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      Set.Poly.empty
  | TernaryIf (predicate, texpr, fexpr) ->
      Set.Poly.inter
        (Set.Poly.inter (query_expr predicate) (query_expr texpr))
        (query_expr fexpr)
  | Indexed (expr, indexed) ->
      let query_index = search_index query_expr in
      Set.Poly.inter (query_expr expr)
        (Set.Poly.union_list (List.map ~f:query_index indexed))
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      Set.Poly.inter (query_expr lhs) (query_expr rhs)

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
let query_mem_pattern_stmt (success_set : string Set.Poly.t)
    (_ : (int, Stmt.Located.Non_recursive.t) Core_kernel.Map.Poly.t) pattern =
  let query_expr = query_mem_pattern_exprs success_set in
  match pattern with
  | Stmt.Fixed.Pattern.Decl _ -> Set.Poly.empty
  | Assignment (((assign_name : string), (_ : UnsizedType.t), lhs), rhs) ->
      (*let query_index = search_index query_expr in*)
      (*let check_name item = assign_name = item in*)
      (*NOTE: This logic is 100% wrong*)
      let is_bad_assign = Set.Poly.length (query_expr rhs) = 0 in
      let is_single_indexed = List.exists ~f:index_constains_single lhs in
      if is_bad_assign || not is_single_indexed then Set.Poly.empty
      else Set.Poly.singleton assign_name
  | NRFunApp (kind, exprs) ->
      query_mem_pattern_funkinds query_mem_pattern_exprs success_set kind exprs
  (*For the below, we just want to look at expressions*)
  | IfElse (predicate, (_ : int), (_ : int option)) -> query_expr predicate
  | Return optional_expr -> (
    match optional_expr with
    | Some expr -> query_expr expr
    | None -> Set.Poly.empty )
  | TargetPE expr -> query_expr expr
  (*Need handling for body in for and while*)
  | For {lower; upper; _} ->
      Set.Poly.inter (query_expr lower) (query_expr upper)
      (* query_stmt (get_key sub_stmts)*)
  | While (predicate, _) -> query_expr predicate
  (*Sub stmts of statements handled by Monotone Framework*)
  | Skip | Break | Continue | SList _ | Block _ | Profile _ -> Set.Poly.empty
