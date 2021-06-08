open Core_kernel
open Middle
open Middle.Expr

(**
 * Recursivly look in Decls for sized types that hold matrices or vectors
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
let modify_index op ind =
  match ind with
  | Index.All -> Index.All
  | Single ind_expr -> Single (op ind_expr)
  | Upfrom ind_expr -> Upfrom (op ind_expr)
  | Between (expr_top, expr_bottom) -> Between (op expr_top, op expr_bottom)
  | MultiIndex exprs -> MultiIndex (op exprs)

(**
 * Apply an op returning a Set of strings for `Index` types
 *  with inner expressions
 * @param op a functor returning a Set of strings
 * @param ind the Index.t to modify
 *)
let map_index op ind =
  match ind with
  | Index.All -> Set.Poly.empty
  | Single ind_expr -> op ind_expr
  | Upfrom ind_expr -> op ind_expr
  | Between (expr_top, expr_bottom) ->
      Set.Poly.union (op expr_top) (op expr_bottom)
  | MultiIndex exprs -> op exprs

(**
 * Apply a functor to an `Index` type's inner returning a boolean if 
 * the conditional is true for any of the inner expressions.
 *)
let name_exists_in_index op ind =
  match ind with
  | Index.All -> false
  | Single ind_expr -> op ind_expr
  | Upfrom ind_expr -> op ind_expr
  | Between (expr_top, expr_bottom) -> op expr_top || op expr_bottom
  | MultiIndex exprs -> op exprs

(**
 * Search through an expression for `Var name` where `name` is in
 *  the `aos_exits`
 * and the type of the expression contains an eigen type.
 * @param aos_exits A Set of strings with the name of the obj
 *  we are searching for.
 * @param pattern An expression pattern we recursivly search through 
 *  for the name.
 **)
let rec query_expr_eigen_names (aos_exits : string Set.Poly.t)
    Expr.Fixed.({pattern; meta= Expr.Typed.Meta.({type_; _})}) =
  let query_name = query_expr_eigen_names aos_exits in
  match pattern with
  | FunApp ((_ : Fun_kind.t), (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      List.exists ~f:query_name exprs
  | TernaryIf (predicate, texpr, fexpr) ->
      query_name predicate || query_name texpr || query_name fexpr
  | Indexed (expr, indexed) ->
      query_name expr
      || List.exists ~f:(name_exists_in_index query_name) indexed
  | Var (name : string) when UnsizedType.contains_eigen_type type_ ->
      Set.Poly.mem aos_exits name
  | Var (_ : string) -> false
  | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) -> false
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_name lhs || query_name rhs

(**
 * Search through an expression for `Var name` where `name` is in
 *  the `aos_exits` and return `true` if it exists and false otherwise.
 * and the type of the expression contains an eigen type.
 * @param aos_exits A Set of strings with the name of the obj
 *  we are searching for.
 * @param pattern An expression pattern we recursivly search through 
 *  for the name.
 **)
let rec query_all_expr_eigen_names
    Expr.Fixed.({pattern; meta= Expr.Typed.Meta.({type_; _})}) =
  let find_name = query_all_expr_eigen_names in
  match pattern with
  | FunApp ((_ : Fun_kind.t), (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      Set.Poly.union_list (List.map ~f:find_name exprs)
  | TernaryIf (predicate, texpr, fexpr) ->
      Set.Poly.union_list
        [find_name predicate; find_name texpr; find_name fexpr]
  | Indexed (expr, indexed) ->
      let find_in_index = map_index find_name in
      Set.Poly.union (find_name expr)
        (Set.Poly.union_list (List.map ~f:find_in_index indexed))
  | Var (name : string) when UnsizedType.contains_eigen_type type_ ->
      Set.Poly.singleton name
  | Var (_ : string) -> Set.Poly.empty
  | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) -> Set.Poly.empty
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      Set.Poly.union (find_name lhs) (find_name rhs)

(** 
 * Modify functions expressions from SoA to AoS
 * The only real path in the below is on the functions.
 * If all of the eigen types in the function's expression list
 * We make the function AoS, else it stays as SoA.
 *
 * @param aos_exits The name of the variable whose
 *  associated expressions we want to modify.
 * @param expr The expression to modify
 *)
let rec demote_exprs (aos_exits : string Set.Poly.t)
    (Expr.Fixed.({pattern; _}) as expr) =
  let mod_expr = demote_exprs aos_exits in
  let find_name = query_expr_eigen_names aos_exits in
  let new_pattern =
    match pattern with
    | FunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
        let exprs' = List.map ~f:mod_expr exprs in
        let modify_funs kind =
          match kind with
          | Fun_kind.StanLib (name, sfx, _) as func -> (
            match List.for_all ~f:find_name exprs with
            | true -> Fun_kind.StanLib (name, sfx, Common.Helpers.AoS)
            | false -> func )
          | (_ : Fun_kind.t) -> kind
        in
        Expr.Fixed.Pattern.FunApp (modify_funs kind, exprs')
    | TernaryIf (predicate, texpr, fexpr) ->
        TernaryIf (mod_expr predicate, mod_expr texpr, mod_expr fexpr)
    | Indexed (idx_expr, indexed) ->
        let query_index = modify_index mod_expr in
        Indexed (mod_expr idx_expr, List.map ~f:query_index indexed)
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string))
      ->
        pattern
    | EAnd (lhs, rhs) -> EAnd (mod_expr lhs, mod_expr rhs)
    | EOr (lhs, rhs) -> EOr (mod_expr lhs, mod_expr rhs)
  in
  {expr with pattern= new_pattern}

(*Given a sizedtype, demote it's mem pattern from SoA to AoS*)
let rec demote_sizedtype_mem st =
  match st with
  | (SizedType.SInt | SReal) as ret -> ret
  | SVector (SoA, dim) -> SVector (AoS, dim)
  | SRowVector (SoA, dim) -> SRowVector (AoS, dim)
  | SMatrix (SoA, dim1, dim2) -> SMatrix (AoS, dim1, dim2)
  | SArray (inner_type, dim) -> SArray (demote_sizedtype_mem inner_type, dim)
  | _ -> st

(**
 * Demote statements in the MIR so that they go from Array of Structs (AoS)
 * to Struct of Arrays (SoA) .
 * @param pattern The statement to modify 
 * @param aos_exits The name of the variable we are searching for.
 *)
let rec demote_stmt_pattern
    (pattern : ('a Fixed.t, ('a, 'b) Stmt.Fixed.t) Stmt.Fixed.Pattern.t)
    (aos_exits : string Core_kernel.Set.Poly.t) =
  let mod_expr = demote_exprs aos_exits in
  let mod_stmt = demote_stmts aos_exits demote_stmt_pattern in
  let find_name = query_expr_eigen_names aos_exits in
  (*let printer intro s = Set.Poly.iter ~f:(printf intro) s in*)
  match pattern with
  | Stmt.Fixed.Pattern.Decl
      {decl_adtype; decl_id; decl_type= Type.Sized sized_type}
    when Set.Poly.mem aos_exits decl_id ->
      Stmt.Fixed.Pattern.Decl
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
            FunApp (CompilerInternal (FnReadParam (constrain_op, SoA)), args)
        ; meta= emeta } ) ->
      if Set.Poly.mem aos_exits name then
        let query_index = modify_index mod_expr in
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
      let query_index = modify_index mod_expr in
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

(**
 * Internal to the module, used when looking recursivly
 *  in statement patterns that hold statements
 *)
and demote_stmts (aos_exits : string Set.Poly.t) pattern_fn
    (Stmt.Fixed.({pattern; _}) as stmt) =
  {stmt with pattern= pattern_fn pattern aos_exits}

(* Look through an expression and find the overall type and adlevel*)
let find_args Expr.Fixed.({meta= Expr.Typed.Meta.({type_; adlevel; _}); _}) =
  (adlevel, type_)

(**
 * Check a StanLib Fun_kind to detect whether itself and it's 
 * subexpressions can be used in an SoA context.
 * 
 *)
let query_demotable_funkinds query_expr kind exprs =
  let ret_all_eigen_names =
    Set.Poly.union_list (List.map ~f:query_all_expr_eigen_names exprs)
  in
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
    match name with
    | x
      when Stan_math_signatures.is_reduce_sum_fn x
           || Stan_math_signatures.is_variadic_ode_fn x ->
        ret_all_eigen_names
    | (_ : string) ->
        let is_fun_support =
          Stan_math_signatures.query_stan_math_mem_pattern_support name
            (List.map ~f:find_args exprs)
        in
        if is_fun_support then
          Set.Poly.union_list (List.map ~f:query_expr exprs)
        else ret_all_eigen_names )
  | CompilerInternal (_ : Internal_fun.t) -> Set.Poly.empty
  | UserDefined ((_ : string), (_ : bool Fun_kind.suffix)) ->
      ret_all_eigen_names

(* Look through an expression to see whether it needs modified from
 * SoA to AoS.
 *)
let rec query_demotable_exprs (aos_exits : string Set.Poly.t)
    Expr.Fixed.({pattern; _}) =
  let query_expr = query_demotable_exprs aos_exits in
  match pattern with
  | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      query_demotable_funkinds (query_demotable_exprs aos_exits) kind exprs
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      Set.Poly.empty
  | TernaryIf (predicate, texpr, fexpr) ->
      Set.Poly.union
        (Set.Poly.union (query_expr predicate) (query_expr texpr))
        (query_expr fexpr)
  | Indexed (expr, indexed) ->
      let query_index = map_index query_expr in
      Set.Poly.union (query_expr expr)
        (Set.Poly.union_list (List.map ~f:query_index indexed))
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      Set.Poly.union (query_expr lhs) (query_expr rhs)

(* Look through a statement to see whether it needs modified from
 * SoA to AoS. Returns the set of object names that need demoted
 * in a statement, if any.
 *)
let query_demotable_stmt (aos_exits : string Set.Poly.t) pattern =
  let query_expr = query_demotable_exprs aos_exits in
  match pattern with
  | Stmt.Fixed.Pattern.Decl _ -> Set.Poly.empty
  | Assignment (((assign_name : string), (_ : UnsizedType.t), lhs), rhs) ->
      let check_lhs =
        Set.Poly.union_list (List.map ~f:(map_index query_expr) lhs)
      in
      let check_rhs = query_expr rhs in
      let is_aos_rhs = Set.Poly.length check_rhs > 0 in
      let is_aos_assigned =
        match is_aos_rhs with
        | true -> Set.Poly.singleton assign_name
        | false -> Set.Poly.empty
      in
      let is_aos_assignee =
        match Set.Poly.mem aos_exits assign_name with
        | true -> query_all_expr_eigen_names rhs
        | false -> check_rhs
      in
      Set.Poly.union_list [is_aos_assigned; check_lhs; is_aos_assignee]
  | NRFunApp (kind, exprs) ->
      query_demotable_funkinds (query_demotable_exprs aos_exits) kind exprs
  (*For the below, we just want to look at expressions*)
  | IfElse (predicate, (_ : int), (_ : int option)) -> query_expr predicate
  | Return optional_expr -> (
    match optional_expr with
    | Some expr -> query_expr expr
    | None -> Set.Poly.empty )
  | TargetPE expr -> query_expr expr
  (*Sub stmts of statements handled by Monotone Framework*)
  | For {lower; upper; _} ->
      Set.Poly.union (query_expr lower) (query_expr upper)
  | While (predicate, _) -> query_expr predicate
  | Skip | Break | Continue
   |SList (_ : int list)
   |Block (_ : int list)
   |Profile _ ->
      Set.Poly.empty

(** 
 * Given a Set of strings containing the names of objects that can be 
 * promoted from AoS -> SoA for a given expression, promote them.
 *)
let rec promote_exprs promotable_types (Expr.Fixed.({pattern; _}) as expr) =
  let mod_expr = promote_exprs promotable_types in
  let new_pattern =
    match pattern with
    | FunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
        let expr_names =
          Set.Poly.union_list (List.map ~f:query_all_expr_eigen_names exprs)
        in
        let name_diff =
          Set.Poly.length (Set.Poly.inter promotable_types expr_names)
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
        let query_index = modify_index mod_expr in
        Indexed (mod_expr idx_expr, List.map ~f:query_index indexed)
    | EAnd (lhs, rhs) -> EAnd (mod_expr lhs, mod_expr rhs)
    | EOr (lhs, rhs) -> EOr (mod_expr lhs, mod_expr rhs)
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string))
      ->
        pattern
  in
  {expr with pattern= new_pattern}

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
 * Given a Set of strings containing the names of objects that can be 
 * promoted from AoS -> SoA for a given statment, promote them.
 *)
let rec promote_stmts promotable_types (Stmt.Fixed.({pattern; _}) as stmt) =
  let mod_expr = promote_exprs promotable_types in
  let mod_stmt = promote_stmts promotable_types in
  let new_pattern =
    match pattern with
    | Stmt.Fixed.Pattern.Decl
        {decl_adtype; decl_id; decl_type= Type.Sized sized_type}
      when SizedType.contains_eigen_type sized_type ->
        Stmt.Fixed.Pattern.Decl
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
      when Set.Poly.mem promotable_types name ->
        let query_index = modify_index mod_expr in
        Assignment
          ( (name, ut, List.map ~f:query_index lhs)
          , { pattern=
                FunApp
                  ( CompilerInternal (FnReadParam (constrain_op, SoA))
                  , List.map ~f:mod_expr args )
            ; meta= emeta } )
    | Assignment (((name : string), (ut : UnsizedType.t), lhs), rhs) ->
        let modify_index = modify_index mod_expr in
        Assignment ((name, ut, List.map ~f:modify_index lhs), mod_expr rhs)
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
  in
  {stmt with pattern= new_pattern}

(**
 * Specialization of the above to handle demotion while carrying 
 * information on whether we have entered a for loop.
 * (TODO: We can probably coerce this and the 
 *  other query demotable function together)
 * 
 *)
let query_demotable_funkinds_loop query_expr_loop in_loop kind exprs =
  let ret_demotable_types =
    Set.Poly.union_list (List.map ~f:query_all_expr_eigen_names exprs)
  in
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) -> (
    match name with
    | x
      when Stan_math_signatures.is_reduce_sum_fn x
           || Stan_math_signatures.is_variadic_ode_fn x ->
        ret_demotable_types
    | _ ->
        let is_fun_support =
          Stan_math_signatures.query_stan_math_mem_pattern_support name
            (List.map ~f:find_args exprs)
        in
        if is_fun_support then
          Set.Poly.union_list (List.map ~f:(query_expr_loop in_loop) exprs)
        else ret_demotable_types )
  | CompilerInternal (_ : Internal_fun.t) -> Set.Poly.empty
  | UserDefined ((_ : string), (_ : bool Fun_kind.suffix)) ->
      ret_demotable_types

(**
 * Find indices on Matrix and Vector types that perform single 
 *  cell access. Returns true if it finds
 * a vector, row vector, matrix, or matrix with single cell access
 * as well as an array of any of the above that is accessing the 
 * inner matrix types cell.
 *)
let rec is_uni_eigen_loop_indexing (ut : UnsizedType.t)
    (index : 'a Expr.Fixed.t Index.t list) =
  match (ut, index) with
  | (UnsizedType.UArray t | UFun (_, ReturnType t, _, _)), index -> (
    match List.tl index with
    | Some cut_list -> is_uni_eigen_loop_indexing t cut_list
    | None -> is_uni_eigen_loop_indexing t [Index.All] )
  | (UVector | URowVector), [Index.Single (_ : 'a Expr.Fixed.t)] -> true
  | UMatrix, [Single (_ : 'a Expr.Fixed.t); Single (_ : 'a Expr.Fixed.t)] ->
      true
  (* None of the below contain single cell access*)
  | (UReal | UInt | UMathLibraryFunction | UFun (_, Void, _, _)), _ -> false
  | (UVector | URowVector), _ -> false
  | UMatrix, _ -> false

(**
 * Specialization to find demotable (SoA -> AoS) objects
 *  that carries logic on whether we are in a for loop or not.
 * (TODO: Could be combined with other query demotable above.)
 *)
let rec query_demotable_exprs_loop in_loop Expr.Fixed.({pattern; _}) =
  let query_expr = query_demotable_exprs_loop in_loop in
  match pattern with
  | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) ->
      query_demotable_funkinds_loop query_demotable_exprs_loop in_loop kind
        exprs
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      Set.Poly.empty
  | TernaryIf (predicate, texpr, fexpr) ->
      Set.Poly.union
        (Set.Poly.union (query_expr predicate) (query_expr texpr))
        (query_expr fexpr)
  | Indexed (Expr.Fixed.({pattern= Var name; meta= {type_; _}}), indexed) ->
      let query_index = map_index query_expr in
      if in_loop && is_uni_eigen_loop_indexing type_ indexed then
        Set.Poly.union (Set.Poly.singleton name)
          (Set.Poly.union_list (List.map ~f:query_index indexed))
      else Set.Poly.union_list (List.map ~f:query_index indexed)
  | Indexed (expr, indexed) ->
      let query_index = map_index query_expr in
      Set.Poly.union (query_expr expr)
        (Set.Poly.union_list (List.map ~f:query_index indexed))
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      Set.Poly.union (query_expr lhs) (query_expr rhs)

(**
 * Specialization of the above to handle demotion while carrying 
 * information on whether we have entered a for loop.
 * (TODO: We can probably coerce this and the 
 *  other query demotable function together)
 * 
 *)
let rec query_demotable_stmts_loop in_loop Stmt.Fixed.({pattern; _}) =
  let query_expr = query_demotable_exprs_loop in_loop in
  let query_inner_stmt checking_loop =
    query_demotable_stmts_loop checking_loop
  in
  match pattern with
  | Stmt.Fixed.Pattern.Decl _ -> Set.Poly.empty
  | Assignment (((name : string), (_ : UnsizedType.t), lhs), rhs) ->
      let check_lhs =
        Set.Poly.union_list (List.map ~f:(map_index query_expr) lhs)
      in
      let check_rhs = query_expr rhs in
      if Set.Poly.length check_rhs > 0 then
        let sub_set = Set.Poly.union check_lhs check_rhs in
        Set.Poly.add sub_set name
      else Set.Poly.union check_lhs check_rhs
  | NRFunApp (kind, exprs) ->
      query_demotable_funkinds_loop query_demotable_exprs_loop in_loop kind
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
