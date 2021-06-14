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
 * Search through an expression for the names of all types that hold matrices 
 *  and vectors.
 **)
let query_eigen_names expr =
  let get_expr_eigen_names (Dataflow_types.VVar s, Expr.Typed.Meta.({type_; _}))
      =
    if UnsizedType.contains_eigen_type type_ then Some s else None
  in
  Set.Poly.filter_map ~f:get_expr_eigen_names (Mir_utils.expr_var_set expr)

(**
 * Query an expression to check if any of it's named types exists in a set.
 *  Return true if any of the names in `name_set` are in the expression.
 **)
let contains_eigen_names (name_set : string Set.Poly.t) (expr : Typed.t) =
  not (Set.Poly.is_empty (Set.inter name_set (query_eigen_names expr)))

let modify_kind mem_pattern modifiable_set kind exprs =
  let find_eigen_names = contains_eigen_names modifiable_set in
  let expr_checker =
    match mem_pattern with
    | Common.Helpers.AoS -> List.for_all
    | Common.Helpers.SoA -> List.exists
  in
  match kind with
  | Fun_kind.StanLib (name, sfx, _) when expr_checker ~f:find_eigen_names exprs
    ->
      Fun_kind.StanLib (name, sfx, mem_pattern)
  | (_ : Fun_kind.t) -> kind

(** 
 * Modify the expressions to demote/promote from AoS <-> SoA and vice versa
 * The only real path in the below is on the functions.
 *
 * For AoS, we check that all of the matrix and vector names
 *  in the list of expressions are in the `modifiable_set` and if so
 *  make the function AoS. 
 * For SoA, we check that any of the matrix and vector names
 *  in the list of expressions are in the `modifiable_set` and if so
 *  make the function SoA. 
 * @param mem_pattern The memory pattern to change functions to.
 * @param modifiable_set The name of the variables whose
 *  associated expressions we want to modify.
 * @param pattern The expression to modify
 *)
let rec modify_expr_pattern mem_pattern modifiable_set pattern =
  let mod_expr = modify_expr mem_pattern modifiable_set in
  match pattern with
  | Expr.Fixed.Pattern.FunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
      let mod_kind = modify_kind mem_pattern modifiable_set kind exprs in
      Expr.Fixed.Pattern.FunApp (mod_kind, List.map ~f:mod_expr exprs)
  | TernaryIf (predicate, texpr, fexpr) ->
      TernaryIf (mod_expr predicate, mod_expr texpr, mod_expr fexpr)
  | Indexed (idx_expr, indexed) ->
      Indexed (mod_expr idx_expr, List.map ~f:(Index.map mod_expr) indexed)
  | EAnd (lhs, rhs) -> EAnd (mod_expr lhs, mod_expr rhs)
  | EOr (lhs, rhs) -> EOr (mod_expr lhs, mod_expr rhs)
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      pattern

(** 
   * Given a Set of strings containing the names of objects that can be 
   * promoted from AoS -> SoA for a given expression, promote them.
   *)
and modify_expr mem_pattern modifiable_set (Expr.Fixed.({pattern; _}) as expr)
    =
  let new_pattern = modify_expr_pattern mem_pattern modifiable_set pattern in
  {expr with pattern= new_pattern}

(*Given a sizedtype, demote it's mem pattern from SoA to AoS*)
let rec demote_sizedtype_mem st =
  match st with
  | ( SizedType.SInt | SReal
    | SVector (AoS, _)
    | SRowVector (AoS, _)
    | SMatrix (AoS, _, _) ) as ret ->
      ret
  | SArray (inner_type, dim) -> SArray (demote_sizedtype_mem inner_type, dim)
  | SVector (SoA, dim) -> SVector (AoS, dim)
  | SRowVector (SoA, dim) -> SRowVector (AoS, dim)
  | SMatrix (SoA, dim1, dim2) -> SMatrix (AoS, dim1, dim2)

(*Given a sizedtype, promote it's mem pattern from AoS to SoA*)
let rec promote_sizedtype_mem st =
  match st with
  | (SizedType.SInt | SReal) as ret -> ret
  | SVector (AoS, dim) -> SVector (SoA, dim)
  | SRowVector (AoS, dim) -> SRowVector (SoA, dim)
  | SMatrix (AoS, dim1, dim2) -> SMatrix (SoA, dim1, dim2)
  | SArray (inner_type, dim) -> SArray (promote_sizedtype_mem inner_type, dim)
  | _ -> st

(*Given a mem_pattern and SizedType, modify the SizedType to AoS or SoA*)
let modify_sizedtype_mem (mem_pattern : Common.Helpers.mem_pattern) st =
  match mem_pattern with
  | Common.Helpers.AoS -> demote_sizedtype_mem st
  | Common.Helpers.SoA -> promote_sizedtype_mem st

(**
 * Modify statement patterns in the MIR from AoS <-> SoA and vice versa 
 * @param mem_pattern A mem_pattern to modify expressions to. For the 
 *  given memory pattern, this modifies statement patterns and expressions it.
 * @param pattern The statement pattern to modify 
 * @param modifiable_set The name of the variable we are searching for.
 *)
let rec modify_stmt_pattern (mem_pattern : Common.Helpers.mem_pattern)
    (pattern : ('a Fixed.t, ('a, 'b) Stmt.Fixed.t) Stmt.Fixed.Pattern.t)
    (modifiable_set : string Core_kernel.Set.Poly.t) =
  let mod_expr =
    Mir_utils.map_rec_expr (modify_expr_pattern mem_pattern modifiable_set)
  in
  let mod_stmt stmt = modify_stmt mem_pattern stmt modifiable_set in
  (*let printer intro s = Set.Poly.iter ~f:(printf intro) s in*)
  match pattern with
  | Stmt.Fixed.Pattern.Decl
      ({decl_id; decl_type= Type.Sized sized_type; _} as decl)
    when Set.Poly.mem modifiable_set decl_id ->
      Stmt.Fixed.Pattern.Decl
        { decl with
          decl_type= Type.Sized (modify_sizedtype_mem mem_pattern sized_type)
        }
  | NRFunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
      let mod_kind = modify_kind mem_pattern modifiable_set kind exprs in
      NRFunApp (mod_kind, List.map ~f:mod_expr exprs)
  | Assignment
      ( ((name : string), (ut : UnsizedType.t), lhs)
      , ( { pattern=
              FunApp (CompilerInternal (FnReadParam (constrain_op, _)), args); _
          } as assigner ) )
    when Set.Poly.mem modifiable_set name ->
      Assignment
        ( (name, ut, List.map ~f:(Index.map mod_expr) lhs)
        , { assigner with
            pattern=
              FunApp
                ( CompilerInternal (FnReadParam (constrain_op, mem_pattern))
                , List.map ~f:mod_expr args ) } )
  | Assignment (((name : string), (ut : UnsizedType.t), lhs), rhs) ->
      Assignment
        ((name, ut, List.map ~f:(Index.map mod_expr) lhs), mod_expr rhs)
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
  | Profile ((a : string), stmt) -> Profile (a, List.map ~f:mod_stmt stmt)
  | While (predicate, body) -> While (mod_expr predicate, mod_stmt body)
  | Skip | Break | Continue | Decl _ -> pattern

(**
 * Internal to the module, used when looking recursivly
 *  in statement patterns that hold statements
 *)
and modify_stmt mem_pattern (Stmt.Fixed.({pattern; _}) as stmt)
    (modifiable_set : string Set.Poly.t) =
  {stmt with pattern= modify_stmt_pattern mem_pattern pattern modifiable_set}

(* Look through a statement to see whether it needs modified from
 * SoA to AoS. Returns the set of object names that need demoted
 * in a statement, if any.
 *)
let query_demotable_stmt (aos_exits : string Set.Poly.t) pattern =
  match pattern with
  | Stmt.Fixed.Pattern.Assignment
      ( ( (assign_name : string)
        , (_ : UnsizedType.t)
        , (_ : Typed.t Index.t list) )
      , rhs ) ->
      let all_rhs_eigen_names = query_eigen_names rhs in
      let rhs_fails =
        match Set.Poly.mem aos_exits assign_name with
        | true -> all_rhs_eigen_names
        | false -> Set.Poly.empty
      in
      let lhs_fails =
        match not (contains_eigen_names aos_exits rhs) with
        | false -> Set.Poly.singleton assign_name
        | true -> Set.Poly.empty
      in
      Set.Poly.union rhs_fails lhs_fails
  | Decl _
   |NRFunApp ((_ : Fun_kind.t), (_ : Typed.t list))
   |Return (_ : Typed.t option)
   |TargetPE (_ : Typed.t)
   |IfElse ((_ : Typed.t), (_ : int), (_ : int option))
   |For _
   |While (_, (_ : int))
   |Skip | Break | Continue
   |SList (_ : int list)
   |Block (_ : int list)
   |Profile ((_ : string), (_ : int list)) ->
      Set.Poly.empty

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
        Set.Poly.union_list (List.map ~f:(map_index query_expr) indexed)
      in
      if in_loop && is_uni_eigen_loop_indexing type_ indexed then
        Set.Poly.union (query_eigen_names expr) index_set
      else Set.Poly.union (query_expr expr) index_set
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      Set.Poly.empty
  | TernaryIf (predicate, texpr, fexpr) ->
      Set.Poly.union
        (Set.Poly.union (query_expr predicate) (query_expr texpr))
        (query_expr fexpr)
  | EAnd (lhs, rhs) | EOr (lhs, rhs) ->
      Set.Poly.union (query_expr lhs) (query_expr rhs)

(**
 * Query a function to detect if it or any of its used 
 *  expression's objects or expressions should be demoted to AoS.
 * @param in_loop A boolean to specify the logic of indexing expressions. See
 *  `query_initial_demotable_expr` for an explanation of the logic.
 *)
and query_initial_demotable_funs (in_loop : bool) kind (exprs : Typed.t list) =
  let query_expr = query_initial_demotable_expr in_loop in
  let all_eigen_names =
    Set.Poly.union_list (List.map ~f:query_eigen_names exprs)
  in
  match kind with
  | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), _) ->
      let find_args
          Expr.Fixed.({meta= Expr.Typed.Meta.({type_; adlevel; _}); _}) =
        (adlevel, type_)
      in
      let is_fun_support =
        Stan_math_signatures.query_stan_math_mem_pattern_support name
          (List.map ~f:find_args exprs)
      in
      if is_fun_support then Set.Poly.union_list (List.map ~f:query_expr exprs)
      else all_eigen_names
  | CompilerInternal (_ : Internal_fun.t) -> Set.Poly.empty
  | UserDefined ((_ : string), (_ : bool Fun_kind.suffix)) -> all_eigen_names

(**
 * Query to find the initial set of objects in statements that cannot be SoA.
 * This is mostly recursive over expressions and statements, with the exception of 
 * functions and Assignments.
 * @param in_loop A boolean to specify the logic of indexing expressions. See
 *  `query_initial_demotable_expr` for an explanation of the logic.
 *)
let rec query_initial_demotable_stmt (in_loop : bool) Stmt.Fixed.({pattern; _})
    =
  let query_expr = query_initial_demotable_expr in_loop in
  let query_inner_stmt checking_loop =
    query_initial_demotable_stmt checking_loop
  in
  match pattern with
  | Stmt.Fixed.Pattern.Decl _ -> Set.Poly.empty
  | Assignment (((name : string), (_ : UnsizedType.t), lhs), rhs) ->
      let check_lhs =
        Set.Poly.union_list (List.map ~f:(map_index query_expr) lhs)
      in
      let check_rhs = query_expr rhs in
      let both_sides = Set.Poly.union check_lhs check_rhs in
      if Set.Poly.length check_rhs > 0 then Set.Poly.add both_sides name
      else both_sides
  | NRFunApp (kind, exprs) -> query_initial_demotable_funs in_loop kind exprs
  | IfElse (predicate, lhs, rhs) ->
      let query_rhs =
        Option.value_map ~f:(query_inner_stmt in_loop) ~default:Set.Poly.empty
          rhs
      in
      Set.Poly.union_list
        [query_expr predicate; query_inner_stmt in_loop lhs; query_rhs]
  | Return optional_expr ->
      Option.value_map ~f:query_expr ~default:Set.Poly.empty optional_expr
  | SList lst | Profile (_, lst) | Block lst ->
      Set.Poly.union_list (List.map ~f:(query_inner_stmt in_loop) lst)
  | TargetPE expr -> query_expr expr
  | For {lower; upper; body; _} ->
      Set.Poly.union
        (Set.Poly.union (query_expr lower) (query_expr upper))
        (query_inner_stmt true body)
  | While (predicate, body) ->
      Set.Poly.union (query_expr predicate) (query_inner_stmt true body)
  | Skip | Break | Continue -> Set.Poly.empty
