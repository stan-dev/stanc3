open Core_kernel
open Middle
open Middle.Expr

(**
 * Recursivly look in Decls for sized types that hold matrices or vectors
 *)
let rec get_eigen_decls Stmt.Fixed.({pattern; _}) : string Set.Poly.t =
  match pattern with
  | Stmt.Fixed.Pattern.Decl
      { decl_adtype= UnsizedType.AutoDiffable
      ; decl_id
      ; decl_type= Type.Sized sized_type }
    when SizedType.contains_eigen_type sized_type ->
      Set.Poly.singleton decl_id
  | IfElse
      ( (_ : Expr.Typed.t)
      , (true_stmt : (Typed.Meta.t, 'a) Stmt.Fixed.t)
      , (false_stmt : (Typed.Meta.t, 'a) Stmt.Fixed.t option) ) ->
      let false_op =
        match false_stmt with
        | Some x -> get_eigen_decls x
        | None -> Set.Poly.empty
      in
      Set.Poly.union (get_eigen_decls true_stmt) false_op
  | For {body; _} -> get_eigen_decls body
  | While (_, stmt) -> get_eigen_decls stmt
  | SList stmt_lst | Block stmt_lst | Profile ((_ : string), stmt_lst) ->
      Set.Poly.union_list (List.map ~f:get_eigen_decls stmt_lst)
  | Skip | Break | Continue -> Set.Poly.empty
  | _ -> Set.Poly.empty

(** See interface file *)
let rec expr_eigen_set
    Expr.Fixed.({pattern; meta= Expr.Typed.Meta.({type_; adlevel; _}) as meta})
    =
  let union_recur exprs =
    Set.Poly.union_list (List.map exprs ~f:expr_eigen_set)
  in
  match
    UnsizedType.contains_eigen_type type_ && adlevel = UnsizedType.AutoDiffable
  with
  | false -> Set.Poly.empty
  | true -> (
    match pattern with
    | Var s -> Set.Poly.singleton (Dataflow_types.VVar s, meta)
    | Lit _ -> Set.Poly.empty
    | FunApp (_, exprs) -> union_recur exprs
    | TernaryIf (expr1, expr2, expr3) -> union_recur [expr1; expr2; expr3]
    | Indexed (expr, ix) ->
        let apply_idx =
          Index.apply ~default:Set.Poly.empty ~merge:Set.Poly.union
            expr_eigen_set
        in
        Set.Poly.union_list (expr_eigen_set expr :: List.map ix ~f:apply_idx)
    | EAnd (expr1, expr2) | EOr (expr1, expr2) -> union_recur [expr1; expr2] )

(**
 * Search through an expression for the names of all types that hold matrices 
 *  and vectors.
 **)
let query_eigen_names (expr : Typed.Meta.t Expr.Fixed.t) : string Set.Poly.t =
  let get_expr_eigen_names (Dataflow_types.VVar s, Expr.Typed.Meta.({type_; _}))
      =
    if UnsizedType.contains_eigen_type type_ then Some s else None
  in
  Set.Poly.filter_map ~f:get_expr_eigen_names (expr_eigen_set expr)

(**
 * Query an expression to check if any of it's named types exists in a set.
 *  Return true if any of the names in `name_set` are in the expression.
 **)
let contains_eigen_names (name_set : string Set.Poly.t)
    (expr : Typed.Meta.t Expr.Fixed.t) : bool =
  not (Set.Poly.is_empty (Set.inter name_set (query_eigen_names expr)))

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
let modify_kind (mem_pattern : Common.Helpers.mem_pattern)
    (modifiable_set : string Set.Poly.t) (kind : Fun_kind.t)
    (exprs : Typed.Meta.t Expr.Fixed.t list) : Fun_kind.t =
  let find_eigen_names = contains_eigen_names modifiable_set in
  let expr_checker =
    match mem_pattern with
    | Common.Helpers.AoS -> List.for_all
    | Common.Helpers.SoA -> List.exists
  in
  match kind with
  | Fun_kind.StanLib (name, sfx, (_ : Common.Helpers.mem_pattern))
    when expr_checker ~f:find_eigen_names exprs ->
      Fun_kind.StanLib (name, sfx, mem_pattern)
  | (_ : Fun_kind.t) -> kind

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
let rec modify_expr_pattern (mem_pattern : Common.Helpers.mem_pattern)
    (modifiable_set : string Set.Poly.t)
    (pattern : Typed.Meta.t Expr.Fixed.t Fixed.Pattern.t) =
  let mod_expr = modify_expr mem_pattern modifiable_set in
  match pattern with
  | Expr.Fixed.Pattern.FunApp (kind, (exprs : Typed.Meta.t Expr.Fixed.t list))
    ->
      let new_exprs = List.map ~f:mod_expr exprs in
      let mod_kind = modify_kind mem_pattern modifiable_set in
      Expr.Fixed.Pattern.FunApp (mod_kind kind new_exprs, new_exprs)
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
 * modified from AoS <-> SoA and vice versa, modify them within the expression.
 * @param mem_pattern The memory pattern to change expressions to.
 * @param modifiable_set The name of the variables whose
 *  associated expressions we want to modify.
 * @param expr the expression to modify.
 *)
and modify_expr (mem_pattern : Common.Helpers.mem_pattern)
    (modifiable_set : string Set.Poly.t) (Expr.Fixed.({pattern; _}) as expr) =
  {expr with pattern= modify_expr_pattern mem_pattern modifiable_set pattern}

(**
 * Modify statement patterns in the MIR from AoS <-> SoA and vice versa 
 * @param mem_pattern A mem_pattern to modify expressions to. For the 
 *  given memory pattern, this modifies 
 *  statement patterns and expressions to it.
 * @param pattern The statement pattern to modify 
 * @param modifiable_set The name of the variable we are searching for.
 *)
let rec modify_stmt_pattern (mem_pattern : Common.Helpers.mem_pattern)
    (pattern :
      ( Typed.Meta.t Expr.Fixed.t
      , (Typed.Meta.t, Stmt.Located.Meta.t) Stmt.Fixed.t )
      Stmt.Fixed.Pattern.t) (modifiable_set : string Core_kernel.Set.Poly.t) =
  let mod_expr =
    Mir_utils.map_rec_expr (modify_expr_pattern mem_pattern modifiable_set)
  in
  let mod_stmt stmt = modify_stmt mem_pattern stmt modifiable_set in
  match pattern with
  | Stmt.Fixed.Pattern.Decl
      ({decl_id; decl_type= Type.Sized sized_type; _} as decl)
    when Set.Poly.mem modifiable_set decl_id ->
      Stmt.Fixed.Pattern.Decl
        { decl with
          decl_type=
            Type.Sized (SizedType.modify_sizedtype_mem mem_pattern sized_type)
        }
  | NRFunApp (kind, (exprs : Typed.Meta.t Expr.Fixed.t list)) ->
      let new_exprs = List.map ~f:mod_expr exprs in
      let mod_kind = modify_kind mem_pattern modifiable_set kind new_exprs in
      NRFunApp (mod_kind, new_exprs)
  | Assignment
      ( (name, ut, lhs)
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
and modify_stmt (mem_pattern : Common.Helpers.mem_pattern)
    (Stmt.Fixed.({pattern; _}) as stmt) (modifiable_set : string Set.Poly.t) =
  {stmt with pattern= modify_stmt_pattern mem_pattern pattern modifiable_set}

(**
 * Check one set against a base set to validate if anything in the set is 
 * in the base set.
 **)
let check_names (base_set : string Set.Poly.t) (alt_set : string Set.Poly.t) :
    bool =
  let inter_set = Set.Poly.inter base_set alt_set in
  let check_single_names (x : string) = Set.Poly.mem inter_set x in
  Set.Poly.is_empty inter_set
  || not (Set.Poly.for_all ~f:check_single_names alt_set)

(*let printer intro s = Set.Poly.iter ~f:(printf intro) s*)

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
      let rhs_type_eigen =
        UnsizedType.contains_eigen_type (Expr.Typed.type_of rhs)
      in
      let rhs_set =
        match Set.Poly.mem aos_exits assign_name && rhs_type_eigen with
        | true -> all_rhs_eigen_names
        | false -> Set.Poly.empty
      in
      let lhs_set =
        match check_names aos_exits all_rhs_eigen_names with
        | true -> Set.Poly.empty
        | false -> Set.Poly.singleton assign_name
      in
      Set.Poly.union rhs_set lhs_set
  | Decl _
   |NRFunApp ((_ : Fun_kind.t), (_ : Expr.Typed.t list))
   |Return (_ : Expr.Typed.t option)
   |TargetPE (_ : Expr.Typed.t)
   |IfElse ((_ : Expr.Typed.t), (_ : int), (_ : int option))
   |For _
   |While ((_ : Typed.t), (_ : int))
   |Skip | Break | Continue
   |SList (_ : int list)
   |Block (_ : int list)
   |Profile ((_ : string), (_ : int list)) ->
      Set.Poly.empty

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
      + List.fold_left ~init:0 ~f:is_single_idx indexed
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
and is_single_idx (acc : int) (idx : Expr.Typed.Meta.t Expr.Fixed.t Index.t) =
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
        List.fold_left ~init:0 ~f:is_single_idx index
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
      | (UReal | UInt | UMathLibraryFunction | UFun (_, Void, _, _)), _ ->
          false
      | (UVector | URowVector | UMatrix), _ -> false )

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
 * @param kind The function type, for StanLib functions we check if the 
 *  function supports SoA and for UserDefined functions we always fail 
 *  and return back all of the names of the objects passed in expressions
 *  to the UDF.
 * exprs The expression list passed to the functions.
 *)
and query_initial_demotable_funs (in_loop : bool) (kind : Fun_kind.t)
    (exprs : Typed.Meta.t Expr.Fixed.t list) : string Set.Poly.t =
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
let rec query_initial_demotable_stmt (in_loop : bool)
    (Stmt.Fixed.({pattern; _}) :
      (Expr.Typed.Meta.t, Stmt.Located.Meta.t) Stmt.Fixed.t) :
    string Set.Poly.t =
  let query_expr = query_initial_demotable_expr in_loop in
  let query_inner_stmt checking_loop =
    query_initial_demotable_stmt checking_loop
  in
  match pattern with
  | Stmt.Fixed.Pattern.Assignment
      (((name : string), (ut : UnsizedType.t), lhs), rhs) ->
      let check_lhs =
        let lhs_list =
          Set.Poly.union_list
            (List.map
               ~f:
                 (Index.apply ~default:Set.Poly.empty ~merge:Set.Poly.union
                    query_expr)
               lhs)
        in
        match is_uni_eigen_loop_indexing in_loop ut lhs with
        | true -> Set.Poly.add lhs_list name
        | false -> lhs_list
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
  | Skip | Break | Continue | Decl _ -> Set.Poly.empty
