(* Flips SoA to AoS *)

open Core_kernel

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
let rec query_for_name_functions var_name Expr.Fixed.({pattern; _}) =
  let query_name = query_for_name_functions var_name in
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
 * Modify functions expressions from SoA to AoS
 *)
let rec modify_expr_functions var_name Expr.Fixed.({pattern; meta}) =
  let mod_expr = modify_expr_functions var_name in
  let find_name = query_for_name_functions var_name in
  (*TODO: Only modify FunApps that have the variable name in their exprs*)
  let new_pattern =
    match pattern with
    | FunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
        let modify_funs kind =
          match kind with
          | Fun_kind.StanLib (name, sfx, Common.Helpers.SoA) as func -> (
            match List.exists ~f:find_name exprs with
            | true -> Fun_kind.StanLib (name, sfx, Common.Helpers.AoS)
            | false -> func )
          | _ -> kind
        in
        Expr.Fixed.Pattern.FunApp (modify_funs kind, List.map ~f:mod_expr exprs)
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

let rec modify_stmt_functions var_name Stmt.Fixed.({pattern; meta}) =
  let mod_expr = modify_expr_functions var_name in
  let mod_stmt = modify_stmt_functions var_name in
  let find_name = query_for_name_functions var_name in
  let rec mod_pattern pattern =
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
    | For
        { loopvar
        ; lower
        ; upper
        ; body= Stmt.Fixed.({pattern= sub_pattern; meta= metablock}) } ->
        let new_pattern = mod_pattern sub_pattern in
        Stmt.Fixed.Pattern.For
          { loopvar
          ; lower
          ; upper
          ; body= Stmt.Fixed.{pattern= new_pattern; meta= metablock} }
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
 *)
let rec query_expr_functions var_name Expr.Fixed.({pattern; _}) =
  let query_expr = query_expr_functions var_name in
  let query_name = query_for_name_functions var_name in
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
    | Fun_kind.StanLib (_, _, Common.Helpers.AoS) -> true
    | UserDefined _ -> true )
  | TernaryIf (predicate, texpr, fexpr) ->
      query_expr predicate || query_expr texpr || query_expr fexpr
  | Indexed (expr, indexed) ->
      let query_index = search_index query_expr in
      query_expr expr || List.exists ~f:query_index indexed
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      false
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_expr lhs || query_expr rhs

(* Look through a statement to see whether it needs modified from
 * SoA to AoS.
 * TODO: The logic on when to return true/false is wrong.
 * I really want to move from returning true/false to return SoA/AoS
 * which should be a lot easier to read.
 *  I think we want to use this in an "is any" style context
 *)
let rec query_stmt_functions var_name Stmt.Fixed.({pattern; _}) =
  let query_expr = query_expr_functions var_name in
  let query_stmt = query_stmt_functions var_name in
  let query_name = query_for_name_functions var_name in
  let rec find_pattern pattern =
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
    | NRFunApp (UserDefined _, _) -> true
    | NRFunApp (StanLib (_, _, AoS), _) -> true
    | Assignment (((_ : string), (_ : UnsizedType.t), lhs), rhs) ->
        let query_index = search_index query_expr in
        List.exists ~f:query_index lhs || query_expr rhs
    | IfElse (predicate, true_stmt, op_false_stmt) ->
        let pred_query = query_expr predicate in
        let true_query = query_stmt true_stmt in
        let false_query =
          match op_false_stmt with
          | Some stmt -> query_stmt stmt
          | None -> false
        in
        pred_query || true_query || false_query
    | Block stmts -> List.exists ~f:query_stmt stmts
    | SList stmts -> List.exists ~f:query_stmt stmts
    | For {lower; upper; body= Stmt.Fixed.({pattern= sub_pattern; _}); _} ->
        query_expr lower || query_expr upper || find_pattern sub_pattern
    | TargetPE expr -> query_expr expr
    | Return optional_expr -> (
      match optional_expr with Some expr -> query_expr expr | None -> false )
    | Profile ((_ : string), stmt) -> List.exists ~f:query_stmt stmt
    | Skip | Decl _ | Break | Continue -> false
    | While (predicate, body) -> query_expr predicate || query_stmt body
  in
  find_pattern pattern

(*
 * This is where we do the query to check if we should 
 * modify the list of statements.
 *)
let find_any_bads var_name lst =
  let query_stmts = query_stmt_functions var_name in
  match lst with
  | Some lst2 -> (
    match List.for_all ~f:query_stmts lst2 with
    | true -> Common.Helpers.AoS
    | false -> SoA )
  | None -> SoA

(**
 * If the queries come back with true, we use this to swap the 
 * Decl's SizedType's mem pattern.
 *)
let rec swap_mem_pattern obj =
  match obj with
  | SizedType.SVector (Common.Helpers.SoA, (dim : Expr.Typed.t)) ->
      SizedType.SVector (AoS, dim)
  | SRowVector (SoA, dim) -> SRowVector (AoS, dim)
  | SMatrix (SoA, rows, cols) -> SMatrix (AoS, rows, cols)
  | SArray (inner_type, dim) -> SArray (swap_mem_pattern inner_type, dim)
  | _ -> obj

let rec contains_eigen (ut : UnsizedType.t) : bool =
  match ut with
  | UnsizedType.UArray t -> contains_eigen t
  | UMatrix | URowVector | UVector -> true
  | UInt | UReal | UMathLibraryFunction | UFun _ -> false

(*
  * Here, search for the list of statments for Decl's, then search
  * through the rest of the list to see if that Decl can be a 
  * struct of arrays or arrays of structs.
  *  If we find it then switch the SoA in the decl_type to AoS.
  * The main path we care about is `Decl`. For each Decl we want to:
  * 1. Check if it's a matrix type, if not then we cares,
  * 2. Search for stan math functions in the rest of the statements.
  *    - When we find one, we want to search it's argument's
  *       expressions to see if our object in the decl 
  *       is used (so looking for a Var name)
  *    - If we find it in there, that's where things get 
  *      a little more complicated.
  *    - I _think_, what we need to do is that if 
  *      the object is used *anywhere* in that functions arguments,
  *      since we always return back a varmat with a varmat
  *      input then we need to check that our outer function
  *      supports varmat, then run the same scheme above
  *      on the expressions in the functions arguments.
  * 
  * If (2) returns true, it means all is good and this Decl 
  *  can produce a varmat. If (2) comes back false, we need to 
  *  go through the list of statements, find everywhere that 
  *  that object is used, and modify the functions so that they
  *  are tagged as AoS instead of SoA. Then we finish by
  *  rewriting the Decl to also be an AoS and continuing our 
  *  search recursivly for other Decls.
  **)
let rec rewrite_soa_to_aos lst =
  let binder other lst =
    match List.tl lst with
    | Some sub_lst -> [other] @ rewrite_soa_to_aos sub_lst
    | None -> lst
  in
  let rewrite_stmt pattern meta = Stmt.Fixed.{pattern; meta} in
  let top_list = List.hd lst in
  match top_list with
  | Some Stmt.Fixed.({pattern; meta}) -> (
    match pattern with
    | Decl {decl_adtype; decl_id; decl_type= Type.Sized obj} -> (
        let rewrite_decl sized_type =
          Stmt.Fixed.
            { pattern= Decl {decl_adtype; decl_id; decl_type= Sized sized_type}
            ; meta }
        in
        match contains_eigen (SizedType.to_unsized obj) with
        | true -> (
            let check_bad_match = find_any_bads decl_id (List.tl lst) in
            let swap_fun_mem_pattern decl_id lst =
              let mod_stmts = modify_stmt_functions decl_id in
              List.map ~f:mod_stmts lst
            in
            match check_bad_match with
            | AoS ->
                binder
                  (rewrite_decl (swap_mem_pattern obj))
                  (swap_fun_mem_pattern decl_id lst)
            | SoA -> binder (rewrite_decl obj) lst )
        (*TODO: Figure out SArray*)
        | false -> binder (rewrite_decl obj) lst )
    | Block inner_lst ->
        let new_block =
          Stmt.Fixed.Pattern.Block (rewrite_soa_to_aos inner_lst)
        in
        binder (rewrite_stmt new_block meta) lst
    | SList inner_lst ->
        let new_list =
          Stmt.Fixed.Pattern.SList (rewrite_soa_to_aos inner_lst)
        in
        binder (rewrite_stmt new_list meta) lst
    (*Idk how to handle these block and whiles. I think I just want to
     * search them if they contains types that have lists?*)
    | For
        { loopvar
        ; lower
        ; upper
        ; body= {pattern= Block sub_list; meta= metablock} } ->
        let mod_sub_list = rewrite_soa_to_aos sub_list in
        let new_for =
          Stmt.Fixed.Pattern.For
            { loopvar
            ; lower
            ; upper
            ; body= Stmt.Fixed.{pattern= Block mod_sub_list; meta= metablock}
            }
        in
        binder (rewrite_stmt new_for meta) lst
    | For
        { loopvar
        ; lower
        ; upper
        ; body= {pattern= SList sub_list; meta= metablock} } ->
        let mod_sub_list = rewrite_soa_to_aos sub_list in
        let new_for =
          Stmt.Fixed.Pattern.For
            { loopvar
            ; lower
            ; upper
            ; body= Stmt.Fixed.{pattern= Block mod_sub_list; meta= metablock}
            }
        in
        binder (rewrite_stmt new_for meta) lst
    | While (predicate, {pattern= Block sub_list; meta= metablock}) ->
        let mod_sub_list = rewrite_soa_to_aos sub_list in
        let new_while =
          Stmt.Fixed.Pattern.While
            ( predicate
            , Stmt.Fixed.{pattern= Block mod_sub_list; meta= metablock} )
        in
        binder (rewrite_stmt new_while meta) lst
    | While (predicate, {pattern= SList sub_list; meta= metablock}) ->
        let mod_sub_list = rewrite_soa_to_aos sub_list in
        let new_while =
          Stmt.Fixed.Pattern.While
            ( predicate
            , Stmt.Fixed.{pattern= Block mod_sub_list; meta= metablock} )
        in
        binder (rewrite_stmt new_while meta) lst
    | pat -> binder (rewrite_stmt pat meta) lst )
  | None -> lst

(*
  * TODO: Need to also go over functions block
  * Ideally, we would search the functions in the 
  * functions block to see if they use types that are SoA supported
  * Then we would pass those along to rewrite_soa_to_aos
  * for log_prob
  *)
let eval_prog (mir : (Expr.Typed.t, Stmt.Located.t) Program.t) :
    (Expr.Typed.t, Stmt.Located.t) Program.t =
  { Program.functions_block= mir.functions_block
  ; input_vars= mir.input_vars
  ; prepare_data= mir.prepare_data
  ; log_prob= rewrite_soa_to_aos mir.log_prob
  ; generate_quantities= mir.generate_quantities
  ; transform_inits= mir.transform_inits
  ; output_vars= mir.output_vars
  ; prog_name= mir.prog_name
  ; prog_path= mir.prog_path }
