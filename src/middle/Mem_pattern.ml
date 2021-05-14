(* A partial evaluator for use in static analysis and optimization *)

open Core_kernel

(*open Mir_utils*)

(** Query function expressions in expressions returning back a list of optionals 
 *    with each Some element holding the queried function types.
 * @param select A functor taking in a tuple of the same types as 
 *  those in `FunApp` and returning a subset of the `FunApp`'s types.
 * @param where A functor that accepts a tuple returned by select
 *  and returns either true or false.
 *  This is used to decide if a function's subsetted tuple should be returned.
 * @param pattern A pattern of fixed expressions to recurse over.
let rec query_expr_functions select (where : 'a -> bool)
    Expr.Fixed.({pattern; _}) =
  let query_expr = query_expr_functions select where in
  match pattern with
  | FunApp (kind, exprs) -> (
      let subset = select (kind, exprs) in
      match where subset with
      | true -> List.concat [[Some subset]; List.concat_map ~f:query_expr exprs]
      | false -> List.concat_map ~f:query_expr exprs )
  | TernaryIf (predicate, texpr, fexpr) ->
      List.concat_map ~f:query_expr [predicate; texpr; fexpr]
  | EAnd (lhs, rhs) -> List.concat_map ~f:query_expr [lhs; rhs]
  | EOr (lhs, rhs) -> List.concat_map ~f:query_expr [lhs; rhs]
  | Indexed (expr, indexed) ->
      let query_index ind =
        match ind with
        | Index.All -> [None]
        | Single index_expr -> query_expr index_expr
        | Upfrom index_expr -> query_expr index_expr
        | Between (expr_top, expr_bottom) ->
            List.concat_map ~f:query_expr [expr_top; expr_bottom]
        | MultiIndex exprs -> query_expr exprs
      in
      List.concat [query_expr expr; List.concat_map ~f:query_index indexed]
  (*Vars and Literal types can't hold rngs*)
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      [None]
 *)

(** Query function expressions in statements returning back a list
 *   of optionals with the the elements of the list holding the 
 *   selected subset of function arguments.
 *   For an example of this function see `pp_rng_in_td`.
 * @param select A functor taking in a tuple of the same types as 
 *  those in `FunApp` and returning a subset of the `FunApp`'s types.
 * @param where A functor that accepts a tuple returned by select
 *   returning true or false. This is used to decide if a function's
 *   subsetted tuple should be returned.
 * @param pattern A pattern of fixed statements to recurse over.
let rec query_stmt_functions select (where : 'a -> bool)
    Stmt.Fixed.({pattern; _}) =
  let query_stmt = query_stmt_functions select where in
  let query_expr = query_expr_functions select where in
  match pattern with
  | Assignment (((_ : string), (_ : UnsizedType.t), lhs), rhs) ->
      let query_index ind =
        match ind with
        | Index.All -> [None]
        | Single ind_expr -> query_expr ind_expr
        | Upfrom ind_expr -> query_expr ind_expr
        | Between (expr_top, expr_bottom) ->
            List.concat_map ~f:query_expr [expr_top; expr_bottom]
        | MultiIndex exprs -> query_expr exprs
      in
      List.concat [query_expr rhs; List.concat_map ~f:query_index lhs]
  | TargetPE expr -> query_expr expr
  | NRFunApp (kind, exprs) -> (
      let subset = select (kind, exprs) in
      let expr_gets = List.concat_map ~f:query_expr exprs in
      match where subset with
      | true -> List.concat [[Some subset]; expr_gets]
      | false -> expr_gets )
  | Break | Continue -> [None]
  | Return optional_expr -> (
    match optional_expr with Some expr -> query_expr expr | None -> [None] )
  | Skip -> [None]
  | IfElse (predicate, true_stmt, op_false_stmt) -> (
      let pred_query = query_expr predicate in
      let true_query = query_stmt true_stmt in
      match op_false_stmt with
      | Some stmt -> List.concat [pred_query; true_query; query_stmt stmt]
      | None -> List.concat [true_query; pred_query] )
  | While (expr, stmt) -> List.concat [query_expr expr; query_stmt stmt]
  | For {lower; upper; body; _} ->
      List.concat [query_expr lower; query_expr upper; query_stmt body]
  | Profile ((_ : string), stmt) -> List.concat_map ~f:query_stmt stmt
  | Block stmts -> List.concat_map ~f:query_stmt stmts
  | SList stmts -> List.concat_map ~f:query_stmt stmts
  (*A Decl's record does not hold functions*)
  | Decl _ -> [None]
 **)

let rec swap_mem_pattern obj =
  match obj with
  | SizedType.SVector (Common.Helpers.SoA, dim) ->
      SizedType.SVector (Common.Helpers.AoS, dim)
  | SRowVector (Common.Helpers.SoA, dim) -> SRowVector (Common.Helpers.AoS, dim)
  | SMatrix (Common.Helpers.SoA, rows, cols) ->
      SMatrix (Common.Helpers.AoS, rows, cols)
  | SArray (inner_type, (dim : Expr.Typed.t)) ->
      SArray (swap_mem_pattern inner_type, dim)
  | _ -> obj

(** Query function expressions in expressions returning back a list of optionals 
 *    with each Some element holding the queried function types.
 * @param select A functor taking in a tuple of the same types as 
 *  those in `FunApp` and returning a subset of the `FunApp`'s types.
 * @param where A functor that accepts a tuple returned by select
 *  and returns either true or false.
 *  This is used to decide if a function's subsetted tuple should be returned.
 * @param pattern A pattern of fixed expressions to recurse over.
 *)
let rec modify_expr_functions Expr.Fixed.({pattern; meta}) =
  let query_expr = modify_expr_functions in
  let new_pattern =
    match pattern with
    | FunApp (kind, (exprs : 'a Expr.Fixed.t list)) ->
        let modify_funs kind =
          match kind with
          | Fun_kind.StanLib (name, sfx, Common.Helpers.SoA) ->
              Fun_kind.StanLib (name, sfx, Common.Helpers.AoS)
          | _ -> kind
        in
        Expr.Fixed.Pattern.FunApp
          (modify_funs kind, List.map ~f:query_expr exprs)
    | TernaryIf (predicate, texpr, fexpr) ->
        TernaryIf (query_expr predicate, query_expr texpr, query_expr fexpr)
    | Indexed (expr, indexed) ->
        let query_index ind =
          match ind with
          | Index.All -> Index.All
          | Single ind_expr -> Single (query_expr ind_expr)
          | Upfrom ind_expr -> Upfrom (query_expr ind_expr)
          | Between (expr_top, expr_bottom) ->
              Between (query_expr expr_top, query_expr expr_bottom)
          | MultiIndex exprs -> MultiIndex (query_expr exprs)
        in
        Indexed (query_expr expr, List.map ~f:query_index indexed)
    | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string))
      ->
        pattern
    | EAnd (lhs, rhs) -> EAnd (query_expr lhs, query_expr rhs)
    | EOr (lhs, rhs) -> EOr (query_expr lhs, query_expr rhs)
  in
  Expr.Fixed.{pattern= new_pattern; meta}

(** Query function expressions in statements returning back a list
*   of optionals with the the elements of the list holding the 
*   selected subset of function arguments.
*   For an example of this function see `pp_rng_in_td`.
* @param select A functor taking in a tuple of the same types as 
*  those in `FunApp` and returning a subset of the `FunApp`'s types.
* @param where A functor that accepts a tuple returned by select
*   returning true or false. This is used to decide if a function's
*   subsetted tuple should be returned.
* @param pattern A pattern of fixed statements to recurse over.
**)
let rec modify_stmt_functions Stmt.Fixed.({pattern; meta}) =
  let query_expr = modify_expr_functions in
  let query_stmt = modify_stmt_functions in
  let rec new_pattern pattern =
    match pattern with
    | Stmt.Fixed.Pattern.NRFunApp (StanLib (name, kind, Common.Helpers.SoA), expr) ->
        Stmt.Fixed.Pattern.NRFunApp
          ( StanLib (name, kind, Common.Helpers.AoS)
          , List.map ~f:modify_expr_functions expr )
    | NRFunApp (fun_kind, expr) ->
        NRFunApp (fun_kind, List.map ~f:modify_expr_functions expr)
    | Assignment (((name : string), (ut : UnsizedType.t), lhs), rhs) ->
        let query_index ind =
          match ind with
          | Index.All -> Index.All
          | Single ind_expr -> Single (query_expr ind_expr)
          | Upfrom ind_expr -> Upfrom (query_expr ind_expr)
          | Between (expr_top, expr_bottom) ->
              Between (query_expr expr_top, query_expr expr_bottom)
          | MultiIndex exprs -> MultiIndex (query_expr exprs)
        in
        Assignment ((name, ut, List.map ~f:query_index lhs), query_expr rhs)
    | IfElse (predicate, true_stmt, op_false_stmt) ->
        let pred_query = query_expr predicate in
        let true_query = query_stmt true_stmt in
        let blah =
          match op_false_stmt with
          | Some stmt -> Some (query_stmt stmt)
          | None -> None
        in
        IfElse (pred_query, true_query, blah)
    | Block stmts -> Block (List.map ~f:modify_stmt_functions stmts)
    | SList stmts -> SList (List.map ~f:modify_stmt_functions stmts)
    | For
        { loopvar
        ; lower
        ; upper
        ; body= Stmt.Fixed.({pattern= a; meta= metablock}) } ->
        let ooof = new_pattern a in
        Stmt.Fixed.Pattern.For
          { loopvar
          ; lower
          ; upper
          ; body= Stmt.Fixed.{pattern= ooof; meta= metablock} }
    | TargetPE expr ->
        let modee = query_expr expr in
        TargetPE modee
    | Return optional_expr -> (
      match optional_expr with
      | Some expr -> Return (Some (query_expr expr))
      | None -> Return None )
    | Profile ((a : string), stmt) -> Profile (a, List.map ~f:query_stmt stmt)
    | Skip | Decl _ | Break | Continue -> pattern
    | While (predicate, body) -> While (query_expr predicate, query_stmt body)
  in
  Stmt.Fixed.{pattern= (new_pattern pattern); meta}

let rec query_for_name_functions var_name Expr.Fixed.({pattern; _}) =
  let query_name = query_for_name_functions var_name in
  match pattern with
  | FunApp (_, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) -> 
        List.for_all ~f:query_name exprs 
  | TernaryIf (predicate, texpr, fexpr) ->
      query_name predicate && query_name texpr && query_name fexpr
  | Indexed (expr, indexed) ->
      let query_index ind =
        match ind with
        | Index.All -> true
        | Single ind_expr -> query_name ind_expr
        | Upfrom ind_expr -> query_name ind_expr
        | Between (expr_top, expr_bottom) ->
            query_name expr_top && query_name expr_bottom
        | MultiIndex exprs -> query_name exprs
      in
      query_name expr && List.exists ~f:query_index indexed
  | Var (a : string) -> a = var_name
  | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) -> true
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_name lhs && query_name rhs

(** Query function expressions in expressions returning back a list of optionals 
 *    with each Some element holding the queried function types.
 * @param select A functor taking in a tuple of the same types as 
 *  those in `FunApp` and returning a subset of the `FunApp`'s types.
 * @param where A functor that accepts a tuple returned by select
 *  and returns either true or false.
 *  This is used to decide if a function's subsetted tuple should be returned.
 * @param pattern A pattern of fixed expressions to recurse over.
*)
let rec query_expr_functions var_name Expr.Fixed.({pattern; _}) =
  let query_expr = query_expr_functions var_name in
  let query_name = query_for_name_functions var_name in
  match pattern with
  | FunApp (kind, (exprs : Expr.Typed.Meta.t Expr.Fixed.t list)) -> (
    match kind with
    | Fun_kind.StanLib (name, (_ : bool Fun_kind.suffix), Common.Helpers.SoA)
      -> (
        let does_name_exist = List.for_all ~f:query_name exprs in
        match does_name_exist with
        | false -> true
        | true ->
            let make_args =
              let find_args
                  Expr.Fixed.({meta= Expr.Typed.Meta.({type_; adlevel; _}); _})
                  =
                (adlevel, type_)
              in
              List.map ~f:find_args exprs
            in
            let check_fun_support =
              Stan_math_signatures.query_stan_math_mem_pattern_support name
                make_args
            in
            check_fun_support && List.for_all ~f:query_expr exprs)
    | CompilerInternal _ -> true
    | Fun_kind.StanLib (_, _, Common.Helpers.AoS) -> false
    | UserDefined _ -> false )
  | TernaryIf (predicate, texpr, fexpr) ->
      query_expr predicate && query_expr texpr && query_expr fexpr
  | Indexed (expr, indexed) ->
      let query_index ind =
        match ind with
        | Index.All -> true
        | Single ind_expr -> query_expr ind_expr
        | Upfrom ind_expr -> query_expr ind_expr
        | Between (expr_top, expr_bottom) ->
            query_expr expr_top && query_expr expr_bottom
        | MultiIndex exprs -> query_expr exprs
      in
      query_expr expr && List.exists ~f:query_index indexed
  | Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
      true
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> query_expr lhs && query_expr rhs

(** Query function expressions in statements returning back a list
*   of optionals with the the elements of the list holding the 
*   selected subset of function arguments.
*   For an example of this function see `pp_rng_in_td`.
* @param select A functor taking in a tuple of the same types as 
*  those in `FunApp` and returning a subset of the `FunApp`'s types.
* @param where A functor that accepts a tuple returned by select
*   returning true or false. This is used to decide if a function's
*   subsetted tuple should be returned.
* @param pattern A pattern of fixed statements to recurse over.
**)
let rec query_stmt_functions var_name Stmt.Fixed.({pattern; _}) =
  let query_expr = query_expr_functions var_name in
  let query_stmt = query_stmt_functions var_name in
  let query_name = query_for_name_functions var_name in
  let rec find_pattern pattern = (
  match pattern with
  | Stmt.Fixed.Pattern.NRFunApp
      ( StanLib
          ((name : string), (_ : bool Fun_kind.suffix), Common.Helpers.SoA)
      , (exprs : Expr.Typed.Meta.t Expr.Fixed.t list) ) -> (
      let does_name_exist = List.for_all ~f:query_name exprs in
      match does_name_exist with
      | false -> true
      | true ->
          let make_args =
            let find_args
                Expr.Fixed.({meta= Expr.Typed.Meta.({type_; adlevel; _}); _}) =
              (adlevel, type_)
            in
            List.map ~f:find_args exprs
          in
          let check_fun_support =
            Stan_math_signatures.query_stan_math_mem_pattern_support name
              make_args
          in
          check_fun_support && List.for_all ~f:query_expr exprs)
  | NRFunApp ((_ : Fun_kind.t), (expr : Expr.Typed.Meta.t Expr.Fixed.t list))
    ->
      List.for_all ~f:query_expr expr
  | Assignment (((_ : string), (_ : UnsizedType.t), lhs), rhs) ->
      let query_index ind =
        match ind with
        | Index.All -> true
        (*This should really be in the loop block but for now always fail*)
        | Single _ -> false
        | Upfrom ind_expr -> query_expr ind_expr
        | Between (expr_top, expr_bottom) ->
            query_expr expr_top && query_expr expr_bottom
        | MultiIndex exprs -> query_expr exprs
      in
      List.for_all ~f:query_index lhs && query_expr rhs
  | IfElse (predicate, true_stmt, op_false_stmt) ->
      let pred_query = query_expr predicate in
      let true_query = query_stmt true_stmt in
      let blah =
        match op_false_stmt with Some stmt -> query_stmt stmt | None -> true
      in
      pred_query && true_query && blah
  | Block stmts -> List.for_all ~f:query_stmt stmts
  | SList stmts -> List.for_all ~f:query_stmt stmts
  | For {lower; upper; body= Stmt.Fixed.({pattern= a; _}); _} ->
      let ooof = find_pattern a in
      query_expr lower && query_expr upper && ooof
  | TargetPE expr -> query_expr expr
  | Return optional_expr -> (
    match optional_expr with Some expr -> query_expr expr | None -> true )
  | Profile ((_ : string), stmt) -> List.for_all ~f:query_stmt stmt
  | Skip | Decl _ | Break | Continue -> true
  | While (predicate, body) -> query_expr predicate && query_stmt body)
  in find_pattern pattern

let find_any_bads var_name lst =
  let query_stmts = query_stmt_functions var_name in
  match lst with
  | Some lst2 -> List.for_all ~f:query_stmts lst2
  | None -> false

let swap_fun_mem_pattern lst = List.map ~f:modify_stmt_functions lst

(*
  * Here, search for Lst.tl for a function that goofs with the SoA.
  *  If we find it then switch the SoA in the decl_type to AoS.
  * Then we need to rewrite the MIR to fix everywhere that decl_id
  * is used, then we need to do cmon_son again on List.tl lst
  *)
let rec rewrite_soa_to_aos lst =
  let binder other lst =
    match List.tl lst with
    | Some a -> [other] @ rewrite_soa_to_aos a
    | None -> lst
  in
  let rewrite_stmt pattern meta = Stmt.Fixed.{pattern; meta} in
  match List.hd lst with
  | Some Stmt.Fixed.({pattern; meta}) -> (
    match pattern with
    | Decl {decl_adtype; decl_id; decl_type= Type.Sized obj} -> (
        (* At this point I need to search the rest of the list for 
       * the decl_id and decl_type's SOA *)
        let rewrite_decl sized_type =
          Stmt.Fixed.
            { pattern= Decl {decl_adtype; decl_id; decl_type= Sized sized_type}
            ; meta }
        in
        let check_bad_match = find_any_bads decl_id (List.tl lst) in
        match check_bad_match with
        | false ->
            binder
              (rewrite_decl (swap_mem_pattern obj))
              (swap_fun_mem_pattern lst)
        | true -> binder (rewrite_decl obj) lst )
    | Block inner_lst ->
        let ooof = rewrite_soa_to_aos inner_lst in
        let blah2 = Stmt.Fixed.Pattern.Block ooof in
        let blah = rewrite_stmt blah2 meta in
        binder blah lst
    | SList inner_lst ->
        let ooof = rewrite_soa_to_aos inner_lst in
        let blah2 = Stmt.Fixed.Pattern.SList ooof in
        let blah = rewrite_stmt blah2 meta in
        binder blah lst
    | For
        { loopvar
        ; lower
        ; upper
        ; body= Stmt.Fixed.({pattern= Block wooof; meta= metablock}) } ->
        let ooof = rewrite_soa_to_aos wooof in
        let blah2 =
          Stmt.Fixed.Pattern.For
            { loopvar
            ; lower
            ; upper
            ; body= Stmt.Fixed.{pattern= Block ooof; meta= metablock} }
        in
        let blah = rewrite_stmt blah2 meta in
        binder blah lst
    | While (predicate, Stmt.Fixed.({pattern= Block wooof; meta= metablock}))
      ->
        let ooof = rewrite_soa_to_aos wooof in
        let blah2 =
          Stmt.Fixed.Pattern.While
            (predicate, Stmt.Fixed.{pattern= Block ooof; meta= metablock})
        in
        let blah = rewrite_stmt blah2 meta in
        binder blah lst
    | stmt -> binder (rewrite_stmt stmt meta) lst )
  | None -> lst

(*
    | Assignment _ -> stmt
    | TargetPE _ -> stmt
    | NRFunApp _ -> stmt
    | Break | Continue -> stmt
    | Return _ -> stmt
    | Skip -> stmt
    | IfElse (predicate, true_stmt, op_false_stmt) -> 
      let false_stmt = 
        (match op_false_stmt with 
        | Some stmt -> 
          let ahh = squish_stmt_loops stmt in Some ahh  
        | None -> None) in
      Stmt.Fixed.({pattern=IfElse (predicate, squish_stmt_loops true_stmt, false_stmt); meta=meta})
    | While _ -> stmt
    | For {loopvar; lower; upper; body} ->
      let new_body = zip_for_loop (loopvar, lower, upper, body) in
      Stmt.Fixed.({pattern=For {loopvar; lower; upper; body=new_body}; meta=meta})
    | Profile _ -> stmt
    | Block stmts -> 
      let new_stmts = List.map ~f:squish_stmt_loops stmts in
      Stmt.Fixed.({pattern= Block new_stmts; meta=meta})
    | SList stmts -> 
      let new_stmts = List.map ~f:squish_stmt_loops stmts in
      Stmt.Fixed.({pattern= SList new_stmts; meta=meta}))
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
