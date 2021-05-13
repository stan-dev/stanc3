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
 *)
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

let rec swap_mem_pattern obj =
  match obj with
  | SizedType.SVector (Common.Helpers.SoA, dim) ->
      SizedType.SVector (Common.Helpers.AoS, dim)
  | SRowVector (Common.Helpers.SoA, dim) ->
      SRowVector (Common.Helpers.AoS, dim)
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
  let expr = Expr.Fixed.{pattern; meta} in
  let query_expr = modify_expr_functions in
  match pattern with
  | FunApp (kind, exprs) ->
      let modify_funs kind =
        match kind with
        | Fun_kind.StanLib (name, sfx, Common.Helpers.SoA) ->
            Fun_kind.StanLib (name, sfx, Common.Helpers.AoS)
        | _ -> kind
      in
      Expr.Fixed.
        {pattern= FunApp (modify_funs kind, List.map ~f:query_expr exprs); meta}
  | _ -> expr

(*
| TernaryIf (predicate, texpr, fexpr) ->
   List.concat_map ~f:query_expr [predicate; texpr; fexpr]
| EAnd (lhs, rhs) -> List.concat_map ~f:query_expr [lhs; rhs]
| EOr (lhs, rhs) -> List.concat_map ~f:query_expr [lhs; rhs]
| Indexed (expr, indexed) ->
   let query_index ind =
     (match ind with
     | Index.All -> Expr.Fixed.({pattern; meta})
     | Single index_expr -> query_expr index_expr
     | Upfrom index_expr -> query_expr index_expr
     | Between (expr_top, expr_bottom) ->
         List.concat_map ~f:query_expr [expr_top; expr_bottom]
     | MultiIndex exprs -> query_expr exprs)
   in
   List.concat [query_expr expr; List.concat_map ~f:query_index indexed]
(*Vars and Literal types can't hold rngs*)
| Var (_ : string) | Lit ((_ : Expr.Fixed.Pattern.litType), (_ : string)) ->
  Expr.Fixed.({pattern; meta})
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
**)
let rec modify_stmt_functions Stmt.Fixed.({pattern; meta}) =
  let real_stmt = Stmt.Fixed.{pattern; meta} in
  let mod_stmt mod_pattern = Stmt.Fixed.{pattern=mod_pattern; meta} in
  (*
let query_stmt = query_stmt_functions where in
let query_expr = query_expr_functions where in
*)
  match pattern with 
  | NRFunApp (StanLib (name, kind, Common.Helpers.SoA), expr) -> 
    let new_stmt = Stmt.Fixed.Pattern.NRFunApp (StanLib (name, kind, Common.Helpers.AoS), List.map ~f:modify_expr_functions expr) in
    mod_stmt new_stmt
  | Block stmts -> 
  let new_stmt = Stmt.Fixed.Pattern.Block (List.map ~f:modify_stmt_functions stmts) in
  mod_stmt new_stmt
  | SList stmts -> 
  let new_stmt = Stmt.Fixed.Pattern.SList (List.map ~f:modify_stmt_functions stmts) in
  mod_stmt new_stmt

  | _ -> real_stmt

(*
| Assignment (((_ : string), (_ : UnsizedType.t), lhs), rhs) ->
   let query_index ind =
     match ind with
     | Index.All -> [None]
     | Single ind_expr -> Index.Single (List.map ~f:query_expr ind_expr)
     | Upfrom ind_expr -> query_expr ind_expr
     | Between (expr_top, expr_bottom) ->
         List.concat_map ~f:query_expr [expr_top; expr_bottom]
     | MultiIndex exprs -> query_expr exprs
   in
   List.concat [query_expr rhs; List.concat_map ~f:query_index lhs]
| TargetPE expr -> query_expr expr
| NRFunApp (kind, exprs) -> (
   let subset = select (kind, exprs) in
   let expr_gets = List.map ~f:query_expr exprs in
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
*)
let find_any_bads obj =
  match obj with SizedType.SVector _ -> true | _ -> true

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
      match List.tl lst with
      | Some lst2 -> (
          let rewrite_decl sized_type =
            Stmt.Fixed.
              { pattern=
                  Decl {decl_adtype; decl_id; decl_type= Type.Sized sized_type}
              ; meta }
          in
          let check_bad_match = find_any_bads obj in
          match check_bad_match with
          | true ->
          (*
            raise (Failure "bababooiie")
              *)
              binder (rewrite_decl (swap_mem_pattern obj)) (rewrite_soa_to_aos (swap_fun_mem_pattern lst2))
              | false -> [rewrite_decl obj] @ rewrite_soa_to_aos lst2 )
      (*If we hit the end of the list just return the whole list*)
      | None -> lst )
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
