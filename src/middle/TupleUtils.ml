open Core_kernel

(* Cannot be placed in Utils.ml due to circular import issue
   TUPLE TODO find a better place for these
*)

let zip_tuple_trans_exn pst trans =
  let tms =
    match trans with
    | Transformation.TupleTransformation tms -> tms
    | _ ->
        Common.FatalError.fatal_error_msg
          [%message "Internal error: expected TupleTransformation with Tuple"]
  in
  let rec tuple_psts pst =
    match pst with
    | Type.Unsized (UTuple uts) -> List.map ~f:(fun ut -> Type.Unsized ut) uts
    | Type.Unsized (UArray ut) -> tuple_psts (Type.Unsized ut)
    | Type.Sized (STuple sts) -> List.map ~f:(fun st -> Type.Sized st) sts
    | Type.Sized (SArray (st, _)) -> tuple_psts (Type.Sized st)
    | _ ->
        Common.FatalError.fatal_error_msg
          [%message "Internal error: expected Tuple with TupleTransformation"]
  in
  let psts = tuple_psts pst in
  List.zip_exn psts tms

let zip_stuple_trans_exn pst trans =
  List.map (zip_tuple_trans_exn (Sized pst) trans) ~f:(fun (pst, trans) ->
      match pst with
      | Sized st -> (st, trans)
      | _ ->
          Common.FatalError.fatal_error_msg
            [%message "Internal error in zip_tuple"] )

let zip_utuple_trans_exn pst trans =
  List.map (zip_tuple_trans_exn (Unsized pst) trans) ~f:(fun (pst, trans) ->
      match pst with
      | Unsized ut -> (ut, trans)
      | _ ->
          Common.FatalError.fatal_error_msg
            [%message "Internal error in zip_tuple"] )

(* Copied from AST's version in AST.ml *)
let rec lvalue_of_expr_opt (expr : 'e Expr.Fixed.t) :
    'e Expr.Fixed.t Stmt.Fixed.Pattern.lvalue option =
  match expr.pattern with
  | Var s -> Some (LVariable s)
  | Indexed (l, i) ->
      Option.( >>= ) (lvalue_of_expr_opt l) (fun lv ->
          Some (Stmt.Fixed.Pattern.LIndexed (lv, i)) )
  | IndexedTuple (l, i) ->
      Option.( >>= ) (lvalue_of_expr_opt l) (fun lv ->
          Some (Stmt.Fixed.Pattern.LIndexedTuple (lv, i)) )
  | _ -> None

let rec expr_of_lvalue (lhs : 'e Expr.Fixed.t Stmt.Fixed.Pattern.lvalue)
    ~(meta : 'e) : 'e Expr.Fixed.t =
  let pattern =
    match lhs with
    | LVariable v -> Expr.Fixed.Pattern.Var v
    | LIndexed (lv, ix) -> Indexed (expr_of_lvalue ~meta lv, ix)
    | LIndexedTuple (lv, ix) -> IndexedTuple (expr_of_lvalue ~meta lv, ix) in
  {pattern; meta}

let rec map_lhs_variable ~(f : string -> string)
    (lhs : 'e Stmt.Fixed.Pattern.lvalue) : 'e Stmt.Fixed.Pattern.lvalue =
  match lhs with
  | LVariable v -> LVariable (f v)
  | LIndexed (lv, ix) -> LIndexed (map_lhs_variable ~f lv, ix)
  | LIndexedTuple (lv, ix) -> LIndexedTuple (map_lhs_variable ~f lv, ix)

let rec lhs_indices (lhs : 'e Stmt.Fixed.Pattern.lvalue) : 'e Index.t list =
  match lhs with
  | LVariable _ -> []
  | LIndexed (lv, idcs) -> idcs @ lhs_indices lv
  | LIndexedTuple (lv, _) -> lhs_indices lv

let rec lhs_variable (lhs : 'e Stmt.Fixed.Pattern.lvalue) : string =
  match lhs with
  | LVariable v -> v
  | LIndexed (lv, _) | LIndexedTuple (lv, _) -> lhs_variable lv

(* Reduce an lvalue down to its "base reference", which is a variable with maximum tuple indices after it.
   For example:
   x[1,2][3] -> x
   x.1[1,2].2[3].3 -> x.1
   x.1.2[1,2][3].3 -> x.1.2
*)
let lvalue_base_reference (lvalue : 'e Stmt.Fixed.Pattern.lvalue) =
  let rec go (lv : 'e Stmt.Fixed.Pattern.lvalue) wrap =
    match lv with
    | LVariable _ | LIndexedTuple (LVariable _, _) -> wrap lv
    | LIndexed (lv, _) -> go lv Fn.id
    | LIndexedTuple (lv, ix) -> go lv (fun lv -> wrap (LIndexedTuple (lv, ix)))
  in
  go lvalue Fn.id
