open Core_kernel

let option_or_else ~if_none x = Option.first_some x if_none

(* Name mangling helper functions for distributions *)
let unnormalized_suffices = ["_lupdf"; "_lupmf"]

(* _log is listed last so that it only gets picked up if no other implementation exists *)
let distribution_suffices = ["_lpmf"; "_lpdf"; "_log"]

let conditioning_suffices =
  ["_lpdf"; "_lupdf"; "_lupmf"; "_lpmf"; "_lcdf"; "_lccdf"]

let conditioning_suffices_w_log = conditioning_suffices @ ["_log"]
let is_user_ident = Fn.non (String.is_suffix ~suffix:"__")

let unnormalized_suffix = function
  | "_lpdf" -> "_lupdf"
  | "_lpmf" -> "_lupmf"
  | x -> x

let is_distribution_name s =
  (not
     ( String.is_suffix s ~suffix:"_cdf_log"
     || String.is_suffix s ~suffix:"_ccdf_log" ))
  && List.exists
       ~f:(fun suffix -> String.is_suffix s ~suffix)
       (distribution_suffices @ unnormalized_suffices)

let is_unnormalized_distribution s =
  List.exists
    ~f:(fun suffix -> String.is_suffix s ~suffix)
    unnormalized_suffices

let with_unnormalized_suffix (name : string) =
  Option.merge
    ~f:(fun x _ -> x)
    ( String.chop_suffix ~suffix:"_lpdf" name
    |> Option.map ~f:(fun n -> n ^ "_lupdf") )
    ( String.chop_suffix ~suffix:"_lpmf" name
    |> Option.map ~f:(fun n -> n ^ "_lupmf") )

let replace_unnormalized_suffix suffix ~name =
  name
  |> String.chop_suffix ~suffix:(unnormalized_suffix suffix)
  |> Option.map ~f:(fun x -> x ^ suffix)

let stdlib_distribution_name s =
  List.map ~f:(replace_unnormalized_suffix ~name:s) distribution_suffices
  |> List.filter_opt |> List.hd |> Option.value ~default:s

let normalized_name name =
  match name with
  | x when is_distribution_name x -> stdlib_distribution_name x
  | x -> x

let%expect_test "unnormalized name mangling" =
  stdlib_distribution_name "bernoulli_logit_lupmf" |> print_string ;
  stdlib_distribution_name "normal_lupdf" |> ( ^ ) "; " |> print_string ;
  stdlib_distribution_name "normal_lpdf" |> ( ^ ) "; " |> print_string ;
  stdlib_distribution_name "normal" |> ( ^ ) "; " |> print_string ;
  [%expect {| bernoulli_logit_lpmf; normal_lpdf; normal_lpdf; normal |}]

let all_but_last_n l n =
  List.fold_right l ~init:([], n) ~f:(fun ele (accum, n) ->
      if n = 0 then (ele :: accum, n) else (accum, n - 1) )
  |> fst

let%expect_test "all but last n" =
  let l = all_but_last_n [1; 2; 3; 4] 2 in
  print_s [%sexp (l : int list)] ;
  [%expect {| (1 2) |}]

let zip_tuple_trans_exn pst trans =
  let tms =
    match trans with
    | Program.TupleTransformation tms -> tms
    | _ ->
        raise_s
          [%message "Internal error: expected TupleTransformation with Tuple"]
  in
  let rec tuple_psts pst =
    match pst with
    | Type.Unsized (UTuple uts) -> List.map ~f:(fun ut -> Type.Unsized ut) uts
    | Type.Unsized (UArray ut) -> tuple_psts (Type.Unsized ut)
    | Type.Sized (STuple sts) -> List.map ~f:(fun st -> Type.Sized st) sts
    | Type.Sized (SArray (st, _)) -> tuple_psts (Type.Sized st)
    | _ ->
        raise_s
          [%message "Internal error: expected Tuple with TupleTransformation"]
  in
  let psts = tuple_psts pst in
  Option.value_exn
    ~message:
      "Internal representation error: TupleTransformation not same length as \
       Tuple"
    (List.zip psts tms)

let zip_stuple_trans_exn pst trans =
  List.map (zip_tuple_trans_exn (Sized pst) trans) ~f:(fun (pst, trans) ->
      match pst with
      | Sized st -> (st, trans)
      | _ -> raise_s [%message "Internal error in zip_tuple"] )

let zip_utuple_trans_exn pst trans =
  List.map (zip_tuple_trans_exn (Unsized pst) trans) ~f:(fun (pst, trans) ->
      match pst with
      | Unsized ut -> (ut, trans)
      | _ -> raise_s [%message "Internal error in zip_tuple"] )

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
    | LIndexedTuple (lv, ix) -> IndexedTuple (expr_of_lvalue ~meta lv, ix)
  in
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
