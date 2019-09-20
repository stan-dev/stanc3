open Core_kernel
open Middle

(** [for_scalar unsizedtype...] generates a For statement that loops
    over the scalars in the underlying [unsizedtype].

    We can call [bodyfn] directly on scalars, make a direct For loop
    around Eigen types, or for Arrays we call mkfor but inserting a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_scalar st bodyfn var smeta =
  match st with
  | SizedType.SInt | SReal -> bodyfn var
  | SVector d | SRowVector d -> Stmt.Helpers.mkfor d bodyfn var smeta
  | SMatrix (d1, d2) ->
      Stmt.Helpers.mkfor d1
        (fun e -> for_scalar (SRowVector d2) bodyfn e smeta)
        var smeta
  | SArray (t, d) ->
      Stmt.Helpers.mkfor d (fun e -> for_scalar t bodyfn e smeta) var smeta

(** Exactly like for_scalar, but iterating through array dimensions in the 
inverted order.*)
let for_scalar_inv st bodyfn (var : Expr.Typed.t) (smeta : Location_span.t) :
    Stmt.Located.t =
  let invert_index_order e =
    match Expr.Fixed.pattern_of e with
    | Indexed (obj, idxs) -> {e with pattern= Indexed (obj, List.rev idxs)}
    | _ -> e
  in
  let rec go st bodyfn var smeta =
    match st with
    | SizedType.SArray (t, d) ->
        let bodyfn' var = Stmt.Helpers.mkfor d bodyfn var smeta in
        go t bodyfn' var smeta
    | SMatrix (d1, d2) ->
        let bodyfn' var = Stmt.Helpers.mkfor d1 bodyfn var smeta in
        go (SRowVector d2) bodyfn' var smeta
    | _ -> for_scalar st bodyfn var smeta
  in
  go st (Fn.compose bodyfn invert_index_order) var smeta

(* let%expect_test "inverted for" =
  let int i = {expr= Lit (Int, string_of_int i); emeta= internal_meta} in
  let bodyfn var =
    {stmt= NRFunApp (StanLib, "print", [var]); smeta= no_span}
  in
  for_scalar_inv
    (SArray (SArray (SMatrix (int 2, int 3), int 5), int 4))
    bodyfn
    {expr= Var "hi"; emeta= {internal_meta with mtype= UArray (UArray UMatrix)}}
    no_span
  |> sexp_of_stmt_loc |> Sexp.to_string_hum |> print_endline ;
  [%expect
    {|
    (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 3))
     (body
      (Block
       ((For (loopvar sym2__) (lower (Lit Int 1)) (upper (Lit Int 2))
         (body
          (Block
           ((For (loopvar sym3__) (lower (Lit Int 1)) (upper (Lit Int 5))
             (body
              (Block
               ((For (loopvar sym4__) (lower (Lit Int 1)) (upper (Lit Int 4))
                 (body
                  (Block
                   ((NRFunApp StanLib print
                     ((Indexed (Var hi)
                       ((Single (Var sym4__)) (Single (Var sym3__))
                        (Single (Var sym2__)) (Single (Var sym1__)))))))))))))))))))))) |}] *)

(** [for_eigen unsizedtype...] generates a For statement that loops
    over the eigen types in the underlying [unsizedtype]; i.e. just iterating
    overarrays and running bodyfn on any eign types found within.

    We can call [bodyfn] directly on scalars and Eigen types;
    for Arrays we call mkfor but insert a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_eigen st bodyfn var smeta =
  match st with
  | SizedType.SInt | SReal | SVector _ | SRowVector _ | SMatrix _ -> bodyfn var
  | SArray (t, d) ->
      Stmt.Helpers.mkfor d (fun e -> for_eigen t bodyfn e smeta) var smeta

let rec pull_indices expr =
  match Expr.Fixed.pattern_of expr with
  | Indexed (obj, indices) -> pull_indices obj @ indices
  | _ -> []

let assign_indexed decl_type vident smeta varfn var =
  let indices = pull_indices var in
  let pattern =
    Stmt.Fixed.Pattern.Assignment ((vident, decl_type, indices), varfn var)
  in
  Stmt.Fixed.fix (smeta, pattern)

let rec eigen_size st =
  (* (st : mtype_loc_ad with_expr sizedtype) = *)
  match st with
  | SizedType.SArray (t, _) -> eigen_size t
  | SMatrix (d1, d2) -> [d1; d2]
  | SRowVector dim | SVector dim -> [dim]
  | SInt | SReal -> []
