include Mir
include Stan_math_signatures
include Type_conversion
include Mir_utils
open Core_kernel
module Validation = Validation
module Pretty = Mir_pretty_printer
module Utils = Utils

(* -- Locations and spans --------------------------------------------------- *)

(** Render a location as a string *)
let rec string_of_location ?(print_file = true) ?(print_line = true) loc =
  let open Format in
  let file = if print_file then sprintf "'%s', " loc.filename else "" in
  let line = if print_line then sprintf "line %d, " loc.line_num else "" in
  let incl =
    match loc.included_from with
    | Some loc2 -> sprintf ", included from\n%s" (string_of_location loc2)
    | None -> ""
  in
  sprintf "%s%scolumn %d%s" file line loc.col_num incl

(** Render a location_span as a string *)
let string_of_location_span {begin_loc; end_loc} =
  let end_loc_str =
    match begin_loc.included_from with
    | None ->
        " to "
        ^ string_of_location
            ~print_file:(begin_loc.filename <> end_loc.filename)
            ~print_line:(begin_loc.line_num <> end_loc.line_num)
            end_loc
    | Some _ -> ""
  in
  string_of_location begin_loc ^ end_loc_str

let merge_spans left right = {begin_loc= left.begin_loc; end_loc= right.end_loc}

(*-- mutable counter for symbol names --*)
let _counter = ref 0

let gensym () =
  _counter := !_counter + 1 ;
  sprintf "sym%d__" !_counter

let gensym_enter () =
  let old_counter = !_counter in
  (gensym (), fun () -> _counter := old_counter)

let gensym_reset_danger_use_cautiously () = _counter := 0

(** Querying stan_math_signatures for operator signatures by string name *)
let operator_return_type_from_string op_name argtypes =
  if op_name = "Assign" || op_name = "ArrowAssign" then
    match List.map ~f:snd argtypes with
    | [ut1; ut2] when check_of_same_type_mod_array_conv "" ut1 ut2 -> Some Void
    | _ -> None
  else
    Map.Poly.find_multi string_of_operators op_name
    |> List.find_map ~f:(fun name -> stan_math_returntype name argtypes)

let operator_return_type op =
  operator_return_type_from_string (string_of_operator op)

let rec sexp_of_expr_typed_located {expr; _} =
  sexp_of_expr sexp_of_expr_typed_located expr

let rec sexp_of_stmt_loc {stmt; _} =
  sexp_of_statement sexp_of_expr_typed_located sexp_of_stmt_loc stmt

let rec expr_contains_fn fname accum e =
  accum
  ||
  match e.expr with
  | FunApp (_, name, _) when name = fname -> true
  | x -> fold_expr (expr_contains_fn fname) accum x

let%test "expr contains fn" =
  internal_funapp FnReadData [] ()
  |> expr_contains_fn (string_of_internal_fn FnReadData) false

let contains_fn fname s =
  let rec contains_fn_go fname accum {stmt; _} =
    match stmt with
    | NRFunApp (_, fname', _) when fname' = fname -> true
    | _ ->
        fold_statement (expr_contains_fn fname) (contains_fn_go fname) accum
          stmt
  in
  contains_fn_go fname false s

let rec is_indexing_matrix = function
  | UArray t, _ :: idcs -> is_indexing_matrix (t, idcs)
  | UMatrix, [] -> false
  | UMatrix, _ -> true
  | _ -> false

let mock_stmt stmt = {stmt; smeta= no_span}
let mir_int i = {expr= Lit (Int, string_of_int i); emeta= internal_meta}

let mock_for i body =
  For
    { loopvar= "lv"
    ; lower= mir_int 0
    ; upper= mir_int i
    ; body= mock_stmt (Block [body]) }
  |> mock_stmt

let%test "contains fn" =
  let f =
    mock_for 8
      (mock_for 9
         (mock_stmt
            (Assignment
               (("v", UInt, []), internal_funapp FnReadData [] internal_meta))))
  in
  contains_fn
    (string_of_internal_fn FnReadData)
    (mock_stmt (Block [f; mock_stmt Break]))

let%test "contains nrfn" =
  let f =
    mock_for 8
      (mock_for 9
         (mock_stmt
            (NRFunApp (CompilerInternal, string_of_internal_fn FnWriteParam, []))))
  in
  contains_fn
    (string_of_internal_fn FnWriteParam)
    (mock_stmt
       (Block
          [ mock_stmt
              (NRFunApp
                 (CompilerInternal, string_of_internal_fn FnWriteParam, []))
          ; f ]))

let rec infer_type_of_indexed ut indices =
  match (ut, indices) with
  | _, [] -> ut
  | _, [All] | _, [Upfrom _] | _, [Between _] -> ut
  | UMatrix, [All; Single _]
   |UMatrix, [Upfrom _; Single _]
   |UMatrix, [Between _; Single _]
   |UMatrix, [MultiIndex _]
   |UMatrix, [Single _] ->
      UVector
  | UArray t, Single _ :: tl -> infer_type_of_indexed t tl
  | UArray t, _ :: tl -> UArray (infer_type_of_indexed t tl)
  | UMatrix, [Single _; Single _] | UVector, [_] | URowVector, [_] -> UReal
  | _ -> raise_s [%message "Can't index" (ut : unsizedtype)]

let%expect_test "infer type of indexed" =
  [ (UArray UMatrix, [Single loop_bottom; Single loop_bottom])
  ; (UArray (UArray UMatrix), [Single loop_bottom])
  ; (UArray UMatrix, [Single loop_bottom])
  ; (UArray UMatrix, [Upfrom loop_bottom; Single loop_bottom])
  ; ( UArray UMatrix
    , [Single loop_bottom; Single loop_bottom; Single loop_bottom] )
  ; ( UArray UMatrix
    , [Upfrom loop_bottom; Single loop_bottom; Single loop_bottom] ) ]
  |> List.map ~f:(fun (ut, idx) -> infer_type_of_indexed ut idx)
  |> Fmt.(strf "@[<hov>%a@]" (list ~sep:comma Pretty.pp_unsizedtype))
  |> print_endline ;
  [%expect {|
    vector, matrix[], matrix, vector[], real, real[] |}]

(** [add_index expression index] returns an expression that (additionally)
  *  indexes into the input [expression] by [index].
  * e: mytype_loc_ad with_expr
  * i: mtype_loc_ad with_expr index
  *)
let add_int_index e i =
  let mtype = infer_type_of_indexed e.emeta.mtype [i] in
  let expr =
    match e.expr with
    | Var _ -> Indexed (e, [i])
    | Indexed (e, indices) -> Indexed (e, indices @ [i])
    | _ -> raise_s [%message "These should go away with Ryan's LHS"]
  in
  {expr; emeta= {e.emeta with mtype}}

let%expect_test "adding integer index" =
  let idx s =
    Single
      {expr= Var s; emeta= {mtype= UInt; mloc= no_span; madlevel= DataOnly}}
  in
  let decl_var =
    { expr= Var "test_val"
    ; emeta= {mloc= no_span; mtype= UArray UInt; madlevel= DataOnly} }
  in
  Fmt.(
    strf "@[%a@]" Pretty.pp_expr_typed_located
      (add_int_index decl_var (idx "foo")))
  |> print_endline ;
  [%expect {| test_val[foo] |}]

(** [mkfor] returns a MIR For statement that iterates over the given expression
    [iteratee]. *)
let mkfor upper bodyfn iteratee smeta =
  let idx s =
    Single {expr= Var s; emeta= {mtype= UInt; mloc= smeta; madlevel= DataOnly}}
  in
  let loopvar, reset = gensym_enter () in
  let lower = loop_bottom in
  let stmt = Block [bodyfn (add_int_index iteratee (idx loopvar))] in
  reset () ;
  {stmt= For {loopvar; lower; upper; body= {stmt; smeta}}; smeta}

let%expect_test "making vector for loop" =
  let bodyfn var =
    {stmt= NRFunApp (StanLib, "print", [var]); smeta= no_span}
  in
  let var_test =
    {expr= Var "hi"; emeta= {internal_meta with mtype= UVector}}
  in
  let int i = {expr= Lit (Int, string_of_int i); emeta= internal_meta} in
  let dim_test = int 1 in
  mkfor dim_test bodyfn var_test no_span
  |> sexp_of_stmt_loc |> Sexp.to_string_hum |> print_endline ;
  [%expect
    {|
    (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 1))
     (body
      (Block
       ((NRFunApp StanLib print ((Indexed (Var hi) ((Single (Var sym1__)))))))))) |}]

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
  | SInt | SReal -> bodyfn var
  | SVector d | SRowVector d -> mkfor d bodyfn var smeta
  | SMatrix (d1, d2) ->
      mkfor d1 (fun e -> for_scalar (SRowVector d2) bodyfn e smeta) var smeta
  | SArray (t, d) -> mkfor d (fun e -> for_scalar t bodyfn e smeta) var smeta

(* Exactly like for_scalar, but iterating through array dimensions in the inverted order.*)
let for_scalar_inv st bodyfn var smeta =
  let invert_index_order = function
    | {expr= Indexed (obj, indices); emeta} ->
        {expr= Indexed (obj, List.rev indices); emeta}
    | e -> e
  in
  let rec go st bodyfn var smeta =
    match st with
    | SArray (t, d) ->
        let bodyfn' var = mkfor d bodyfn var smeta in
        go t bodyfn' var smeta
    | SMatrix (d1, d2) ->
        let bodyfn' var = mkfor d1 bodyfn var smeta in
        go (SRowVector d2) bodyfn' var smeta
    | _ -> for_scalar st bodyfn var smeta
  in
  go st (Fn.compose bodyfn invert_index_order) var smeta

let%expect_test "inverted for" =
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
                        (Single (Var sym2__)) (Single (Var sym1__)))))))))))))))))))))) |}]

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
  | SInt | SReal | SVector _ | SRowVector _ | SMatrix _ -> bodyfn var
  | SArray (t, d) -> mkfor d (fun e -> for_eigen t bodyfn e smeta) var smeta

let rec pull_indices {expr; _} =
  match expr with
  | Indexed (obj, indices) -> pull_indices obj @ indices
  | _ -> []

let assign_indexed decl_type vident smeta varfn var =
  let indices = pull_indices var in
  {stmt= Assignment ((vident, decl_type, indices), varfn var); smeta}

let rec eigen_size (st : mtype_loc_ad with_expr sizedtype) =
  match st with
  | SArray (t, _) -> eigen_size t
  | SMatrix (d1, d2) -> [d1; d2]
  | SRowVector dim | SVector dim -> [dim]
  | SInt | SReal -> []
