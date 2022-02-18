open Core_kernel
open Middle
open Frontend
open Ast

let rec transpose = function
  | [] :: _ -> []
  | rows ->
      let hd = List.map ~f:List.hd_exn rows in
      let tl = List.map ~f:List.tl_exn rows in
      hd :: transpose tl

let dotproduct xs ys =
  List.fold2_exn xs ys ~init:0. ~f:(fun accum x y -> accum +. (x *. y))

let matprod x y =
  let y_T = transpose y in
  if List.length x <> List.length y_T then
    Common.FatalError.fatal_error_msg
      [%message "Matrix multiplication dim. mismatch"]
  else List.map ~f:(fun row -> List.map ~f:(dotproduct row) y_T) x

let rec vect_to_mat l m =
  let len = List.length l in
  if len % m <> 0 then
    Common.FatalError.fatal_error_msg
      [%message "The length has to be a whole multiple of the partition size"]
  else if len = m then [l]
  else
    let hd, tl = List.split_n l m in
    hd :: vect_to_mat tl m

let unwrap_num_exn m e =
  let e = Ast_to_Mir.trans_expr e in
  let m = Map.Poly.map m ~f:Ast_to_Mir.trans_expr in
  let e = Mir_utils.subst_expr m e in
  let e = Partial_evaluator.eval_expr e in
  let rec strip_promotions (e : Middle.Expr.Typed.t) =
    match e.pattern with Promotion (e, _, _) -> strip_promotions e | _ -> e
  in
  let e = strip_promotions e in
  match e.pattern with
  | Lit (_, s) -> Float.of_string s
  | _ ->
      Common.FatalError.fatal_error_msg
        [%message "Cannot convert size to number."]

let unwrap_int_exn m e = Int.of_float (unwrap_num_exn m e)

let gen_num_int m t =
  let def_low, diff = (2, 4) in
  let low, up =
    match t with
    | Transformation.Lower e -> (unwrap_int_exn m e, unwrap_int_exn m e + diff)
    | Upper e -> (unwrap_int_exn m e - diff, unwrap_int_exn m e)
    | LowerUpper (e1, e2) -> (unwrap_int_exn m e1, unwrap_int_exn m e2)
    | _ -> (def_low, def_low + diff) in
  let low = if low = 0 && up <> 1 then low + 1 else low in
  Random.int (up - low + 1) + low

let gen_num_real m t =
  let def_low, diff = (2., 5.) in
  let low, up =
    match t with
    | Transformation.Lower e -> (unwrap_num_exn m e, unwrap_num_exn m e +. diff)
    | Upper e -> (unwrap_num_exn m e -. diff, unwrap_num_exn m e)
    | LowerUpper (e1, e2) -> (unwrap_num_exn m e1, unwrap_num_exn m e2)
    | _ -> (def_low, def_low +. diff) in
  Random.float_range low up

let rec repeat n e =
  match n with n when n <= 0 -> [] | m -> e :: repeat (m - 1) e

let rec repeat_th n f =
  match n with n when n <= 0 -> [] | m -> f () :: repeat_th (m - 1) f

let wrap_int n =
  { expr= IntNumeral (Int.to_string n)
  ; emeta= {loc= Location_span.empty; ad_level= DataOnly; type_= UInt} }

let int_two = wrap_int 2

let wrap_real r =
  { expr= RealNumeral (Float.to_string r)
  ; emeta= {loc= Location_span.empty; ad_level= DataOnly; type_= UReal} }

let wrap_row_vector l =
  { expr= RowVectorExpr l
  ; emeta= {loc= Location_span.empty; ad_level= DataOnly; type_= URowVector} }

let wrap_vector l =
  { expr= PostfixOp (wrap_row_vector l, Transpose)
  ; emeta= {loc= Location_span.empty; ad_level= DataOnly; type_= UVector} }

let gen_int m t = wrap_int (gen_num_int m t)
let gen_real m t = wrap_real (gen_num_real m t)

let gen_row_vector m n t =
  let extract_var e =
    match e with {expr= Variable x; _} -> Map.find_exn m x.name | _ -> e in
  let gen_bounded t e =
    match e with
    | {expr= RowVectorExpr unpacked_e; _}
     |{expr= ArrayExpr unpacked_e; _}
     |{expr= PostfixOp ({expr= RowVectorExpr unpacked_e; _}, Transpose); _} ->
        wrap_row_vector (List.map ~f:(fun x -> gen_real m (t x)) unpacked_e)
    | _ ->
        Common.FatalError.fatal_error_msg
          [%message
            "Bad bounded (upper OR lower) expr: "
              (e : (typed_expr_meta, fun_kind) expr_with)] in
  let gen_ul_bounded e1 e2 =
    let create_bounds l u =
      wrap_row_vector
        (List.map2_exn
           ~f:(fun x y -> gen_real m (Transformation.LowerUpper (x, y)))
           l u ) in
    match (e1, e2) with
    | ( ( {expr= RowVectorExpr unpacked_e1 | ArrayExpr unpacked_e1; _}
        | {expr= PostfixOp ({expr= RowVectorExpr unpacked_e1; _}, Transpose); _}
          )
      , ( {expr= RowVectorExpr unpacked_e2 | ArrayExpr unpacked_e2; _}
        | {expr= PostfixOp ({expr= RowVectorExpr unpacked_e2; _}, Transpose); _}
          ) ) ->
        (* | {expr= ArrayExpr unpacked_e1; _}, {expr= ArrayExpr unpacked_e2; _} -> *)
        create_bounds unpacked_e1 unpacked_e2
    | ( ({expr= RealNumeral _; _} | {expr= IntNumeral _; _})
      , ( {expr= RowVectorExpr unpacked_e2; _}
        | {expr= ArrayExpr unpacked_e2; _}
        | {expr= PostfixOp ({expr= RowVectorExpr unpacked_e2; _}, Transpose); _}
          ) ) ->
        create_bounds
          (List.init (List.length unpacked_e2) ~f:(fun _ -> e1))
          unpacked_e2
    | ( ( {expr= RowVectorExpr unpacked_e1; _}
        | {expr= PostfixOp ({expr= RowVectorExpr unpacked_e1; _}, Transpose); _}
        | {expr= ArrayExpr unpacked_e1; _} )
      , ({expr= RealNumeral _; _} | {expr= IntNumeral _; _}) ) ->
        create_bounds unpacked_e1
          (List.init (List.length unpacked_e1) ~f:(fun _ -> e2))
    | _ ->
        Common.FatalError.fatal_error_msg
          [%message
            "Bad bounded upper and lower expr: "
              (e1 : (typed_expr_meta, fun_kind) expr_with)
              " and "
              (e2 : (typed_expr_meta, fun_kind) expr_with)] in
  match t with
  | Transformation.Lower ({emeta= {type_= UVector | URowVector; _}; _} as e) ->
      gen_bounded (fun x -> Transformation.Lower x) (extract_var e)
  | Transformation.Upper ({emeta= {type_= UVector | URowVector; _}; _} as e) ->
      gen_bounded (fun x -> Transformation.Upper x) (extract_var e)
  | Transformation.LowerUpper
      ( ({emeta= {type_= UVector | URowVector | UReal | UInt; _}; _} as e1)
      , ({emeta= {type_= UVector | URowVector; _}; _} as e2) )
   |Transformation.LowerUpper
      ( ({emeta= {type_= UVector | URowVector; _}; _} as e1)
      , ({emeta= {type_= UReal | UInt; _}; _} as e2) ) ->
      gen_ul_bounded (extract_var e1) (extract_var e2)
  | _ ->
      { expr= RowVectorExpr (repeat_th n (fun _ -> gen_real m t))
      ; emeta= {loc= Location_span.empty; ad_level= DataOnly; type_= UMatrix} }

let gen_vector m n t =
  let gen_ordered n =
    let l = repeat_th n (fun _ -> Random.float 1.) in
    let l =
      List.fold (List.tl_exn l) ~init:[List.hd_exn l] ~f:(fun accum elt ->
          (Float.exp elt +. List.hd_exn accum) :: accum ) in
    l in
  match t with
  | Transformation.Simplex ->
      let l = repeat_th n (fun _ -> Random.float 1.) in
      let sum = List.fold l ~init:0. ~f:(fun accum elt -> accum +. elt) in
      let l = List.map l ~f:(fun x -> x /. sum) in
      wrap_vector (List.map ~f:wrap_real l)
  | Ordered ->
      let l = gen_ordered n in
      let halfmax =
        Option.value_exn (List.max_elt l ~compare:compare_float) /. 2. in
      let l = List.map l ~f:(fun x -> (x -. halfmax) /. halfmax) in
      wrap_vector (List.map ~f:wrap_real l)
  | PositiveOrdered ->
      let l = gen_ordered n in
      let max = Option.value_exn (List.max_elt l ~compare:compare_float) in
      let l = List.map l ~f:(fun x -> x /. max) in
      wrap_vector (List.map ~f:wrap_real l)
  | UnitVector ->
      let l = repeat_th n (fun _ -> Random.float 1.) in
      let sum =
        Float.sqrt
          (List.fold l ~init:0. ~f:(fun accum elt -> accum +. (elt ** 2.)))
      in
      let l = List.map l ~f:(fun x -> x /. sum) in
      wrap_vector (List.map ~f:wrap_real l)
  | _ -> {int_two with expr= PostfixOp (gen_row_vector m n t, Transpose)}

let gen_cov_unwrapped n =
  let l = repeat_th (n * n) (fun _ -> Random.float 2.) in
  let l_mat = vect_to_mat l n in
  matprod l_mat (transpose l_mat)

let wrap_real_mat m =
  let mat_wrapped =
    List.map ~f:wrap_row_vector
      (List.map ~f:(fun x -> List.map ~f:wrap_real x) m) in
  {int_two with expr= RowVectorExpr mat_wrapped}

let gen_diag_mat l =
  let n = List.length l in
  List.map
    (List.range 1 (n + 1))
    ~f:(fun k ->
      repeat (min (k - 1) n) 0.
      @ (if k <= n then [List.nth_exn l (k - 1)] else [])
      @ repeat (n - k) 0. )

let fill_lower_triangular m =
  let fill_row i l =
    let _, tl = List.split_n l i in
    List.init ~f:(fun _ -> Random.float 2.) i @ tl in
  List.mapi ~f:fill_row m

let pad_mat mm m n =
  let padding_mat =
    List.init (m - n) ~f:(fun _ -> List.init n ~f:(fun _ -> Random.float 2.))
  in
  wrap_real_mat (mm @ padding_mat)

let gen_cov_cholesky m n =
  let diag_mat = gen_diag_mat (List.init ~f:(fun _ -> Random.float 2.) n) in
  let filled_mat = fill_lower_triangular diag_mat in
  if m <= n then wrap_real_mat filled_mat else pad_mat filled_mat m n

let gen_corr_cholesky_unwrapped n =
  let diag_mat = gen_diag_mat (List.init ~f:(fun _ -> Random.float 2.) n) in
  let filled_mat = fill_lower_triangular diag_mat in
  let row_normalizer l =
    let row_norm =
      Float.sqrt (List.fold ~init:0. ~f:(fun accum x -> accum +. (x *. x)) l)
    in
    List.map ~f:(fun x -> x /. row_norm) l in
  List.map ~f:row_normalizer filled_mat

let gen_corr_cholesky n = wrap_real_mat (gen_corr_cholesky_unwrapped n)

(* let gen_identity_matrix m n =
   let id_mat = gen_diag_mat (List.init ~f:(fun _ -> 1.) n) in
   if m <= n then wrap_real_mat id_mat else pad_mat id_mat m n *)

let gen_cov_matrix n =
  let cov = gen_cov_unwrapped n in
  wrap_real_mat cov

let gen_corr_matrix n =
  let corr_chol = gen_corr_cholesky_unwrapped n in
  wrap_real_mat (matprod corr_chol (transpose corr_chol))

let gen_matrix mm m n t =
  match t with
  | Transformation.Covariance -> gen_cov_matrix m
  | Correlation -> gen_corr_matrix m
  | CholeskyCov -> gen_cov_cholesky m n
  | CholeskyCorr -> gen_corr_cholesky m
  | _ ->
      { int_two with
        expr= RowVectorExpr (repeat_th m (fun () -> gen_row_vector mm n t)) }

(* TODO: do some proper random generation of these special matrices *)

let gen_array elt n _ = {int_two with expr= ArrayExpr (repeat_th n elt)}

let rec generate_value m st t =
  match st with
  | SizedType.SInt -> gen_int m t
  | SReal -> gen_real m t
  | SComplex ->
      (* when serialzied, a complex number looks just like a 2-array of reals *)
      generate_value m (SArray (SReal, wrap_int 2)) t
  | SVector (_, e) -> gen_vector m (unwrap_int_exn m e) t
  | SRowVector (_, e) -> gen_row_vector m (unwrap_int_exn m e) t
  | SMatrix (_, e1, e2) ->
      gen_matrix m (unwrap_int_exn m e1) (unwrap_int_exn m e2) t
  | SArray (st, e) ->
      let element () = generate_value m st t in
      gen_array element (unwrap_int_exn m e) t

let rec pp_value_json ppf e =
  match e.expr with
  | PostfixOp (e, Transpose) -> pp_value_json ppf e
  | IntNumeral s | RealNumeral s -> Fmt.string ppf s
  | ArrayExpr l | RowVectorExpr l ->
      Fmt.(pf ppf "[@[<hov 1>%a@]]" (list ~sep:comma pp_value_json) l)
  | _ ->
      Common.FatalError.fatal_error_msg
        [%message "Could not evaluate expression " (e : typed_expression)]

let print_data_prog s =
  let data = Ast.get_stmts s.datablock in
  let l, _ =
    List.fold data ~init:([], Map.Poly.empty) ~f:(fun (l, m) decl ->
        match decl.stmt with
        | VarDecl
            { decl_type= Sized sizedtype
            ; transformation
            ; identifier= {name; _}
            ; _ } ->
            let value = generate_value m sizedtype transformation in
            ((name, value) :: l, Map.set m ~key:name ~data:value)
        | _ -> (l, m) ) in
  let pp ppf (id, value) =
    Fmt.pf ppf {|@[<hov 2>"%s":@ %a@]|} id pp_value_json value in
  Fmt.(str "{@ @[<hov>%a@]@ }" (list ~sep:comma pp) (List.rev l))
