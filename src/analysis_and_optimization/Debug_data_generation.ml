open Core_kernel
open Middle

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

let eval_expr m e =
  let e = Mir_utils.subst_expr m e in
  let e = Partial_evaluator.eval_expr e in
  let rec strip_promotions (e : Middle.Expr.Typed.t) =
    match e.pattern with Promotion (e, _, _) -> strip_promotions e | _ -> e
  in
  strip_promotions e

let unwrap_num_exn m e =
  let e = eval_expr m e in
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

let gen_bounded m gen e =
  match Expr.Helpers.try_unpack (eval_expr m e) with
  | Some unpacked_e -> List.map ~f:gen unpacked_e
  | None ->
      Common.FatalError.fatal_error_msg
        [%message "Bad bounded (upper OR lower) expr: " (e : Expr.Typed.t)]

let gen_ul_bounded m gen e1 e2 =
  let create_bounds l u =
    List.map2_exn ~f:(fun x y -> gen (Transformation.LowerUpper (x, y))) l u
  in
  let e1, e2 = (eval_expr m e1, eval_expr m e2) in
  match Expr.Helpers.(try_unpack e1, try_unpack e2) with
  | Some unpacked_e1, Some unpacked_e2 -> create_bounds unpacked_e1 unpacked_e2
  | None, Some unpacked_e2 ->
      create_bounds
        (List.init (List.length unpacked_e2) ~f:(fun _ -> e1))
        unpacked_e2
  | Some unpacked_e1, None ->
      create_bounds unpacked_e1
        (List.init (List.length unpacked_e1) ~f:(fun _ -> e2))
  | _ ->
      Common.FatalError.fatal_error_msg
        [%message
          "Bad bounded upper and lower expr: "
            (e1 : Expr.Typed.t)
            " and "
            (e2 : Expr.Typed.t)]

let gen_row_vector m n t =
  match (t : Expr.Typed.t Transformation.t) with
  | Transformation.Lower ({meta= {type_= UVector | URowVector; _}; _} as e) ->
      gen_bounded m (fun x -> gen_num_real m (Transformation.Lower x)) e
      |> Expr.Helpers.row_vector
  | Transformation.Upper ({meta= {type_= UVector | URowVector; _}; _} as e) ->
      gen_bounded m (fun x -> gen_num_real m (Transformation.Upper x)) e
      |> Expr.Helpers.row_vector
  | Transformation.LowerUpper
      ( ({meta= {type_= UVector | URowVector | UReal | UInt; _}; _} as e1)
      , ({meta= {type_= UVector | URowVector; _}; _} as e2) )
   |Transformation.LowerUpper
      ( ({meta= {type_= UVector | URowVector; _}; _} as e1)
      , ({meta= {type_= UReal | UInt; _}; _} as e2) ) ->
      gen_ul_bounded m (gen_num_real m) e1 e2 |> Expr.Helpers.row_vector
  | _ -> Expr.Helpers.row_vector (repeat_th n (fun _ -> gen_num_real m t))

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
      Expr.Helpers.vector l
  | Ordered ->
      let l = gen_ordered n in
      let halfmax =
        Option.value_exn (List.max_elt l ~compare:compare_float) /. 2. in
      let l = List.map l ~f:(fun x -> (x -. halfmax) /. halfmax) in
      Expr.Helpers.vector l
  | PositiveOrdered ->
      let l = gen_ordered n in
      let max = Option.value_exn (List.max_elt l ~compare:compare_float) in
      let l = List.map l ~f:(fun x -> x /. max) in
      Expr.Helpers.vector l
  | UnitVector ->
      let l = repeat_th n (fun _ -> Random.float 1.) in
      let sum =
        Float.sqrt
          (List.fold l ~init:0. ~f:(fun accum elt -> accum +. (elt ** 2.)))
      in
      let l = List.map l ~f:(fun x -> x /. sum) in
      Expr.Helpers.vector l
  | _ ->
      let v = Expr.Helpers.unary_op Transpose (gen_row_vector m n t) in
      {v with meta= {v.meta with type_= UVector}}

let gen_cov_unwrapped n =
  let l = repeat_th (n * n) (fun _ -> Random.float 2.) in
  let l_mat = vect_to_mat l n in
  matprod l_mat (transpose l_mat)

let wrap_real_mat m = Expr.Helpers.matrix m

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
  match (t : Expr.Typed.t Transformation.t) with
  | Covariance -> gen_cov_matrix m
  | Correlation -> gen_corr_matrix m
  | CholeskyCov -> gen_cov_cholesky m n
  | CholeskyCorr -> gen_corr_cholesky m
  | Lower ({meta= {type_= UMatrix; _}; _} as e) ->
      Expr.Helpers.matrix_from_rows
        (gen_bounded mm (fun x -> gen_row_vector mm n (Lower x)) e)
  | Upper ({meta= {type_= UMatrix; _}; _} as e) ->
      Expr.Helpers.matrix_from_rows
        (gen_bounded mm (fun x -> gen_row_vector mm n (Upper x)) e)
  | LowerUpper (({meta= {type_= UMatrix; _}; _} as e1), e2)
   |LowerUpper (e1, ({meta= {type_= UMatrix; _}; _} as e2)) ->
      Expr.Helpers.matrix_from_rows
        (gen_ul_bounded mm (gen_row_vector mm n) e1 e2)
  | _ ->
      Expr.Helpers.matrix_from_rows
        (repeat_th m (fun () -> gen_row_vector mm n t))

let rec gen_array m st n t =
  let elt () = generate_value m st t in
  match (t : Expr.Typed.t Transformation.t) with
  | Lower ({meta= {type_= UArray _; _}; _} as e) ->
      Expr.Helpers.array_expr
        (gen_bounded m (fun x -> generate_value m st (Lower x)) e)
  | Upper ({meta= {type_= UArray _; _}; _} as e) ->
      Expr.Helpers.array_expr
        (gen_bounded m (fun x -> generate_value m st (Upper x)) e)
  | LowerUpper (({meta= {type_= UArray _; _}; _} as e1), e2)
   |LowerUpper (e1, ({meta= {type_= UArray _; _}; _} as e2)) ->
      Expr.Helpers.array_expr (gen_ul_bounded m (generate_value m st) e1 e2)
  | _ -> Expr.Helpers.array_expr (repeat_th n elt)

and generate_value m st t =
  match st with
  | SizedType.SInt -> Expr.Helpers.int (gen_num_int m t)
  | SReal -> Expr.Helpers.float (gen_num_real m t)
  | SComplex ->
      (* when serialzied, a complex number looks just like a 2-array of reals *)
      generate_value m (SArray (SReal, Expr.Helpers.int 2)) t
  | SVector (_, e) -> gen_vector m (unwrap_int_exn m e) t
  | SRowVector (_, e) -> gen_row_vector m (unwrap_int_exn m e) t
  | SMatrix (_, e1, e2) ->
      gen_matrix m (unwrap_int_exn m e1) (unwrap_int_exn m e2) t
  | SArray (st, e) -> gen_array m st (unwrap_int_exn m e) t

let rec pp_value_json ppf e =
  match e.Expr.Fixed.pattern with
  | Lit ((Int | Real), s) -> Fmt.string ppf s
  | FunApp (CompilerInternal (FnMakeRowVec | FnMakeArray), l) ->
      Fmt.(pf ppf "[@[<hov 1>%a@]]" (list ~sep:comma pp_value_json) l)
  | FunApp (StanLib (transpose, _, _), [e])
    when String.equal transpose (Operator.to_string Transpose) ->
      pp_value_json ppf e
  | _ ->
      Common.FatalError.fatal_error_msg
        [%message "Could not evaluate expression " (e : Expr.Typed.t)]

let print_data_prog s =
  let l, _ =
    List.fold s ~init:([], Map.Poly.empty)
      ~f:(fun (l, m) (sizedtype, transformation, name) ->
        let value = generate_value m sizedtype transformation in
        ((name, value) :: l, Map.set m ~key:name ~data:value) ) in
  let pp ppf (id, value) =
    Fmt.pf ppf {|@[<hov 2>"%s":@ %a@]|} id pp_value_json value in
  Fmt.(str "{@ @[<hov>%a@]@ }" (list ~sep:comma pp) (List.rev l))
