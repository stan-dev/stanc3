open Core
open Middle

let rec transpose = function
  | [] :: _ -> []
  | rows ->
      let hd = List.map ~f:List.hd_exn rows in
      let tl = List.map ~f:List.tl_exn rows in
      hd :: transpose tl

let reject loc msg = raise (Partial_evaluator.Rejected (loc, msg))

let dotproduct xs ys =
  List.fold2_exn xs ys ~init:0. ~f:(fun accum x y -> accum +. (x *. y))

let matprod x y =
  let y_T = transpose y in
  if List.length x <> List.length y_T then
    Common.ICE.internal_compiler_error
      [%message "Matrix multiplication dim. mismatch"]
  else List.map ~f:(fun row -> List.map ~f:(dotproduct row) y_T) x

let rec vect_to_mat l m =
  let len = List.length l in
  if len % m <> 0 then
    Common.ICE.internal_compiler_error
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
      reject e.meta.loc
        (Fmt.str "Cannot evaluate expression: %a" Expr.Fixed.pp e)

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
      reject e.meta.loc
        (Fmt.str "Cannot evaluate bounded (upper OR lower) expr: %a"
           Expr.Fixed.pp e)

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
  | None, None ->
      reject e1.meta.loc
        (Fmt.str "Cannot evaluate upper and lower bound expr: %a and %a"
           Expr.Fixed.pp e1 Expr.Fixed.pp e2)

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
    List.fold_map l ~init:0. ~f:(fun accum elt ->
        let elt = accum +. elt in
        (elt, elt)) in
  match t with
  | Transformation.Simplex ->
      let l = repeat_th n (fun _ -> Random.float 1.) in
      let sum = List.fold l ~init:0. ~f:(fun accum elt -> accum +. elt) in
      let l = List.map l ~f:(fun x -> x /. sum) in
      Expr.Helpers.vector l
  | Ordered ->
      let max, l = gen_ordered n in
      let l = List.map l ~f:(fun x -> x -. (max /. 2.0)) in
      Expr.Helpers.vector l
  | PositiveOrdered ->
      let _, l = gen_ordered n in
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
      @ repeat (n - k) 0.)

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

let gen_complex_unwrapped () =
  ( gen_num_real Map.Poly.empty Transformation.Identity
  , gen_num_real Map.Poly.empty Transformation.Identity )

let gen_complex () = Expr.Helpers.complex (gen_complex_unwrapped ())

let gen_complex_row_vector n =
  Expr.Helpers.complex_row_vector (repeat_th n gen_complex_unwrapped)

let gen_complex_vector n =
  Expr.Helpers.complex_vector (repeat_th n gen_complex_unwrapped)

let gen_complex_matrix m n =
  Expr.Helpers.complex_matrix_from_rows
    (repeat_th m (fun () -> gen_complex_row_vector n))

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

and gen_tuple m st t =
  Expr.Helpers.tuple_expr
    (Utils.(zip_stuple_trans_exn st (tuple_trans_exn t))
    |> List.map ~f:(fun (x, y) -> generate_value m x y))

and generate_value m st t =
  match st with
  | SizedType.SInt -> Expr.Helpers.int (gen_num_int m t)
  | SReal -> Expr.Helpers.float (gen_num_real m t)
  | SComplex -> gen_complex ()
  | SVector (_, e) -> gen_vector m (unwrap_int_exn m e) t
  | SRowVector (_, e) -> gen_row_vector m (unwrap_int_exn m e) t
  | SMatrix (_, e1, e2) ->
      gen_matrix m (unwrap_int_exn m e1) (unwrap_int_exn m e2) t
  | SComplexVector e -> gen_complex_vector (unwrap_int_exn m e)
  | SComplexRowVector e -> gen_complex_row_vector (unwrap_int_exn m e)
  | SComplexMatrix (e1, e2) ->
      gen_complex_matrix (unwrap_int_exn m e1) (unwrap_int_exn m e2)
  | SArray (st, e) -> gen_array m st (unwrap_int_exn m e) t
  | STuple _ -> gen_tuple m st t

let generate_expressions input data =
  List.fold data ~init:([], input)
    ~f:(fun (l, m) (sizedtype, transformation, name) ->
      match Map.find m name with
      | Some value -> ((name, value) :: l, m)
      | None ->
          let value = generate_value m sizedtype transformation in
          ((name, value) :: l, Map.set m ~key:name ~data:value))
  |> fst |> List.rev

open Yojson

let json_to_mir (decls : (Expr.Typed.t SizedType.t * 'a * string) list)
    (json : Yojson.Basic.t) =
  let rec create_expr (type_ : UnsizedType.t) (json : Basic.t) =
    let as_float = function `Int i -> float_of_int i | `Float f -> f in
    let try_float = function
      | `Int i -> Some (float_of_int i)
      | `Float f -> Some f
      | _ -> None in
    let try_complex = function
      | `List [((`Int _ | `Float _) as r); ((`Int _ | `Float _) as i)] ->
          Some (as_float r, as_float i)
      | _ -> None in
    let try_map f l g =
      let s = List.filter_map ~f l in
      if List.length s <> List.length l then None else Some (g s) in
    match (json, type_) with
    | `Int i, (UInt | UReal) -> Some (Expr.Helpers.int i)
    | `Float f, (UInt | UReal) -> Some (Expr.Helpers.float f)
    | `List [((`Int _ | `Float _) as r); ((`Int _ | `Float _) as i)], UComplex
      ->
        Some (Expr.Helpers.complex (as_float r, as_float i))
    | `List l, UArray ty -> try_map (create_expr ty) l Expr.Helpers.array_expr
    | `List l, UVector -> try_map try_float l Expr.Helpers.vector
    | `List l, URowVector -> try_map try_float l Expr.Helpers.row_vector
    | `List l, UMatrix ->
        try_map (create_expr URowVector) l Expr.Helpers.matrix_from_rows
    | `List l, UComplexVector ->
        try_map try_complex l Expr.Helpers.complex_vector
    | `List l, UComplexRowVector ->
        try_map try_complex l Expr.Helpers.complex_row_vector
    | `List l, UComplexMatrix ->
        try_map
          (create_expr UComplexRowVector)
          l Expr.Helpers.complex_matrix_from_rows
    | `Assoc l, UTuple ts ->
        l
        |> List.sort ~compare:(fun (x, _) (y, _) ->
               Int.compare (int_of_string x) (int_of_string y))
        |> List.map2_exn ~f:(fun typ_ (_, json) -> create_expr typ_ json) ts
        |> Option.all
        |> Option.map ~f:(fun l -> Expr.Helpers.tuple_expr l)
    | _ -> None in
  let map =
    match json with
    | `Assoc l -> String.Map.of_alist_reduce ~f:Fn.const l
    | _ -> String.Map.empty in
  List.filter_map decls ~f:(fun (st, _, name) ->
      Map.find map name
      |> Option.bind ~f:(fun value ->
             create_expr (SizedType.to_unsized st) value)
      |> Option.map ~f:(fun value -> (name, value)))
  |> Map.Poly.of_alist_reduce ~f:Fn.const

let generate_json_entries (name, expr) : string * t =
  let rec expr_to_json e : t =
    match e.Expr.Fixed.pattern with
    | Lit (Real, s) when String.is_suffix s ~suffix:"." -> `Floatlit (s ^ "0")
    | Lit (Int, s) -> `Intlit s
    | Lit (Real, s) -> `Floatlit s
    | FunApp (CompilerInternal (FnMakeRowVec | FnMakeArray), l)
     |FunApp (StanLib ("to_complex", _, _), l) ->
        `List (List.map ~f:expr_to_json l)
    | FunApp (CompilerInternal FnMakeTuple, l) ->
        `Assoc
          (List.mapi ~f:(fun i t -> (string_of_int (i + 1), expr_to_json t)) l)
    | FunApp (StanLib (transpose, _, _), [e])
      when String.equal transpose (Operator.to_string Transpose) ->
        expr_to_json e
    | _ ->
        reject e.meta.Expr.Typed.Meta.loc
          (Fmt.str "Could not evaluate expression %a" Expr.Fixed.pp e) in
  (name, expr_to_json expr)

let gen_values_json_exn ?(new_only = false) ?(context = Map.Poly.empty) decls =
  let ids_and_values = generate_expressions context decls in
  let json_entries = List.map ~f:generate_json_entries ids_and_values in
  let json_entries =
    if new_only then
      List.filter
        ~f:(fun (name, _) -> Option.is_none (Map.find context name))
        json_entries
    else json_entries in
  let json = `Assoc json_entries in
  pretty_to_string json

let gen_values_json ?(new_only = false) ?(context = Map.Poly.empty) decls =
  try Ok (gen_values_json_exn ~new_only ~context decls)
  with Partial_evaluator.Rejected (loc, msg) ->
    Error (Frontend.Errors.DebugDataError (loc, msg))
