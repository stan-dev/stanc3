open Core_kernel
open Middle
open Ast
open Fmt

let unwrap_num_exn m e =
  let e = Ast_to_Mir.trans_expr e in
  let m = Map.Poly.map m ~f:Ast_to_Mir.trans_expr in
  let e = Analysis_and_optimization.Mir_utils.subst_expr m e in
  let e = Analysis_and_optimization.Partial_evaluator.eval_expr e in
  match e.expr with
  | Lit (_, s) -> Float.of_string s
  | _ -> raise_s [%sexp ("Cannot convert size to number." : string)]

let unwrap_int_exn m e = Int.of_float (unwrap_num_exn m e)

let gen_num_int m t =
  let def_low, diff = (2, 4) in
  let low, up =
    match t with
    | Lower e -> (unwrap_int_exn m e, unwrap_int_exn m e + diff)
    | Upper e -> (unwrap_int_exn m e - diff, unwrap_int_exn m e)
    | LowerUpper (e1, e2) -> (unwrap_int_exn m e1, unwrap_int_exn m e2)
    | _ -> (def_low, def_low + diff)
  in
  let low = if low = 0 && up <> 1 then low + 1 else low in
  Random.int (up - low + 1) + low

let gen_num_real m t =
  let def_low, diff = (2., 5.) in
  let low, up =
    match t with
    | Lower e -> (unwrap_num_exn m e, unwrap_num_exn m e +. diff)
    | Upper e -> (unwrap_num_exn m e -. diff, unwrap_num_exn m e)
    | LowerUpper (e1, e2) -> (unwrap_num_exn m e1, unwrap_num_exn m e2)
    | _ -> (def_low, def_low +. diff)
  in
  Random.float_range low up

let rec repeat n e =
  match n with n when n <= 0 -> [] | m -> e :: repeat (m - 1) e

let rec repeat_th n f =
  match n with n when n <= 0 -> [] | m -> f () :: repeat_th (m - 1) f

let wrap_int n =
  { expr= IntNumeral (Int.to_string n)
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= UInt} }

let int_two = wrap_int 2

let wrap_real r =
  { expr= RealNumeral (Float.to_string r)
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= UReal} }

let wrap_row_vector l =
  { expr= RowVectorExpr l
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= URowVector} }

let wrap_vector l =
  { expr= PostfixOp (wrap_row_vector l, Transpose)
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= UVector} }

let gen_int m t = wrap_int (gen_num_int m t)
let gen_real m t = wrap_real (gen_num_real m t)

let gen_row_vector m n t =
  { expr= RowVectorExpr (repeat_th n (fun _ -> gen_real m t))
  ; emeta= {loc= no_span; ad_level= DataOnly; type_= UMatrix} }

let gen_vector m n t =
  let gen_ordered n =
    let l = repeat_th n (fun _ -> Random.float 1.) in
    let l =
      List.fold (List.tl_exn l) ~init:[List.hd_exn l] ~f:(fun accum elt ->
          (Float.exp elt +. List.hd_exn accum) :: accum )
    in
    l
  in
  match t with
  | Simplex ->
      let l = repeat_th n (fun _ -> Random.float 1.) in
      let sum = List.fold l ~init:0. ~f:(fun accum elt -> accum +. elt) in
      let l = List.map l ~f:(fun x -> x /. sum) in
      wrap_vector (List.map ~f:wrap_real l)
  | Ordered ->
      let l = gen_ordered n in
      let halfmax =
        Option.value_exn (List.max_elt l ~compare:compare_float) /. 2.
      in
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

let gen_identity_matrix n m =
  { int_two with
    expr=
      RowVectorExpr
        (List.map
           (List.range 1 (n + 1))
           ~f:(fun k ->
             wrap_row_vector
               (List.map ~f:wrap_real
                  ( repeat (min (k - 1) m) 0.
                  @ (if k <= m then [1.0] else [])
                  @ repeat (m - k) 0. )) )) }

let gen_matrix mm n m t =
  match t with
  | CholeskyCorr | CholeskyCov | Correlation | Covariance ->
      gen_identity_matrix n m
  | _ ->
      { int_two with
        expr= RowVectorExpr (repeat_th n (fun () -> gen_row_vector mm m t)) }

(* TODO: do some proper random generation of these special matrices *)

let gen_array elt n _ = {int_two with expr= ArrayExpr (repeat_th n elt)}

let rec generate_value m st t =
  match st with
  | SInt -> gen_int m t
  | SReal -> gen_real m t
  | SVector e -> gen_vector m (unwrap_int_exn m e) t
  | SRowVector e -> gen_row_vector m (unwrap_int_exn m e) t
  | SMatrix (e1, e2) ->
      gen_matrix m (unwrap_int_exn m e1) (unwrap_int_exn m e2) t
  | SArray (st, e) ->
      let element () = generate_value m st t in
      gen_array element (unwrap_int_exn m e) t

let rec flatten e =
  let flatten_expr_list l =
    List.fold (List.map ~f:flatten l) ~init:[] ~f:(fun vals new_vals ->
        new_vals @ vals )
  in
  match e.expr with
  | PostfixOp (e, Transpose) -> flatten e
  | IntNumeral s -> [s]
  | RealNumeral s -> [s]
  | ArrayExpr l -> flatten_expr_list l
  | RowVectorExpr l -> flatten_expr_list l
  | _ -> failwith "This should never happen."

let rec dims e =
  let list_dims l =
    if List.length l = 0 then []
    else Int.to_string (List.length l) :: dims (List.hd_exn l)
  in
  match e.expr with
  | PostfixOp (e, Transpose) -> dims e
  | IntNumeral _ -> []
  | RealNumeral _ -> []
  | ArrayExpr l -> list_dims l
  | RowVectorExpr l -> list_dims l
  | _ -> failwith "This should never happen."

let rec print_value_r e =
  let expr = e.expr in
  let print_container e =
    let vals, dims = (flatten e, dims e) in
    let flattened_str = "c(" ^ String.concat ~sep:", " vals ^ ")" in
    if List.length dims <= 1 then flattened_str
    else
      "structure(" ^ flattened_str ^ ", .Dim=" ^ "c("
      ^ String.concat ~sep:", " dims
      ^ ")" ^ ")"
  in
  match expr with
  | PostfixOp (e, Transpose) -> print_value_r e
  | IntNumeral s -> s
  | RealNumeral s -> s
  | ArrayExpr _ -> print_container e
  | RowVectorExpr _ -> print_container e
  | _ -> failwith "This should never happen."

let var_decl_id d =
  match d.stmt with
  | VarDecl {identifier; _} -> identifier.name
  | _ -> failwith "This should never happen."

let var_decl_gen_val m d =
  match d.stmt with
  | VarDecl {sizedtype; transformation; _} ->
      generate_value m sizedtype transformation
  | _ -> failwith "This should never happen."

let print_data_prog s =
  let data = Option.value ~default:[] s.datablock in
  let l, _ =
    List.fold data ~init:([], Map.Poly.empty) ~f:(fun (l, m) decl ->
        let value = var_decl_gen_val m decl in
        ( l @ [var_decl_id decl ^ " <- " ^ print_value_r value]
        , Map.set m ~key:(var_decl_id decl) ~data:value ) )
  in
  String.concat ~sep:"\n" l

(* ---- TESTS ---- *)

let%expect_test "whole program data generation check" =
  let open Parse in
  let ast =
    parse_string Parser.Incremental.program
      {|       data {
                  int<lower=7> K;
                  int<lower=1> D;
                  int<lower=0> N;
                  int<lower=0,upper=1> y[N,D];
                  vector[K] x[N];
                    }
      |}
  in
  let ast =
    Option.value_exn
      (Result.ok
         (Semantic_check.semantic_check_program
            (Option.value_exn (Result.ok ast))))
  in
  let str = print_data_prog ast in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
       "K <- 11\
      \nD <- 3\
      \nN <- 1\
      \ny <- structure(c(0, 1, 1), .Dim=c(1, 3))\
      \nx <- structure(c(4.25312636944623, 5.2015419032442969, 2.7103944900448411, 3.3282621325833865, 2.56799363086151, 4.0759938356540726, 3.604405750889411, 6.0288479433993629, 3.543689144366625, 4.1465170437036338, 5.8799711085519224), .Dim=c(1, 11))" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SArray (SArray (SInt, wrap_int 3), wrap_int 4))
      Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(6, 4, 5, 2, 5, 5, 4, 5, 6, 5, 3, 2), .Dim=c(4, 3))" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SArray (SArray (SArray (SInt, wrap_int 5), wrap_int 2), wrap_int 4))
      Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(6, 4, 5, 2, 5, 5, 4, 5, 6, 5, 3, 2, 3, 2, 3, 5, 2, 3, 2, 3, 2, 4, 3, 6, 5, 2, 4, 3, 2, 2, 5, 3, 5, 6, 2, 5, 4, 6, 4, 5), .Dim=c(4, 2, 5))" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty (SMatrix (wrap_int 3, wrap_int 4)) Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(4.1815278199399577, 6.8017664359959342, 4.8441784126802627, 4.25312636944623, 5.2015419032442969, 2.7103944900448411, 3.3282621325833865, 2.56799363086151, 4.0759938356540726, 3.604405750889411, 6.0288479433993629, 3.543689144366625), .Dim=c(3, 4))" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (wrap_int 3)) Identity in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(4.1815278199399577, 6.8017664359959342, 4.8441784126802627)" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SArray (SVector (wrap_int 3), wrap_int 4))
      Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(4.1815278199399577, 6.8017664359959342, 4.8441784126802627, 4.25312636944623, 5.2015419032442969, 2.7103944900448411, 3.3282621325833865, 2.56799363086151, 4.0759938356540726, 3.604405750889411, 6.0288479433993629, 3.543689144366625), .Dim=c(4, 3))" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (wrap_int 3)) Simplex in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(0.22198258835220422, 0.48860644012069177, 0.289410971527104)" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (wrap_int 3)) UnitVector in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(0.36406675257322474, 0.80134825556167411, 0.47465395076734407)" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (wrap_int 30)) Ordered in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(-0.96832729385188365, -0.91004897647537208, -0.855488426712053, -0.8070719147445744, -0.73591866262419769, -0.64090940155884812, -0.58159948031027586, -0.498033598356894, -0.43497790423044719, -0.39103777937151057, -0.33030882131474781, -0.24446230343719536, -0.18409371422459761, -0.10156543104346247, -0.05653532094436739, 0.013923649350849741, 0.10638632763557807, 0.17175965077143784, 0.22970780349821893, 0.32496498924437173, 0.38362111488017359, 0.44807884297139466, 0.49575395920660592, 0.55125841432324352, 0.60031084605608087, 0.68104157187272929, 0.74782374244229277, 0.82298579970312025, 0.93416731632648975, 1.)" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty (SVector (wrap_int 30)) PositiveOrdered
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "c(0.0158363530740582, 0.044975511762313986, 0.072255786643973488, 0.096464042627712757, 0.13204066868790115, 0.17954529922057594, 0.20920025984486204, 0.250983200821553, 0.28251104788477643, 0.30448111031424469, 0.33484558934262609, 0.37776884828140234, 0.40795314288770118, 0.44921728447826875, 0.47173233952781629, 0.50696182467542483, 0.553193163817789, 0.58587982538571892, 0.61485390174910948, 0.66248249462218589, 0.69181055744008679, 0.72403942148569733, 0.7478769796033029, 0.77562920716162176, 0.80015542302804044, 0.84052078593636459, 0.87391187122114644, 0.91149289985156012, 0.96708365816324482, 1.)" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SMatrix (wrap_int 4, wrap_int 2))
      Correlation
  in
  print_s [%sexp (expr : typed_expression)] ;
  [%expect
    {|
      ((expr
        (RowVectorExpr
         (((expr
            (RowVectorExpr
             (((expr (RealNumeral 1.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal))))
              ((expr (RealNumeral 0.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal)))))))
           (emeta ((loc <opaque>) (ad_level DataOnly) (type_ URowVector))))
          ((expr
            (RowVectorExpr
             (((expr (RealNumeral 0.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal))))
              ((expr (RealNumeral 1.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal)))))))
           (emeta ((loc <opaque>) (ad_level DataOnly) (type_ URowVector))))
          ((expr
            (RowVectorExpr
             (((expr (RealNumeral 0.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal))))
              ((expr (RealNumeral 0.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal)))))))
           (emeta ((loc <opaque>) (ad_level DataOnly) (type_ URowVector))))
          ((expr
            (RowVectorExpr
             (((expr (RealNumeral 0.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal))))
              ((expr (RealNumeral 0.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal)))))))
           (emeta ((loc <opaque>) (ad_level DataOnly) (type_ URowVector)))))))
       (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt)))) |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SMatrix (wrap_int 2, wrap_int 2))
      Correlation
  in
  print_s [%sexp (expr : typed_expression)] ;
  [%expect
    {|
      ((expr
        (RowVectorExpr
         (((expr
            (RowVectorExpr
             (((expr (RealNumeral 1.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal))))
              ((expr (RealNumeral 0.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal)))))))
           (emeta ((loc <opaque>) (ad_level DataOnly) (type_ URowVector))))
          ((expr
            (RowVectorExpr
             (((expr (RealNumeral 0.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal))))
              ((expr (RealNumeral 1.))
               (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UReal)))))))
           (emeta ((loc <opaque>) (ad_level DataOnly) (type_ URowVector)))))))
       (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt)))) |}]

let%expect_test "whole program data generation check" =
  let open Parse in
  let ast =
    parse_string Parser.Incremental.program
      {|       data {
                  int<lower=2, upper=4> K;
                  int<lower=K, upper=K> D;
                  vector[K - 1] x;
                  vector[K * D] y;
                  vector[K ? D : K] z;
                  vector[K ? D : K] w[(D + 2 == K) + 3];
                }
      |}
  in
  let ast =
    Option.value_exn
      (Result.ok
         (Semantic_check.semantic_check_program
            (Option.value_exn (Result.ok ast))))
  in
  let str = print_data_prog ast in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
       "K <- 2\
      \nD <- 2\
      \nx <- c(6.8017664359959342)\
      \ny <- c(4.8441784126802627, 4.25312636944623, 5.2015419032442969, 2.7103944900448411)\
      \nz <- c(3.3282621325833865, 2.56799363086151)\
      \nw <- structure(c(4.0759938356540726, 3.604405750889411, 6.0288479433993629, 3.543689144366625, 4.1465170437036338, 5.8799711085519224), .Dim=c(3, 2))" |}]

let%expect_test "whole program data generation check" =
  let open Parse in
  let ast =
    parse_string Parser.Incremental.program
      {|
        data {
          corr_matrix[5] d;
          cov_matrix[4] e;
          cholesky_factor_cov[4] f;
          cholesky_factor_corr[4] g;
          unit_vector[4] h;
          simplex[12] i;
          ordered[2] j;
          positive_ordered[4] k;
          cholesky_factor_cov[5, 4] l;
        }
      |}
  in
  let ast =
    Option.value_exn
      (Result.ok
         (Semantic_check.semantic_check_program
            (Option.value_exn (Result.ok ast))))
  in
  let str = print_data_prog ast in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
       "d <- structure(c(1., 0., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 1.), .Dim=c(5, 5))\
      \ne <- structure(c(1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1.), .Dim=c(4, 4))\
      \nf <- structure(c(1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1.), .Dim=c(4, 4))\
      \ng <- structure(c(1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1.), .Dim=c(4, 4))\
      \nh <- c(0.3407724101929635, 0.75007501926931519, 0.44428382890598478, 0.35195668667046631)\
      \ni <- c(0.13400440686830675, 0.029734420213118219, 0.055596017363414456, 0.023774060096306261, 0.086893231766599, 0.0671543423517797, 0.16863230135165055, 0.064612975382352431, 0.089845065898632351, 0.16240088144427856, 0.10552362463724606, 0.011828672626315713)\
      \nj <- c(-0.69448357871239441, 1.)\
      \nk <- c(0.080739544327963347, 0.29275587806614473, 0.58578033077923275, 1.)\
      \nl <- structure(c(0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1.), .Dim=c(5, 4))" |}]

let%expect_test "whole program data generation check" =
  let open Parse in
  let ast =
    parse_string Parser.Incremental.program
      {|
        data {
          int<lower=0> N;
          int<lower=0> M;
          int<lower=0, upper=N * M> K;
          int<upper=N> d_int_1d_ar[N];
          int<upper=N> d_int_3d_ar[N, M, K];
          real<lower=-2.0, upper=2.0> J;
          real d_real_1d_ar[N];
          real d_real_3d_ar[N, M, K];
          vector[N] d_vec;
          vector[N] d_1d_vec[N];
          vector[N] d_3d_vec[N, M, K];
          row_vector[N] d_row_vec;
          row_vector[N] d_1d_row_vec[N];
          row_vector[N] d_3d_row_vec[N, M, K];
          matrix<lower=0, upper=1>[2, 3] d_ar_mat[4, 5];
          simplex[N] d_simplex;
          simplex[N] d_1d_simplex[N];
          simplex[N] d_3d_simplex[N, M, K];
          cholesky_factor_cov[5, 4] d_cfcov_54;
          cholesky_factor_cov[3] d_cfcov_33;
          cholesky_factor_cov[3] d_cfcov_33_ar[K];
        }
      |}
  in
  let ast =
    Option.value_exn
      (Result.ok
         (Semantic_check.semantic_check_program
            (Option.value_exn (Result.ok ast))))
  in
  let str = print_data_prog ast in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
       "N <- 3\
      \nM <- 1\
      \nK <- 2\
      \nd_int_1d_ar <- c(-1, 2, 2)\
      \nd_int_3d_ar <- structure(c(1, 2, 3, 2, 0, -1), .Dim=c(3, 1, 2))\
      \nJ <- -0.937390293933291\
      \nd_real_1d_ar <- c(2.56799363086151, 4.0759938356540726, 3.604405750889411)\
      \nd_real_3d_ar <- structure(c(6.0288479433993629, 3.543689144366625, 4.1465170437036338, 5.8799711085519224, 4.521098476935741, 2.2826025797056464), .Dim=c(3, 1, 2))\
      \nd_vec <- c(5.3116509882058738, 3.7482903006745985, 5.508750812969728)\
      \nd_1d_vec <- structure(c(3.7780489469949972, 2.1600852257225154, 3.9660375311552749, 5.3741223139973622, 3.6598286733521297, 6.0158175418085342, 4.5701258402582194, 2.6451502425447266, 3.2425011320285027), .Dim=c(3, 3))\
      \nd_3d_vec <- structure(c(3.5720962307677091, 5.7213345511646363, 4.0319385317762162, 2.3295401414257744, 3.8281495864625956, 4.869813970717308, 3.7293083759369448, 6.97070615398329, 4.1512076186352216, 4.1949090422787174, 6.8138349711922652, 4.9461191671001892, 4.7922353997475788, 2.292622453020976, 3.226595023801782, 3.8556222147542085, 2.0095894885098069, 5.2448759493135153), .Dim=c(3, 1, 2, 3))\
      \nd_row_vec <- c(4.0434169569431706, 5.7759384905058795, 4.2138065257931459)\
      \nd_1d_row_vec <- structure(c(4.6676412390878905, 6.5986584016153875, 6.4287706833617158, 3.19883563012539, 2.5869872307079236, 2.0298667610240964, 2.4903795562578557, 2.1318346935923507, 5.5315257534514712), .Dim=c(3, 3))\
      \nd_3d_row_vec <- structure(c(6.0608327679881207, 3.3969401849295595, 4.9283096565285138, 2.3777024486238973, 6.4536316336809891, 6.2243138037125068, 5.406294923615655, 4.5064008029760334, 6.8340525891748189, 4.3293598111485032, 3.2151318313470769, 5.6252338866513316, 2.7137240155194529, 6.2480788401756451, 6.4835819351872654, 5.7992229887995883, 6.29275143528753, 5.8259738592178358), .Dim=c(3, 1, 2, 3))\
      \nd_ar_mat <- structure(c(0.19010828624332626, 0.77523119897494908, 0.075435907403433713, 0.10640786283706954, 0.9255170309180637, 0.97429570147644506, 0.31270201687960314, 0.67108049948136128, 0.97871332361306373, 0.90299905969887218, 0.90623281519386112, 0.395832644895654, 0.57821828544361364, 0.975395038570636, 0.78390466305359885, 0.9014260551054567, 0.581578803128905, 0.692956694659322, 0.20559486843854785, 0.71924027388196421, 0.79459953975728093, 0.22407621955437815, 0.0331482211644266, 0.29767813861434933, 0.78663225739768683, 0.17576132217686963, 0.16357954010812856, 0.51080157622790656, 0.44445381850779669, 0.55973460502049532, 0.34067444210810638, 0.7923933751954082, 0.89518896165033413, 0.015030294346858163, 0.13622771624317104, 0.43793668913684858, 0.13069440403330407, 0.41035818879220193, 0.6497000557737248, 0.35780805786467229, 0.49822874401216449, 0.51405972722094162, 0.77108493814941892, 0.63969386779910664, 0.027137609222934244, 0.90158697893162376, 0.5801137402153288, 0.67563924241031859, 0.36659486283528481, 0.97374370343899963, 0.91101702970394483, 0.64874258612805491, 0.78991889014591155, 0.70416418983239637, 0.85453819372996653, 0.1331771098716274, 0.90178256755944575, 0.80682474661040526, 0.42006156138308287, 0.53312849666186346, 0.91994370419748739, 0.0645661949334827, 0.90195439170705294, 0.46310136757192755, 0.36618179036051896, 0.10875698401043367, 0.42912560381062059, 0.28217720706625904, 0.73727587916715787, 0.760763820464, 0.28083725052478087, 0.96871518974459825, 0.32648564309712513, 0.65802867310905055, 0.94372088555262923, 0.612115128160238, 0.15572378717971833, 0.88571739200345234, 0.84919075650439724, 0.41461589756593475, 0.97022141344482138, 0.37399667670291026, 0.87516028181059635, 0.23083019388406939, 0.29201824556948469, 0.73738252703807183, 0.20495821973392631, 0.85124674765564845, 0.62502894666644682, 0.71651676085087923, 0.32234368458918788, 0.78382782365681281, 0.42241174496887768, 0.37368848174295793, 0.11839458744816142, 0.4442483267716999, 0.031145119767167891, 0.90928692941997247, 0.18822532999887831, 0.56797857787417749, 0.27600029727304715, 0.70107413958330755, 0.71061143479186961, 0.53296791805267507, 0.7609047403379835, 0.65864668749480892, 0.187060423355116, 0.67800624128286135, 0.80739555544465147, 0.56198012898460514, 0.11214741225685448, 0.34655572443742166, 0.70234904561486411, 0.85689633306737, 0.70138904116412959, 0.86493323130460953, 0.11800610357991351, 0.99435438254623754, 0.53857837713598755, 0.30265828158408009), .Dim=c(4, 5, 2, 3))\
      \nd_simplex <- c(0.30729721114989411, 0.23166736894495787, 0.46103541990514807)\
      \nd_1d_simplex <- structure(c(0.088444449727223429, 0.77945710856527861, 0.13209844170749788, 0.47063988392263983, 0.50671628568629334, 0.022643830391066874, 0.25726638414553332, 0.41903611877747748, 0.32369749707698914), .Dim=c(3, 3))\
      \nd_3d_simplex <- structure(c(0.026806758458343793, 0.276503877179793, 0.69668936436186313, 0.34801006182900285, 0.47131712267841813, 0.18067281549257905, 0.74034599341038165, 0.15014668749639, 0.10950731909322838, 0.36374798352295723, 0.17504619363987861, 0.46120582283716416, 0.4568916340781477, 0.20838925358574761, 0.33471911233610474, 0.079916199639062852, 0.880151205772848, 0.039932594588089045), .Dim=c(3, 1, 2, 3))\
      \nd_cfcov_54 <- structure(c(0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1.), .Dim=c(5, 4))\
      \nd_cfcov_33 <- structure(c(1., 0., 0., 0., 1., 0., 0., 0., 1.), .Dim=c(3, 3))\
      \nd_cfcov_33_ar <- structure(c(1., 0., 0., 0., 1., 0., 0., 0., 1., 1., 0., 0., 0., 1., 0., 0., 0., 1.), .Dim=c(2, 3, 3))" |}]

let%expect_test "whole program data generation check" =
  let open Parse in
  let ast =
    parse_string Parser.Incremental.program
      {|
      data {
        int<lower = 0> K;                     // players
        int<lower = 0> N;                     // games
        int<lower=1, upper = K> player1[N];   // player 1 for game n
        int<lower=1, upper = K> player0[N];   // player 0 for game n
        int<lower = 0, upper = 1> y[N];       // winner for game n
      }
      |}
  in
  let ast =
    Option.value_exn
      (Result.ok
         (Semantic_check.semantic_check_program
            (Option.value_exn (Result.ok ast))))
  in
  let str = print_data_prog ast in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
       "K <- 3\
      \nN <- 1\
      \nplayer1 <- c(2)\
      \nplayer0 <- c(1)\
      \ny <- c(1)" |}]
