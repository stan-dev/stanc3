open Core_kernel
open Middle
open Ast
open Fmt

let rec unwrap_num_exn m e =
  match e.expr with
  | IntNumeral s -> Float.of_string s
  | RealNumeral s -> Float.of_string s
  | Variable s when Map.mem m s.name ->
      unwrap_num_exn m (Map.find_exn m s.name)
  (* TODO: insert partial evaluation here *)
  | _ -> raise_s [%sexp ("Cannot convert size to number." : string)]

let unwrap_int_exn m e = Int.of_float (unwrap_num_exn m e)

let gen_num_int m (t : untyped_expression transformation) =
  let def_low, diff = (2, 5) in
  let low, up =
    match t with
    | Lower e -> (unwrap_int_exn m e, unwrap_int_exn m e + diff)
    | Upper e -> (unwrap_int_exn m e - diff, unwrap_int_exn m e)
    | LowerUpper (e1, e2) -> (unwrap_int_exn m e1, unwrap_int_exn m e2)
    | _ -> (def_low, def_low + diff)
  in
  Random.int (up - low + 1) + low

let gen_num_real m (t : untyped_expression transformation) =
  let def_low, diff = (2., 5.) in
  let low, up =
    match t with
    | Lower e -> (unwrap_num_exn m e, unwrap_num_exn m e +. diff)
    | Upper e -> (unwrap_num_exn m e -. diff, unwrap_num_exn m e)
    | LowerUpper (e1, e2) -> (unwrap_num_exn m e1, unwrap_num_exn m e2)
    | _ -> (def_low, def_low +. diff)
  in
  Random.float_range low up

let rec repeat n e = match n with 0 -> [] | m -> e :: repeat (m - 1) e

let rec repeat_th n f =
  match n with 0 -> [] | m -> f () :: repeat_th (m - 1) f

let wrap_int n = {expr= IntNumeral (Int.to_string n); emeta= {loc= no_span}}
let int_two = wrap_int 2
let wrap_real r = {expr= RealNumeral (Float.to_string r); emeta= {loc= no_span}}
let wrap_row_vector l = {expr= RowVectorExpr l; emeta= {loc= no_span}}

let wrap_vector l =
  {expr= PostfixOp (wrap_row_vector l, Transpose); emeta= {loc= no_span}}

let gen_int m t = wrap_int (gen_num_int m t)
let gen_real m t = wrap_real (gen_num_real m t)

let gen_row_vector m n t =
  {int_two with expr= RowVectorExpr (repeat_th n (fun _ -> gen_real m t))}

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
                  (repeat (k - 1) 0. @ [1.0] @ repeat (m - k) 0.)) )) }

let gen_matrix mm n m t =
  match t with
  | CholeskyCorr | CholeskyCov | Correlation | Covariance ->
      gen_identity_matrix n m
  | _ ->
      { int_two with
        expr= RowVectorExpr (repeat_th n (fun () -> gen_row_vector mm m t)) }

(* TODO: do some proper random generation of these special matrices *)

let gen_array elt n _ = {int_two with expr= ArrayExpr (repeat_th n elt)}

let rec generate_value m (st : untyped_expression sizedtype) t :
    untyped_expression =
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

let rec flatten (e : untyped_expression) =
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
  let list_dims l = Int.to_string (List.length l) :: dims (List.hd_exn l) in
  match e.expr with
  | PostfixOp (e, Transpose) -> dims e
  | IntNumeral _ -> []
  | RealNumeral _ -> []
  | ArrayExpr l -> list_dims l
  | RowVectorExpr l -> list_dims l
  | _ -> failwith "This should never happen."

(* TODO: deal with bounds *)

let rec print_value_r (e : untyped_expression) =
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

let print_data_prog (s : untyped_program) =
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
      "        data {\n\
      \                  int<lower=7> K;\n\
      \                  int<lower=1> D;\n\
      \                  int<lower=0> N;\n\
      \                  int<lower=0,upper=1> y[N,D];\n\
      \                  vector[K] x[N];\n\
      \              }\n\
      \              parameters {\n\
      \                  matrix[D,K] beta;\n\
      \                  cholesky_factor_corr[D] L_Omega;\n\
      \                  real<lower=0,upper=1> u[N,D];\n\
      \              }\n\
      \            "
    |> Result.map_error ~f:render_syntax_error
    |> Result.ok_or_failwith
  in
  let str = print_data_prog ast in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
       "K <- 7\
      \nD <- 1\
      \nN <- 4\
      \ny <- structure(c(0, 1, 1, 0), .Dim=c(4, 1))\
      \nx <- structure(c(4.80196289276064, 2.5002977170064504, 6.9902151159107628, 4.2216206009762942, 4.7079605269879039, 4.3960859534107088, 6.3642805904191038, 4.5612191381201592, 2.7534274519828044, 2.1638430051007465, 4.10860520962623, 6.4002461410324072, 4.5275773278275526, 3.3278518601429194, 4.8795989339689632, 6.5076878401532312, 4.262745557146193, 6.7551216326282217, 4.830628400636872, 4.2526159122345248, 4.1926916964909324, 4.6283924039600581, 5.1840171875153906, 3.8263490131545348, 6.32804481242085, 5.68173766561979, 2.3652769548085839, 4.5357289910613918), .Dim=c(4, 7))" |}]

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
      "structure(c(2, 2, 6, 2, 5, 5, 2, 7, 3, 2, 6, 3), .Dim=c(4, 3))" |}]

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
      "structure(c(2, 2, 6, 2, 5, 5, 2, 7, 3, 2, 6, 3, 2, 4, 5, 7, 5, 5, 6, 5, 6, 5, 5, 5, 7, 5, 5, 3, 2, 3, 7, 3, 4, 6, 3, 3, 4, 3, 2, 5), .Dim=c(4, 2, 5))" |}]

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
      (SMatrix (wrap_int 2, wrap_int 3))
      Correlation
  in
  print_s [%sexp (expr : untyped_expression)] ;
  [%expect
    {|
      ((expr
        (RowVectorExpr
         (((expr
            (RowVectorExpr
             (((expr (RealNumeral 1.)) (emeta ((loc <opaque>))))
              ((expr (RealNumeral 0.)) (emeta ((loc <opaque>))))
              ((expr (RealNumeral 0.)) (emeta ((loc <opaque>)))))))
           (emeta ((loc <opaque>))))
          ((expr
            (RowVectorExpr
             (((expr (RealNumeral 0.)) (emeta ((loc <opaque>))))
              ((expr (RealNumeral 1.)) (emeta ((loc <opaque>))))
              ((expr (RealNumeral 0.)) (emeta ((loc <opaque>)))))))
           (emeta ((loc <opaque>)))))))
       (emeta ((loc <opaque>)))) |}]
