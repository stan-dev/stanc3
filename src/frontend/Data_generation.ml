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
let def_low, diff = 2, 5 in
let low, up = match t with
| Lower e -> unwrap_int_exn m e, unwrap_int_exn m e + diff
| Upper e -> unwrap_int_exn m e - diff, unwrap_int_exn m e
| LowerUpper (e1, e2) -> unwrap_int_exn m e1, unwrap_int_exn m e2
| _ -> def_low, def_low + diff
in 
Random.int (up - low + 1) + low

let gen_num_real m (t : untyped_expression transformation) =
let def_low, diff = 2., 5. in
let low, up = match t with
| Lower e -> unwrap_num_exn m e, unwrap_num_exn m e +. diff
| Upper e -> unwrap_num_exn m e -. diff, unwrap_num_exn m e
| LowerUpper (e1, e2) -> unwrap_num_exn m e1, unwrap_num_exn m e2
| _ -> def_low, def_low +. diff
in 
Random.float_range low up

let rec repeat n e = match n with 0 -> [] | m -> e :: repeat (m - 1) e

let rec repeat_th n f =
  match n with 0 -> [] | m -> f () :: repeat_th (m - 1) f

let int_n n = {expr= IntNumeral (Int.to_string n); emeta= {loc= no_span}}
let int_two = int_n 2
let gen_int m t = int_n (gen_num_int m t)

let gen_real m t =
  {int_two with expr= RealNumeral (string_of_float (gen_num_real m t) ^ ".")}

let gen_row_vector m n t =
  {int_two with expr= RowVectorExpr (repeat_th n (fun _ -> (gen_real m t)))}

let gen_vector m n t =
  {int_two with expr= PostfixOp (gen_row_vector m n t, Transpose)}

let gen_matrix mm n m t =
  { int_two with
    expr= RowVectorExpr (repeat_th n (fun () -> gen_row_vector mm m t)) }
(* TODO: special case the generation of the other constraints *)

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
      \nx <- structure(c(4.80196289276., 2.50029771701., 6.99021511591., 4.22162060098., 4.70796052699., 4.39608595341., 6.36428059042., 4.56121913812., 2.75342745198., 2.1638430051., 4.10860520963., 6.40024614103., 4.52757732783., 3.32785186014., 4.87959893397., 6.50768784015., 4.26274555715., 6.75512163263., 4.83062840064., 4.25261591223., 4.19269169649., 4.62839240396., 5.18401718752., 3.82634901315., 6.32804481242., 5.68173766562., 2.36527695481., 4.53572899106.), .Dim=c(4, 7))" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SArray (SArray (SInt, int_n 3), int_n 4))
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
      (SArray (SArray (SArray (SInt, int_n 5), int_n 2), int_n 4))
      Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(2, 2, 6, 2, 5, 5, 2, 7, 3, 2, 6, 3, 2, 4, 5, 7, 5, 5, 6, 5, 6, 5, 5, 5, 7, 5, 5, 3, 2, 3, 7, 3, 4, 6, 3, 3, 4, 3, 2, 5), .Dim=c(4, 2, 5))" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty (SMatrix (int_n 3, int_n 4)) Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(4.18152781994., 6.801766436., 4.84417841268., 4.25312636945., 5.20154190324., 2.71039449004., 3.32826213258., 2.56799363086., 4.07599383565., 3.60440575089., 6.0288479434., 3.54368914437.), .Dim=c(3, 4))" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (int_n 3)) Identity in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect {|
      "c(4.18152781994., 6.801766436., 4.84417841268.)" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SArray (SVector (int_n 3), int_n 4))
      Identity
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(4.18152781994., 6.801766436., 4.84417841268., 4.25312636945., 5.20154190324., 2.71039449004., 3.32826213258., 2.56799363086., 4.07599383565., 3.60440575089., 6.0288479434., 3.54368914437.), .Dim=c(4, 3))" |}]
