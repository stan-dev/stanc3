open Core_kernel
open Middle
open Ast
open Fmt

let gen_num () = Random.int 7 + 2

let rec repeat n e = match n with 0 -> [] | m -> e :: repeat (m - 1) e

let rec repeat_th n f =
  match n with 0 -> [] | m -> f () :: repeat_th (m - 1) f

let int_n n = {expr= IntNumeral (Int.to_string n); emeta= {loc= no_span}}
let int_two = int_n 2
let gen_int () = int_n (gen_num ())

let gen_real () =
  {int_two with expr= RealNumeral (string_of_int (gen_num ()) ^ ".")}

let gen_row_vector n = {int_two with expr= RowVectorExpr (repeat_th n gen_real)}
let gen_vector n = {int_two with expr= PostfixOp (gen_row_vector n, Transpose)}

let gen_matrix n m =
  {int_two with expr= RowVectorExpr (repeat_th n (fun () -> gen_row_vector m))}

let gen_array elt n = {int_two with expr= ArrayExpr (repeat_th n elt)}

let rec generate_value m (st : untyped_expression sizedtype) :
    untyped_expression =
  let rec get_length_exn m e =
    match e.expr with
    | IntNumeral s -> Int.of_string s
    | Variable s when Map.mem m s.name -> get_length_exn m (Map.find_exn m s.name)
    | _ -> raise_s [%sexp ("Cannot convert size to number." : string)]
  in
  match st with
  | SInt -> gen_int ()
  | SReal -> gen_real ()
  | SVector e -> gen_vector (get_length_exn m e)
  | SRowVector e -> gen_row_vector (get_length_exn m e)
  | SMatrix (e1, e2) -> gen_matrix (get_length_exn m e1) (get_length_exn m e2)
  | SArray (st, e) ->
      let element () = generate_value m st in
      gen_array element (get_length_exn m e)

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

(* TODO: insert partial evaluation *)

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
  | VarDecl {sizedtype; _} -> generate_value m sizedtype
  | _ -> failwith "This should never happen."

let print_data_prog (s : untyped_program) =
  let data = Option.value ~default:[] s.datablock in
  let l, _ =
    List.fold data ~init:([], Map.Poly.empty) ~f:(fun (l, m) decl ->
  let value = (var_decl_gen_val m decl) in
        ( l @ [var_decl_id decl ^ " <- " ^ print_value_r value]
        , Map.set m ~key:(var_decl_id decl) ~data:value )
    )
  in
  String.concat ~sep:"\n" l

(* ---- TESTS ---- *)


let%expect_test "whole program data generation check" =
  let open Parse in
  let ast =
    parse_string Parser.Incremental.program
      "\
      \        data {
      \            int<lower=1> K;
      \            int<lower=1> D;
      \            int<lower=0> N;
      \            int<lower=0,upper=1> y[N,D];
      \            vector[K] x[N];
      \        }
      \        parameters {
      \            matrix[D,K] beta;
      \            cholesky_factor_corr[D] L_Omega;
      \            real<lower=0,upper=1> u[N,D];
      \        }
      \      "
    |> Result.map_error ~f:render_syntax_error
    |> Result.ok_or_failwith
  in
  let str = print_data_prog ast in
  print_s [%sexp (str : string)] ;
  [%expect {|
       "K <- 7\
      \nD <- 5\
      \nN <- 8\
      \ny <- structure(c(8, 7, 3, 2, 8, 3, 2, 4, 3, 3, 4, 4, 5, 8, 3, 4, 6, 4, 3, 8, 2, 8, 2, 2, 5, 8, 7, 6, 4, 3, 7, 8, 5, 8, 7, 4, 3, 6, 6, 8), .Dim=c(8, 5))\
      \nx <- structure(c(5., 4., 3., 8., 7., 6., 3., 2., 7., 8., 2., 2., 5., 8., 3., 8., 5., 7., 7., 6., 7., 5., 6., 2., 5., 8., 5., 3., 6., 2., 2., 8., 2., 6., 5., 5., 2., 6., 8., 3., 5., 2., 5., 6., 3., 4., 5., 5., 6., 8., 7., 4., 6., 4., 5., 6.), .Dim=c(8, 7))" |}]



let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty (SArray (SArray (SInt, int_n 3), int_n 4))
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(7, 5, 8, 8, 7, 3, 2, 8, 3, 2, 4, 3), .Dim=c(4, 3))" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty
      (SArray (SArray (SArray (SInt, int_n 5), int_n 2), int_n 4))
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(7, 5, 8, 8, 7, 3, 2, 8, 3, 2, 4, 3, 3, 4, 4, 5, 8, 3, 4, 6, 4, 3, 8, 2, 8, 2, 2, 5, 8, 7, 6, 4, 3, 7, 8, 5, 8, 7, 4, 3), .Dim=c(4, 2, 5))" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SMatrix (int_n 3, int_n 4)) in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(7., 5., 8., 8., 7., 3., 2., 8., 3., 2., 4., 3.), .Dim=c(3, 4))" |}]

let%expect_test "data generation check" =
  let expr = generate_value Map.Poly.empty (SVector (int_n 3)) in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect {|
      "c(7., 5., 8.)" |}]

let%expect_test "data generation check" =
  let expr =
    generate_value Map.Poly.empty (SArray (SVector (int_n 3), int_n 4))
  in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(7., 5., 8., 8., 7., 3., 2., 8., 3., 2., 4., 3.), .Dim=c(4, 3))" |}]
