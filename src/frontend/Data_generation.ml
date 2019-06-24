open Core_kernel
open Middle
open Ast
open Fmt

let count = ref 0

let gen_num () =
  let out = !count in
  count := out + 1 ;
  out

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

let rec generate_value (st : untyped_expression sizedtype) : untyped_expression
    =
  let get_length_exn e =
    match e.expr with
    | IntNumeral s -> Int.of_string s
    | _ -> raise_s [%sexp ("Cannot convert size to number." : string)]
  in
  match st with
  | SInt -> gen_int ()
  | SReal -> gen_real ()
  | SVector e -> gen_vector (get_length_exn e)
  | SRowVector e -> gen_row_vector (get_length_exn e)
  | SMatrix (e1, e2) -> gen_matrix (get_length_exn e1) (get_length_exn e2)
  | SArray (st, e) ->
      let element () = generate_value st in
      gen_array element (get_length_exn e)

let rec flatten (e : untyped_expression) =
  let flatten_expr_list l =
    List.fold (List.map ~f:flatten l) ~init:[] ~f:(fun vals new_vals ->
        new_vals @ vals )
  in
  match e.expr with
  | IntNumeral s -> [s]
  | RealNumeral s -> [s]
  | ArrayExpr l -> flatten_expr_list l
  | RowVectorExpr l -> flatten_expr_list l
  | _ -> failwith "This should never happen."

let rec dims e =
  let list_dims l = Int.to_string (List.length l) :: dims (List.hd_exn l) in
  match e.expr with
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

let%expect_test "data generation check" =
  let expr = generate_value (SArray (SArray (SInt, int_n 3), int_n 4)) in
  let str = print_value_r expr in
  print_s [%sexp (str : string)] ;
  [%expect
    {|
      "structure(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), .Dim=c(4, 3))" |}]
