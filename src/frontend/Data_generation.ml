open Core_kernel
open Middle
open Ast

let rec repeat n e = match n with 0 -> [] | m -> e :: repeat (m - 1) e

let rec generate_value (st : untyped_expression sizedtype) : untyped_expression
    =
  ignore t ;
  let int_two = {expr= IntNumeral "2"; emeta= {loc= no_span}} in
  let real_two = {int_two with expr= RealNumeral "2."} in
  let row_vector_twos n =
    {int_two with expr= RowVectorExpr (repeat n real_two)}
  in
  let vector_twos n =
    {int_two with expr= PostfixOp (row_vector_twos n, Transpose)}
  in
  let matrix_twos n m =
    {int_two with expr= RowVectorExpr (repeat n (row_vector_twos m))}
  in
  let array elt n = {int_two with expr= ArrayExpr (repeat n elt)} in
  let get_length_exn e =
    match e.expr with
    | IntNumeral s -> Int.of_string s
    | _ -> raise_s [%sexp ("Cannot convert size to number." : string)]
  in
  match st with
  | SInt -> int_two
  | SReal -> real_two
  | SVector e -> vector_twos (get_length_exn e)
  | SRowVector e -> row_vector_twos (get_length_exn e)
  | SMatrix (e1, e2) -> matrix_twos (get_length_exn e1) (get_length_exn e2)
  | SArray (st, e) ->
      let element = generate_value st in
      array element (get_length_exn e)
