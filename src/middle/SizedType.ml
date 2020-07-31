open Core_kernel
open Common

type 'a t =
  | SInt
  | SReal
  | SVector of 'a
  | SRowVector of 'a
  | SMatrix of 'a * 'a
  | SComplex
  | SComplexVector of 'a
  | SComplexRowVector of 'a
  | SComplexMatrix of 'a * 'a
  | SArray of 'a t * 'a
[@@deriving sexp, compare, map, hash, fold]

let rec pp pp_e ppf = function
  | SInt -> Fmt.string ppf "int"
  | SReal -> Fmt.string ppf "real"
  | SVector expr -> Fmt.pf ppf {|vector%a|} (Fmt.brackets pp_e) expr
  | SRowVector expr -> Fmt.pf ppf {|row_vector%a|} (Fmt.brackets pp_e) expr
  | SMatrix (d1_expr, d2_expr) ->
      Fmt.pf ppf {|matrix%a|}
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SComplex -> Fmt.string ppf "complex"
  | SComplexVector expr ->
      Fmt.pf ppf {|complex_vector%a|} (Fmt.brackets pp_e) expr
  | SComplexRowVector expr ->
      Fmt.pf ppf {|complex_row_vector%a|} (Fmt.brackets pp_e) expr
  | SComplexMatrix (d1_expr, d2_expr) ->
      Fmt.pf ppf {|complex_matrix%a|}
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SArray (st, expr) ->
      Fmt.pf ppf {|array%a|}
        Fmt.(pair ~sep:comma (fun ppf st -> pp pp_e ppf st) pp_e |> brackets)
        (st, expr)

let collect_exprs st =
  let rec aux accu = function
    | SInt | SReal | SComplex -> List.rev accu
    | SVector e | SRowVector e | SComplexVector e | SComplexRowVector e ->
        List.rev @@ (e :: accu)
    | SMatrix (e1, e2) | SComplexMatrix (e1, e2) ->
        List.rev @@ (e1 :: e2 :: accu)
    | SArray (inner, e) -> aux (e :: accu) inner
  in
  aux [] st

let rec to_unsized = function
  | SInt -> UnsizedType.UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SComplex -> UComplex
  | SComplexVector _ -> UComplexVector
  | SComplexRowVector _ -> UComplexRowVector
  | SComplexMatrix _ -> UComplexMatrix
  | SArray (t, _) -> UArray (to_unsized t)

let rec associate ?init:(assocs = Label.Int_label.Map.empty) = function
  | SInt | SReal | SComplex -> assocs
  | SVector e | SRowVector e | SComplexVector e | SComplexRowVector e ->
      Expr.Labelled.associate ~init:assocs e
  | SMatrix (e1, e2) | SComplexMatrix (e1, e2) ->
      Expr.Labelled.(associate ~init:(associate ~init:assocs e1) e2)
  | SArray (st, e) ->
      associate ~init:(Expr.Labelled.associate ~init:assocs e) st

let is_scalar = function SInt | SReal | SComplex -> true | _ -> false
let rec inner_type = function SArray (t, _) -> inner_type t | t -> t

let rec dims_of st =
  match st with
  | SArray (t, _) -> dims_of t
  | SMatrix (d1, d2) -> [d1; d2]
  | SRowVector dim | SVector dim -> [dim]
  | SComplexMatrix (d1, d2) -> [d1; d2]
  | SComplexRowVector dim | SComplexVector dim -> [dim]
  | SInt | SReal | SComplex -> []

let rec get_dims = function
  | SInt | SReal | SComplex -> []
  | SVector d | SRowVector d -> [d]
  | SMatrix (dim1, dim2) -> [dim1; dim2]
  | SComplexVector d | SComplexRowVector d -> [d]
  | SComplexMatrix (dim1, dim2) -> [dim1; dim2]
  | SArray (t, dim) -> dim :: get_dims t

let%expect_test "dims" =
  let open Fmt in
  strf "@[%a@]" (list ~sep:comma string)
    (get_dims (SArray (SMatrix ("x", "y"), "z")))
  |> print_endline ;
  [%expect {| z, x, y |}]
