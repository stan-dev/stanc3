open Core_kernel
open Common

(**
  * This type represents whether or not an autodiff type can be represented 
  * as an Array of Structs (AoS) or as a Struct of Arrays. This applies to 
  * matrices, vectors, row vectors, and arrays of those types.
  * In the C++ this allows us to swap out matrix types from an
  * Eigen::Matrix<stan::math::var_value<double>, Rows, Cols> to an 
  * stan::math::var_value<Eigen::Matrix<double, Rows, Cols>>.
  * (fyi a var in the C++ code is an alias for var_value<double>)
  *
 **)
type mem_type = AoS | SoA [@@deriving sexp, compare, map, hash, fold]

type 'a t =
  | SInt
  | SReal
  | SVector of mem_type * 'a
  | SRowVector of mem_type * 'a
  | SMatrix of mem_type * 'a * 'a
  | SArray of 'a t * 'a
[@@deriving sexp, compare, map, hash, fold]

let rec pp pp_e ppf = function
  | SInt -> Fmt.string ppf "int"
  | SReal -> Fmt.string ppf "real"
  | SVector (_, expr) -> Fmt.pf ppf {|vector%a|} (Fmt.brackets pp_e) expr
  | SRowVector (_, expr) ->
      Fmt.pf ppf {|row_vector%a|} (Fmt.brackets pp_e) expr
  | SMatrix (_, d1_expr, d2_expr) ->
      Fmt.pf ppf {|matrix%a|}
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SArray (st, expr) ->
      Fmt.pf ppf {|array%a|}
        Fmt.(pair ~sep:comma (fun ppf st -> pp pp_e ppf st) pp_e |> brackets)
        (st, expr)

let collect_exprs st =
  let rec aux accu = function
    | SInt | SReal -> List.rev accu
    | SVector (_, expr) | SRowVector (_, expr) -> List.rev @@ (expr :: accu)
    | SMatrix (_, expr1, expr2) -> List.rev @@ (expr1 :: expr2 :: accu)
    | SArray (inner, expr) -> aux (expr :: accu) inner
  in
  aux [] st

let rec to_unsized = function
  | SInt -> UnsizedType.UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (to_unsized t)

let rec associate ?init:(assocs = Label.Int_label.Map.empty) = function
  | SInt | SReal -> assocs
  | SVector (_, expr) | SRowVector (_, expr) ->
      Expr.Labelled.associate ~init:assocs expr
  | SMatrix (_, expr1, expr2) ->
      Expr.Labelled.(associate ~init:(associate ~init:assocs expr1) expr2)
  | SArray (st, e) ->
      associate ~init:(Expr.Labelled.associate ~init:assocs e) st

let is_scalar = function SInt | SReal -> true | _ -> false
let rec inner_type = function SArray (t, _) -> inner_type t | t -> t

let rec dims_of st =
  match st with
  | SArray (t, _) -> dims_of t
  | SMatrix (_, dim1, dim2) -> [dim1; dim2]
  | SRowVector (_, dim) | SVector (_, dim) -> [dim]
  | SInt | SReal -> []

let rec get_dims st =
  match st with
  | SInt | SReal -> []
  | SVector (_, dim) | SRowVector (_, dim) -> [dim]
  | SMatrix (_, dim1, dim2) -> [dim1; dim2]
  | SArray (t, dim) -> dim :: get_dims t

let%expect_test "dims" =
  let open Fmt in
  strf "@[%a@]" (list ~sep:comma string)
    (get_dims (SArray (SMatrix (SoA, "x", "y"), "z")))
  |> print_endline ;
  [%expect {| z, x, y |}]
