open Core_kernel
open Common

type 'a t =
  | SInt
  | SReal
  | SVector of 'a
  | SRowVector of 'a
  | SMatrix of 'a * 'a
  | SSparseMatrix of 'a * 'a * 'a * 'a
  | SStaticSparseMatrix of 'a * 'a
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
  | SSparseMatrix (_, _, d1_expr, d2_expr) ->
      Fmt.pf ppf {|matrix%a|}
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SStaticSparseMatrix (d1_expr, d2_expr) ->
      Fmt.pf ppf {|sparse_matrix%a|}
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
    | SArray (st, expr) ->
      Fmt.pf ppf {|array%a|}
        Fmt.(pair ~sep:comma (fun ppf st -> pp pp_e ppf st) pp_e |> brackets)
        (st, expr)

let collect_exprs st =
  let rec aux accu = function
    | SInt | SReal -> List.rev accu
    | SVector e | SRowVector e -> List.rev @@ (e :: accu)
    | SMatrix (e1, e2) -> List.rev @@ (e1 :: e2 :: accu)
    | SSparseMatrix (e1, e2, e3, e4) -> List.rev @@ (e1 :: e2 :: e3 :: e4 :: accu)
    | SStaticSparseMatrix (e1, e2) -> List.rev @@ (e1 :: e2 :: accu)
    | SArray (inner, e) -> aux (e :: accu) inner
  in
  aux [] st

let rec to_unsized = function
  | SInt -> UnsizedType.UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SSparseMatrix _ -> USparseMatrix
  | SStaticSparseMatrix _ -> USparseMatrix
  | SArray (t, _) -> UArray (to_unsized t)

let rec associate ?init:(assocs = Label.Int_label.Map.empty) = function
  | SInt | SReal -> assocs
  | SVector e | SRowVector e -> Expr.Labelled.associate ~init:assocs e
  | SMatrix (e1, e2) ->
      Expr.Labelled.(associate ~init:(associate ~init:assocs e1) e2)
  | SSparseMatrix (e1, e2, e3, e4) ->
      Expr.Labelled.(associate ~init:(associate ~init:(associate ~init:(associate ~init:assocs e1) e2) e3) e4)
  | SStaticSparseMatrix (e1, e2) ->
      Expr.Labelled.(associate ~init:(associate ~init:assocs e1) e2)
  | SArray (st, e) ->
      associate ~init:(Expr.Labelled.associate ~init:assocs e) st

let is_scalar = function SInt | SReal -> true | _ -> false

let rec dims_of st =
  match st with
  | SArray (t, _) -> dims_of t
  | SMatrix (d1, d2) -> [d1; d2]
  | SSparseMatrix (nz_rows, nz_cols, d1, d2) -> [nz_rows; nz_cols; d1; d2]
  | SStaticSparseMatrix (d1, d2) -> [d1; d2]
  | SRowVector dim | SVector dim -> [dim]
  | SInt | SReal -> []

let rec get_dims st
  : [ `Dim of Expr.Typed.t 
    | `SparseIterator of (Expr.Typed.t * Expr.Typed.t * Expr.Typed.t)] List.t =
  match st with
  | SInt | SReal -> []
  | SVector d | SRowVector d -> [`Dim d]
  | SMatrix (dim1, dim2) -> [`Dim dim1; `Dim dim2]
  | SSparseMatrix (nz_rows, nz_cols, _, _) -> 
    [`SparseIterator (nz_rows, nz_cols, 
       Expr.Helpers.internal_funapp FnNonZero [nz_rows] Expr.Typed.Meta.empty)]
  | SStaticSparseMatrix (dim1, dim2) -> [`SparseIterator (dim1, dim2, dim1)]
| SArray (t, dim) -> `Dim dim :: get_dims t
