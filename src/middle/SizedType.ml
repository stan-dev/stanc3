type 'a t = 'a Mir_pattern.sizedtype =
  | SInt
  | SReal
  | SVector of 'a
  | SRowVector of 'a
  | SMatrix of 'a * 'a
  | SArray of 'a t * 'a
[@@deriving sexp, compare, map, hash, fold]

module Make_traversable = Mir_pattern.Make_traversable_sizedtype
module Make_traversable2 = Mir_pattern.Make_traversable_sizedtype2

let rec pp pp_e ppf = function 
  | SInt -> Fmt.string ppf "int"
  | SReal -> Fmt.string ppf "real"
  | SVector expr -> Fmt.pf ppf {|vector%a|} (Fmt.brackets pp_e) expr
  | SRowVector expr -> Fmt.pf ppf {|row_vector%a|} (Fmt.brackets pp_e) expr
  | SMatrix (d1_expr, d2_expr) ->
      Fmt.pf ppf {|matrix%a|}
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SArray (st, expr) ->
      Fmt.pf ppf {|array%a|}
        Fmt.(
          pair ~sep:comma (fun ppf st -> pp pp_e ppf st) pp_e
          |> brackets)
        (st, expr)
let sint = SInt
let sreal = SReal
let svector e = SVector e
let srowvector e = SRowVector e
let smatrix erow ecol = SMatrix (erow, ecol)
let sarray sty e = SArray (sty, e)

let collect_exprs st =
  let rec aux accu = function
    | SInt | SReal -> List.rev accu
    | SVector e | SRowVector e -> List.rev @@ (e :: accu)
    | SMatrix (e1, e2) -> List.rev @@ (e1 :: e2 :: accu)
    | SArray (inner, e) -> aux (e :: accu) inner
  in
  aux [] st

let rec to_unsizedtype = function
  | SInt -> UnsizedType.UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (to_unsizedtype t)

let rec associate ?init:(assocs = Int_label.Map.empty) = function
  | SInt | SReal -> assocs
  | SVector e | SRowVector e -> Expr.Labelled.associate ~init:assocs e
  | SMatrix (e1, e2) ->
      Expr.Labelled.(associate ~init:(associate ~init:assocs e1) e2)
  | SArray (st, e) ->
      associate ~init:(Expr.Labelled.associate ~init:assocs e) st
