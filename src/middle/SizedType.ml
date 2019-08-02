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

let pp f ppf x = Mir_pretty_printer.pp_sizedtype f ppf x
let sint = SInt
let sreal = SReal
let svector e = SVector e
let srowvector e = SRowVector e
let smatrix erow ecol = SMatrix (erow, ecol)
let sarray sty e = SArray (sty, e)

let rec to_unsizedtype = function
  | SInt -> UnsizedType.UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (to_unsizedtype t)

let rec associate ?init:(assocs = Label.Map.empty) = function
  | SInt | SReal -> assocs
  | SVector e | SRowVector e -> Expr.Labelled.associate ~init:assocs e
  | SMatrix (e1, e2) ->
      Expr.Labelled.(associate ~init:(associate ~init:assocs e1) e2)
  | SArray (st, e) ->
      associate ~init:(Expr.Labelled.associate ~init:assocs e) st
