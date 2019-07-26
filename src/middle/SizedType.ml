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

let rec remove_size = function
  | SInt -> UnsizedType.UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (remove_size t)
