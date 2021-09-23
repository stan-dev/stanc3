type 'e t =
  | Native
  | Offset of 'e
  | Multiplier of 'e
  | OffsetMultiplier of 'e * 'e
[@@deriving sexp, compare, map, hash, fold]
