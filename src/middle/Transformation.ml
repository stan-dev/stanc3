(** Transformations (constraints) for global variable declarations *)
type 'e t =
  | Identity
  | Lower of 'e
  | Upper of 'e
  | LowerUpper of 'e * 'e
  | Offset of 'e
  | Multiplier of 'e
  | OffsetMultiplier of 'e * 'e
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance
[@@deriving sexp, compare, map, hash, fold]

let has_check = function
  | Identity | Offset _ | Multiplier _ | OffsetMultiplier _ -> false
  | _ -> true
