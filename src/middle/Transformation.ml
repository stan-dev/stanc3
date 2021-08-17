open Core_kernel
(** Transformations (constraints) for global variable declarations *)
type 'e primitive =
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

type 'e t =
  | Single of 'e primitive
  (* given in CONSTRAINING ORDER
   * e.g. x_con = f(g(x_unc)) is written [f; g]
  *)
  | Chain of (('e primitive) list)  
[@@deriving sexp, compare, map, hash, fold]

let fold_prims f acc = function
  | Single t -> f acc t
  | Chain ts -> List.fold ts ~init:acc ~f:f

let has_check_prim = function
  | Identity | Offset _ | Multiplier _ | OffsetMultiplier _ -> false
  | _ -> true

let has_check (trans: 'e t) = fold_prims (fun x y -> x || has_check_prim y) false trans

let has_transform = function
  | Single Identity -> false
  | _ -> true