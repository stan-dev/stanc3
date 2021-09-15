open Core_kernel

(** A primitive (basic) transformation. One or many of these makes up a transformation *)
type 'e primitive =
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

(** Transformations (constraints) for global variable declarations *)
type 'e t =
  | Identity
  | Single of 'e primitive
  (* given in CONSTRAINING ORDER
   * e.g. x_con = f(g(x_unc)) is written [g;f]
   * = "When constraining, first do g, then do f."
   *)
  | Chain of 'e primitive list
[@@deriving sexp, compare, map, hash, fold]

let fold_prims f acc = function
  | Identity -> acc
  | Single t -> f acc t
  | Chain ts -> List.fold ts ~init:acc ~f

let primitive_has_check = function
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> false
  | _ -> true

let has_transform = function Identity -> false | _ -> true
let list = function Identity -> [] | Single t -> [t] | Chain ts -> ts

let same_transform t1 t2 =
  match (t1, t2) with
  | Lower _, Lower _
   |Upper _, Upper _
   |LowerUpper _, LowerUpper _
   |Offset _, Offset _
   |Multiplier _, Multiplier _
   |OffsetMultiplier _, OffsetMultiplier _ ->
      true
  | _, _ -> t1 = t2

let contains (trans : 'e t) (t : 'e primitive) =
  match trans with
  | Identity -> false
  | Single t' -> same_transform t t'
  | Chain ts -> List.mem ts t ~equal:same_transform
