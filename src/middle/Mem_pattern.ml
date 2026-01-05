open Core
open Core.Poly

(** This type represents whether or not an autodiff type can be represented as
    an Array of Structs (AoS) or as a Struct of Arrays. This applies to
    matrices, vectors, row vectors, and arrays of those types.

    In the C++ this allows us to swap out matrix types from an
    [Eigen::Matrix<stan::math::var_value<double>, Rows, Cols>] to an
    [stan::math::var_value<Eigen::Matrix<double, Rows, Cols>>]. *)
type t = AoS | SoA [@@deriving sexp, compare, map, hash, fold, equal]

let pp ppf = function
  | AoS -> Fmt.string ppf "AoS"
  | SoA -> Fmt.string ppf "SoA"

let lub_mem_pat lst =
  let find_soa mem_pat = mem_pat = SoA in
  if List.exists ~f:find_soa lst then SoA else AoS
