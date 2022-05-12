open Core_kernel
open Core_kernel.Poly

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
type mem_pattern = AoS | SoA [@@deriving sexp, compare, map, hash, fold]

let pp_mem_pattern ppf = function
  | AoS -> Fmt.string ppf "AoS"
  | SoA -> Fmt.string ppf "SoA"

let lub_mem_pat lst =
  let find_soa mem_pat = mem_pat = SoA in
  let any_soa = List.exists ~f:find_soa lst in
  match any_soa with true -> SoA | false -> AoS
