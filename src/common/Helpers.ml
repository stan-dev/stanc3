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

let lub_mem_pat lst =
  let find_soa mem_pat = mem_pat = SoA in
  let any_soa = List.exists ~f:find_soa lst in
  match any_soa with true -> SoA | false -> AoS

let option_or_else ~if_none x = Option.first_some x if_none
let on_snd f (x, y) = (x, f y)
let on_fst f (x, y) = (f x, y)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let pp_builtin_syntax = Fmt.(string |> styled `Yellow)
let pp_keyword = Fmt.(string |> styled `Blue)
let pp_angle_brackets pp_v ppf v = Fmt.pf ppf "@[<1><%a>@]" pp_v v
let pp_brackets_postfix pp_e ppf = Fmt.pf ppf {|%a[]|} pp_e
