type t = StanLib | CompilerInternal | UserDefined | Closure
[@@deriving compare, sexp, hash]

type suffix = FnPure | FnRng | FnLpdf | FnTarget
[@@deriving compare, sexp, hash]

let suffix_from_name fname =
  let is_suffix suffix = Core_kernel.String.is_suffix ~suffix fname in
  if is_suffix "_rng" then FnRng
  else if is_suffix "_lp" then FnTarget
  else if
    is_suffix "_lpdf" || is_suffix "_lpmf"
    || (is_suffix "_log" && not (is_suffix "_cdf_log" || is_suffix "_ccdf_log"))
    || is_suffix "_lupdf" || is_suffix "_lupmf"
  then FnLpdf
  else FnPure
