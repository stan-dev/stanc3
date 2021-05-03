open Core_kernel

type 'propto suffix = FnPlain | FnRng | FnLpdf of 'propto | FnTarget
[@@deriving compare, sexp, hash, map]

type t =
  | StanLib of string * bool suffix
  | CompilerInternal of Internal_fun.t
  | UserDefined of string * bool suffix
[@@deriving compare, sexp, hash]

let suffix_from_name fname =
  let is_suffix suffix = Core_kernel.String.is_suffix ~suffix fname in
  if is_suffix "_rng" then FnRng
  else if is_suffix "_lp" then FnTarget
  else if is_suffix "_lupdf" || is_suffix "_lupmf" then FnLpdf true
  else if is_suffix "_lpdf" || is_suffix "_lpmf" then FnLpdf false
  else if
    is_suffix "_log"
    && not
         ( is_suffix "_cdf_log" || is_suffix "_ccdf_log"
         || fname = "multiply_log"
         || fname = "binomial_coefficient_log" )
  then FnLpdf false
  else FnPlain

let pp ppf = function
  | StanLib (s, FnLpdf true) | UserDefined (s, FnLpdf true) ->
      Fmt.string ppf
        (Utils.with_unnormalized_suffix s |> Option.value ~default:s)
  | StanLib (s, _) | UserDefined (s, _) -> Fmt.string ppf s
  | CompilerInternal internal -> Internal_fun.pp ppf internal
