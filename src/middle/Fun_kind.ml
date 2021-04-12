open Core_kernel

type 'e t =
  | StanLib of string
  | CompilerInternal of 'e Internal_fun.t
  | UserDefined of string
[@@deriving compare, sexp, hash, map, fold]

let pp pp_expr ppf = function
  | StanLib s | UserDefined s -> Fmt.string ppf s
  | CompilerInternal internal -> Internal_fun.pp pp_expr ppf internal
