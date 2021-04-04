open Core_kernel

type t =
  | StanLib of string
  | CompilerInternal of Internal_fun.t
  | UserDefined of string
[@@deriving compare, sexp, hash]

let pp ppf = function
  | StanLib s | UserDefined s -> Fmt.string ppf s
  | CompilerInternal internal -> Internal_fun.pp ppf internal
