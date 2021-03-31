open Core_kernel

type 'internal t =
  | StanLib of string
  | CompilerInternal of 'internal
  | UserDefined of string
[@@deriving compare, sexp, hash]

let pp ppf = function
  | StanLib s | UserDefined s -> Fmt.string ppf s
  | CompilerInternal internal -> Internal_fun.pp ppf internal
