open Core_kernel

type t =
  | FnLength
  | FnMakeArray
  | FnMakeRowVec
  | FnNegInf
  (* In AST_to_MIR being used as StanLib *)
  | FnReadData
  (* XXX move these to a backend specific file?*)
  | FnReadParam of string option
  | FnWriteParam
  | FnValidateSize
  | FnValidateSizeSimplex
  | FnValidateSizeUnitVector
  | FnConstrain of string
  | FnUnconstrain of string
  | FnCheck of string
  | FnPrint
  | FnReject
  | FnResizeToMatch
  | FnNaN
  | FnDeepCopy
[@@deriving sexp, hash, compare]

let to_string x = Sexp.to_string (sexp_of_t x) ^ "__"
let pp ppf internal = Fmt.string ppf (to_string internal)

let of_string_opt x =
  try
    String.chop_suffix_exn ~suffix:"__" x
    |> Sexp.of_string |> t_of_sexp |> Some
  with
  | Sexplib.Conv.Of_sexp_error _ -> None
  | Invalid_argument _ -> None
