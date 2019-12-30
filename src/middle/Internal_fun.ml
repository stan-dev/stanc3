open Core_kernel

type t =
  | FnLength
  | FnMakeArray
  | FnMakeRowVec
  | FnNegInf
  | FnReadData
  (* XXX move these to a backend specific file?*)
  | FnReadParam
  | FnWriteParam
  | FnValidateSize
  | FnValidateSizeSimplex
  | FnValidateSizeUnitVector
  | FnConstrain
  | FnUnconstrain
  | FnCheck
  | FnPrint
  | FnReject
  | FnResizeToMatch
  | FnNaN
[@@deriving sexp]

let to_string x = Sexp.to_string (sexp_of_t x) ^ "__"

let of_string_opt x =
  try
    String.chop_suffix_exn ~suffix:"__" x
    |> Sexp.of_string |> t_of_sexp |> Some
  with
  | Sexplib.Conv.Of_sexp_error _ -> None
  | Invalid_argument _ -> None
