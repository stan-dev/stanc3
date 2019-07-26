open Core_kernel

type t = Mir_pattern.operator =
  | Plus
  | PPlus
  | Minus
  | PMinus
  | Times
  | Divide
  | Modulo
  | LDivide
  | EltTimes
  | EltDivide
  | Pow
  | Or
  | And
  | Equals
  | NEquals
  | Less
  | Leq
  | Greater
  | Geq
  | PNot
  | Transpose
[@@deriving sexp, hash, compare]

let to_string x = Sexp.to_string (sexp_of_t x) ^ "__"

let of_string_opt x =
  try
    String.chop_suffix_exn ~suffix:"__" x
    |> Sexp.of_string |> t_of_sexp |> Some
  with
  | Sexplib.Conv.Of_sexp_error _ -> None
  | Invalid_argument _ -> None
