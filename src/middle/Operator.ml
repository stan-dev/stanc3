(** Utilities for Stan's built in operators *)

open Core_kernel

type t =
  | Plus
  | PPlus
  | Minus
  | PMinus
  | Times
  | Divide
  | IntDivide
  | Modulo
  | LDivide
  | EltTimes
  | EltDivide
  | Pow
  | EltPow
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

let pp ppf = function
  | Plus | PPlus -> Fmt.pf ppf "+"
  | Minus | PMinus -> Fmt.pf ppf "-"
  | Times -> Fmt.pf ppf "*"
  | Divide -> Fmt.pf ppf "/"
  | IntDivide -> Fmt.pf ppf "%%/%%"
  | Modulo -> Fmt.pf ppf "%%"
  | LDivide -> Fmt.pf ppf "\\"
  | EltTimes -> Fmt.pf ppf ".*"
  | EltDivide -> Fmt.pf ppf "./"
  | Pow -> Fmt.pf ppf "^"
  | EltPow -> Fmt.pf ppf ".^"
  | Or -> Fmt.pf ppf "||"
  | And -> Fmt.pf ppf "&&"
  | Equals -> Fmt.pf ppf "=="
  | NEquals -> Fmt.pf ppf "!="
  | Less -> Fmt.pf ppf "<"
  | Leq -> Fmt.pf ppf "<="
  | Greater -> Fmt.pf ppf ">"
  | Geq -> Fmt.pf ppf ">="
  | PNot -> Fmt.pf ppf "!"
  | Transpose -> Fmt.pf ppf "'"

let to_string x = Sexp.to_string (sexp_of_t x) ^ "__"

let of_string_opt x =
  try
    String.chop_suffix_exn ~suffix:"__" x |> Sexp.of_string |> t_of_sexp |> Some
  with
  | Sexplib.Conv.Of_sexp_error _ -> None
  | Invalid_argument _ -> None

let stan_math_name = function
  | Plus -> "add"
  | PPlus -> "plus"
  | Minus -> "subtract"
  | PMinus -> "minus"
  | Times -> "multiply"
  (* TODO: this was taken from [Mir_utils.string_of_operators]
     what was the intended behaviour here?
  *)
  | Divide -> "mdivide_right"
  (* | Divide -> "divide" *)
  | IntDivide -> "divide"
  | Modulo -> "modulus"
  | LDivide -> "mdivide_left"
  | EltTimes -> "elt_multiply"
  | EltDivide -> "elt_divide"
  | Pow -> "pow"
  | EltPow -> "pow"
  | Or -> "logical_or"
  | And -> "logical_and"
  | Equals -> "logical_eq"
  | NEquals -> "logical_neq"
  | Less -> "logical_lt"
  | Leq -> "logical_lte"
  | Greater -> "logical_gt"
  | Geq -> "logical_gte"
  | PNot -> "logical_negation"
  | Transpose -> "transpose"
