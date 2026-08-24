(** Utilities for Stan's built in operators *)

open Std

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
[@@deriving sexp, compare]

let is_cmp = function
  | Equals | NEquals | Less | Leq | Greater | Geq -> true
  | Plus | PPlus | Minus | PMinus | Times | Divide | IntDivide | Modulo
   |LDivide | EltTimes | EltDivide | Pow | EltPow | Or | And | PNot | Transpose
    ->
      false

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

open Sexplib0

let to_string x = Sexp.to_string (sexp_of_t x) ^ "__"

let of_string_opt x =
  let open Option.Syntax in
  try
    let+ ssexp = String.chop_suffix ~suffix:"__" x in
    let sexp = Sexp_conv.sexp_of_string ssexp in
    t_of_sexp sexp
  with
  | Sexp_conv.Of_sexp_error _ -> None
  | Invalid_argument _ -> None
