(** Utilities for Stan's built in operators *)

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

(** The name under which an operator application is stored in the MIR, as a
    [StanLib] call: the constructor name followed by [__], e.g. [Plus__]. *)
let to_string x = Sexplib0.Sexp.to_string (sexp_of_t x) ^ "__"

(** Inverse of [to_string]: [Some op] iff [name] is the MIR name of a built-in
    operator. Written as an explicit table so the encoding is visible here and
    nowhere else. *)
let of_string_opt = function
  | "Plus__" -> Some Plus
  | "PPlus__" -> Some PPlus
  | "Minus__" -> Some Minus
  | "PMinus__" -> Some PMinus
  | "Times__" -> Some Times
  | "Divide__" -> Some Divide
  | "IntDivide__" -> Some IntDivide
  | "Modulo__" -> Some Modulo
  | "LDivide__" -> Some LDivide
  | "EltTimes__" -> Some EltTimes
  | "EltDivide__" -> Some EltDivide
  | "Pow__" -> Some Pow
  | "EltPow__" -> Some EltPow
  | "Or__" -> Some Or
  | "And__" -> Some And
  | "Equals__" -> Some Equals
  | "NEquals__" -> Some NEquals
  | "Less__" -> Some Less
  | "Leq__" -> Some Leq
  | "Greater__" -> Some Greater
  | "Geq__" -> Some Geq
  | "PNot__" -> Some PNot
  | "Transpose__" -> Some Transpose
  | _ -> None
