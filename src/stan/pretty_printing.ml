(** Some helpers to produce nice error messages *)

open Ast

let rec pretty_print_unsizedtype = function
  | Int -> "int"
  | Real -> "real"
  | Vector -> "vector"
  | RowVector -> "row_vector"
  | Matrix -> "matrix"
  | Array ut -> pretty_print_unsizedtype ut ^ "[]"
  | Fun (argtypes, rt) ->
      "("
      ^ String.concat ", " (List.map pretty_print_argtype argtypes)
      ^ ") => " ^ pretty_print_returntype rt
  | PrimitiveFunction -> "Stan Math function"

and pretty_print_returntype = function
  | ReturnType x -> pretty_print_unsizedtype x
  | Void -> "void"

and pretty_print_argtype = function
  | ob, ut -> pretty_print_originblock ob ^ pretty_print_unsizedtype ut

and pretty_print_originblock = function Data | TData -> "data " | _ -> ""

let pretty_print_expressiontype = function
  | _, ut -> pretty_print_unsizedtype ut

let pretty_print_opt_expressiontype = function
  | None -> "unknown"
  | Some x -> pretty_print_expressiontype x

(* TODO: implement more pretty printing functions for generating error messages *)

let pretty_print_infixop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | LDivide -> "\\"
  | EltTimes -> ".*"
  | EltDivide -> "./"
  | Exp -> "^"
  | Or -> "||"
  | And -> "&&"
  | Equals -> "=="
  | NEquals -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

let pretty_print_prefixop = function
  | Not -> "!"
  | UMinus -> "-"
  | UPlus -> "+"

let pretty_print_postfixop = function Transpose -> "'"

let pretty_print_assignmentoperator = function
  | Assign -> "="
  | PlusAssign -> "+="
  | MinusAssign -> "-="
  | TimesAssign -> "*="
  | DivideAssign -> "/="
  | EltTimesAssign -> ".*="
  | EltDivideAssign -> "./="
  | ArrowAssign -> "<-"
