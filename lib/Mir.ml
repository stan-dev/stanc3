open Core_kernel

type litType = Int | Real | Str
[@@deriving sexp, hash]

type sourceMap = {
  file: string;
  line_start: int;
  line_end: int;
  col_start: int;
  col_end: int;
}
[@@deriving sexp, hash]

type expr = Var of string
          | Lit of litType * string
          | FnApp of string * expr list
(*          | IfElse of expr * expr * expr option
                 (* cond   step   body *)
          | Loop of expr * expr * expr
*)
          | ExprList of expr list
          | AssignExpr of string * expr
[@@deriving sexp, hash]
