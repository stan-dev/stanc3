open Core_kernel

type expr = Var of string
          | IntLit of int
          | NumLit of float
          | StrLit of string
          | FnApp of string * expr list
          | If of expr * expr * expr
          | AssignExpr of string * expr
          | ExprList of expr list
[@@deriving sexp, hash]

type stanProg = Prog of expr
              | None
[@@deriving sexp]
