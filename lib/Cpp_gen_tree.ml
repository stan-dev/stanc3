open Core_kernel

type litType = Int | Real | Str

and cond_op =
  | Equals
  | NEquals
  | Less
  | Leq
  | Greater
  | Geq

and expr =
  | Var of string
  | Lit of litType * string
  | FnApp of string * expr list
  | Cond of expr * cond_op * expr
  | ArrayExpr of expr list (* array literal? *)
  | Indexed of expr * expr list

and infixop =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | LDivide
  | EltTimes
  | EltDivide
  | Exp
  | Or
  | And

and statement =
  | Assignment of {
      assignee: string;
      indices: expr list;
      op: infixop;
      rhs: expr;
    }
  | NRFunApp of string * expr list
  | Break
  | Continue
  | Return of expr
  | Skip
  | IfElse of expr * statement * statement option
  | While of expr * statement
  | For of {
      init: statement;
      cond: expr;
      step: statement;
      body: statement;
    }
  | Block of statement list
  | Decl of vardecl * expr option

and cpptype =
  | SInt
  | SReal
  | SArray of cpptype
  | SVector
  | SRowVector
  | SMatrix
  | SSize_t
  | SString

and autodiff =
  | AVar
  | AData

and mutability =
  | Mutable
  | Immutable
  | ByValue

and argdecl = autodiff * mutability * cpptype * string

and vardecl = cpptype * string

and fndef = {
  returntype: (autodiff * cpptype) option;
  name: string;
  arguments: argdecl list;
  body: statement;
  templates: string list;
}

and cppclass = {classname: string; super: string;
                fields: vardecl list;
                methods: fndef list;
                ctor: (vardecl list) * statement}

and prog = {
  cppclass: cppclass;
  functions: fndef list;
}
[@@deriving sexp, hash]
