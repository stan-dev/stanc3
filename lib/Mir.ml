open Core_kernel

type litType = Int | Real | Str

(* Probably need a way to go from something like a sourcemap to
   current_statement_begin__?*)
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
  (* Do we need RowVector? CondFunApp?*)

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
      body: statement}
  | Block of statement list
  | Decl of vardecl * expr option

and fndef = {
  returntype: string;
  name: string;
  arguments: vardecl list;
  body: statement;
}

and vardecl = string * string

and mir = {
  functions: fndef list;
  datafields: vardecl list;
  params: vardecl list;
  methods: fndef list;
  ctor: (vardecl list) * statement list;
}
[@@deriving sexp, hash]
