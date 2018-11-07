open Core_kernel

type litType = Int | Real | Str

and sourceMap = {
  file: string;
  line_start: int;
  line_end: int;
  col_start: int;
  col_end: int;
}


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

and stantype =
  | SInt
  | SReal
  | SVector of expr option
  | SRowVector of expr option
  | SMatrix of expr * expr option
  | SArray of stantype * expr option

and transformation =
  | Identity
  | Lower of expr
  | Upper of expr
  | LowerUpper of expr * expr
  | LocationScale of expr * expr
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance

and argmod = Data | ANone

and fndef = {
  returntype: stantype option;
  name: string;
  arguments: vardecl list;
  body: statement;
}

and vardecl = stantype * string

and block = (vardecl list) * (statement list)

and mir = {
  functions: fndef list;
  datafields: vardecl list;
  params: vardecl list;
  transformations: string * transformation list;
  ctor: statement list;
  gq: block;
}
[@@deriving sexp, hash]
