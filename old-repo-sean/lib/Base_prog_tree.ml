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

and 'd statement =
  | Assignment of {
      assignee: string;
      indices: expr list;
      op: infixop;
      rhs: expr;
    }
  | NRFnApp of string * expr list
  | Break
  | Continue
  | Return of expr
  | Skip
  | IfElse of expr * 'd statement * 'd statement option
  | While of expr * 'd statement
  | For of {
      init: 'd statement;
      cond: expr;
      step: 'd statement;
      body: 'd statement;
    }
  | Block of 'd statement list
  | Decl of 'd * expr option
[@@deriving sexp, hash]

let map_expr expr_f e =
  let map_expr_list = List.map ~f:expr_f in
  match e with
  | FnApp(n, args) -> FnApp(n, map_expr_list args)
  | Cond(e1, cond_op, e2) -> Cond(expr_f e1, cond_op, expr_f e2)
  | ArrayExpr(es) -> ArrayExpr(map_expr_list es)
  | Indexed(lhs, indices) -> Indexed(expr_f lhs, map_expr_list indices)
  | e -> e

let map_statement decl_f statement_f expr_f s =
  let map_expr_list = List.map ~f:expr_f in
  match s with
  | Assignment a ->
    Assignment {a with indices = map_expr_list a.indices; rhs = expr_f a.rhs}
  | NRFnApp(n, args) -> NRFnApp(n, map_expr_list args)
  | Return(e) -> Return(expr_f e)
  | IfElse(cond, ifbranch, elsebranch) ->
    IfElse(expr_f cond, statement_f ifbranch,
           Option.map ~f:statement_f elsebranch)
  | While(cond, body) -> While(expr_f cond, statement_f body)
  | For({init; cond; step; body}) ->
    For({init=statement_f init; cond=expr_f cond; step = statement_f step;
         body=statement_f body})
  | Block(statements) -> Block(List.map ~f:statement_f statements)
  | Decl(d, rhs) -> Decl(decl_f d, Option.map ~f:expr_f rhs)
  | s -> statement_f s
