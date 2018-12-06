open Core_kernel

(*
   XXX Missing:
   * sizes for containers on declarations ("sizedtypes")
   * foreach loops - matrix vs array
   * type of index for multi-index by int array
   * location strings for each statement
   * transformations for vardecls
   * different nodes for when I know the bounds of a for loop or not
   * somehow mark FnApps as containing print or reject
*)


type litType = Int | Real | Str

and cond_op = Equals | NEquals | Less | Leq | Greater | Geq

and expr =
  | Var of string
  | Lit of litType * string
  | FnApp of string * expr list
  | Cond of expr * cond_op * expr
  | ArrayExpr of expr list
  (* array literal? *)
  | Indexed of expr * expr list

and stantype =
  | SInt
  | SReal
  | SArray of expr * stantype
  | SVector of expr
  | SRowVector of expr
  | SMatrix of expr * expr

and loc = string

and vardecl = string * stantype * loc

and statement =
  | Assignment of {assignee: string; indices: expr list; rhs: expr}
  | NRFnApp of string * expr list
  | Break
  | Continue
  | Return of expr
  | Skip
  | IfElse of expr * statement * statement option
  | While of expr * statement
  | For of
      { init: statement
      ; cond: expr
      ; step: statement
      ; body: statement }
  | Block of statement list
  | Decl of vardecl * expr option
[@@deriving sexp, hash]

let map_expr expr_f e =
  let map_expr_list = List.map ~f:expr_f in
  match e with
  | FnApp (n, args) -> FnApp (n, map_expr_list args)
  | Cond (e1, cond_op, e2) -> Cond (expr_f e1, cond_op, expr_f e2)
  | ArrayExpr es -> ArrayExpr (map_expr_list es)
  | Indexed (lhs, indices) -> Indexed (expr_f lhs, map_expr_list indices)
  | e -> e

let map_statement decl_f statement_f expr_f s =
  let map_expr_list = List.map ~f:expr_f in
  match s with
  | Assignment a ->
      Assignment {a with indices= map_expr_list a.indices; rhs= expr_f a.rhs}
  | NRFnApp (n, args) -> NRFnApp (n, map_expr_list args)
  | Return e -> Return (expr_f e)
  | IfElse (cond, ifbranch, elsebranch) ->
      IfElse
        ( expr_f cond
        , statement_f ifbranch
        , Option.map ~f:statement_f elsebranch )
  | While (cond, body) -> While (expr_f cond, statement_f body)
  | For {init; cond; step; body} ->
      For
        { init= statement_f init
        ; cond= expr_f cond
        ; step= statement_f step
        ; body= statement_f body }
  | Block statements -> Block (List.map ~f:statement_f statements)
  | Decl (d, rhs) -> Decl (decl_f d, Option.map ~f:expr_f rhs)
  | s -> statement_f s

type udf_defn =
  { returntype: stantype option
  ; name: string
  ; arguments: vardecl list
  ; body: statement }

and prog =
  { functions: udf_defn list
  ; params: vardecl list
  ; data: vardecl list
  ; model: statement
  ; gq: statement
  ; tdata: statement
  ; tparam: statement
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, hash]

let id x = x

let map_udf_defn sf ef udf =
  {udf with body= map_statement id sf ef udf.body}

let map_prog sf ef p =
  { p with
    functions= List.map ~f:(map_udf_defn sf ef) p.functions
  ; model= map_statement id sf ef p.model
  ; gq= map_statement id sf ef p.gq
  ; tdata= map_statement id sf ef p.tdata
  ; tparam= map_statement id sf ef p.tparam }
