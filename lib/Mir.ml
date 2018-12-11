open Core_kernel

(*
   XXX Missing:
   * TODO? foreach loops - matrix vs array (fine because of get_base1?)
   * TODO during optimization:
       - mark for loops with known bounds
       - mark FnApps as containing print or reject
*)

type litType = Int | Real | Str

and cond_op = Equals | NEquals | Less | Leq | Greater | Geq

and expr =
  | Var of string
  | Lit of litType * string
  | FnApp of string * expr list
  | Cond of expr * cond_op * expr
  | ArrayLiteral of expr list
  | Indexed of expr * expr list
  (* Different type constructor for indexing by an int array *)
  | MultiIndexed of expr * expr list

(* Encode both sized and unsized this way *)
and stantype =
  | SInt
  | SReal
  | SArray of expr option * stantype
  | SVector of expr option
  | SRowVector of expr option
  | SMatrix of (expr * expr) option

and loc = string

and vardecl = {vident: string; st: stantype; trans: transformation; loc: loc}

and argdecl = string * stantype

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
  | NoTransformation

and 's statement =
  | Assignment of {assignee: string; indices: expr list; rhs: expr}
  | NRFnApp of string * expr list
  | Break
  | Continue
  | Return of expr
  | Skip
  | IfElse of expr * 's * 's option
  | While of expr * 's
  | For of {loopvar: expr; lower: expr; upper: expr; body: 's}
  | Block of 's list
  | Decl of vardecl * expr option
[@@deriving sexp, hash]

let map_expr expr_f e =
  let map_expr_list = List.map ~f:expr_f in
  match e with
  | FnApp (n, args) -> FnApp (n, map_expr_list args)
  | Cond (e1, cond_op, e2) -> Cond (expr_f e1, cond_op, expr_f e2)
  | ArrayLiteral es -> ArrayLiteral (map_expr_list es)
  | Indexed (lhs, indices) -> Indexed (expr_f lhs, map_expr_list indices)
  | e -> e

let map_statement statement_f expr_f s =
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
  | For {loopvar; lower; upper; body} ->
      For
        { loopvar= expr_f loopvar
        ; lower= expr_f lower
        ; upper= expr_f upper
        ; body= statement_f body }
  | Block statements -> Block (List.map ~f:statement_f statements)
  | Decl (d, rhs) -> Decl (d, Option.map ~f:expr_f rhs)
  | Break -> Break
  | Continue -> Continue
  | Skip -> Skip

type 's udf_defn =
  { returntype: stantype option
  ; name: string
  ; arguments: argdecl list
  ; body: 's }

and 's prog =
  { functions: 's udf_defn list
  ; params: vardecl list
  ; data: vardecl list
  ; model: 's
  ; gq: 's
  ; tdata: 's
  ; tparam: 's
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, hash]

type stmt_loc = {sloc: loc; stmt: stmt_loc statement}

let map_udf_defn sf ef udf = {udf with body= map_statement sf ef udf.body}

let map_prog sf ef p =
  { p with
    functions= List.map ~f:(map_udf_defn sf ef) p.functions
  ; model= map_statement sf ef p.model
  ; gq= map_statement sf ef p.gq
  ; tdata= map_statement sf ef p.tdata
  ; tparam= map_statement sf ef p.tparam }
