open Core_kernel

(*
   XXX Missing:
   * TODO? foreach loops - matrix vs array (fine because of get_base1?)
   * TODO during optimization:
       - mark for loops with known bounds
       - mark FnApps as containing print or reject
*)

type litType = Int | Real | Str

and operator = Ast.operator

and truncation = expr Ast.truncation

and transformation = expr Ast.transformation

and index =
  | All
  | Single of expr
  | Upfrom of expr
  | Downfrom of expr
  | Between of expr * expr
  | MultiIndex of expr

and expr =
  | Var of string
  | Lit of litType * string
  | FnApp of string * expr list
  | BinOp of expr * operator * expr
  | TernaryIf of expr * expr * expr
  | Indexed of expr * index list
[@@deriving sexp, hash, map]

(* Encode both sized and unsized this way... effectiveness TBD*)
type stantype =
  | SInt
  | SReal
  | SArray of expr option * stantype
  | SVector of expr option
  | SRowVector of expr option
  | SMatrix of (expr * expr) option

and loc = string

and vardecl = {vident: string; st: stantype; trans: transformation; loc: loc}

and argdecl = string * stantype

and 's statement =
  | Assignment of expr * expr
  | NRFnApp of string * expr list
  | Break
  | Continue
  | Return of expr option
  | Skip
  | IfElse of expr * 's * 's option
  | While of expr * 's
  | For of {loopvar: expr; lower: expr; upper: expr; body: 's}
  | Block of 's list
  | Decl of vardecl * expr option
[@@deriving sexp, hash, map]

type 's udf_defn =
  {returntype: stantype option; name: string; arguments: argdecl list; body: 's}

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
[@@deriving sexp, hash, map]

type stmt_loc = {sloc: loc; stmt: stmt_loc statement}
[@@deriving sexp, hash, map]
