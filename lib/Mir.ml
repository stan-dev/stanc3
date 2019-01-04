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

and adtype = Ast.autodifftype [@@deriving sexp, hash]

(* Encode both sized and unsized this way... effectiveness TBD*)
(* Actually probably just use unsizedtype from AST directly;
   maybe also sizedtype with our own expr?
*)
type stantype =
  | SInt
  | SReal
  | SArray of expr option * stantype
  | SVector of expr option
  | SRowVector of expr option
  | SMatrix of (expr * expr) option

and loc = string

and 's statement =
  | Assignment of expr * expr
  | NRFnApp of string * expr list
  | Check of string * expr list
  | Break
  | Continue
  | Return of expr option
  | Skip
  | IfElse of expr * 's * 's option
  | While of expr * 's
  (* XXX Collapse with For? *)
  | For of {loopvar: expr; lower: expr; upper: expr; body: 's}
  (* A Block for now corresponds tightly with a C++ block:
     variables declared within it have local scope and are garbage collected
     when the block ends.*)
  | Block of 's list
  (* An SList does not share any of Block's semantics - it is just multiple
     (ordered!) statements*)
  | SList of 's list
  | Decl of
      { adtype: adtype
      ; vident: string
      ; st: stantype
      ; trans: transformation }
  | FunDef of
      { returntype: stantype option
      ; name: string
      ; arguments: (adtype * string * stantype) list
      ; body: 's }
[@@deriving sexp, hash]

and 's prog =
  { functionsb: 's
  ; paramsb: 's
  ; datab: 's
  ; modelb: 's
  ; gqb: 's
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, hash, map]

type stmt_loc = {sloc: loc; stmt: stmt_loc statement} [@@deriving sexp, hash]
