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
[@@deriving sexp, hash]

type adtype = Ast.autodifftype [@@deriving sexp, hash]
type sizedtype = expr Ast.sizedtype [@@deriving sexp, hash]
type unsizedtype = Ast.unsizedtype [@@deriving sexp, hash]

(* This directive silences some spurious warnings from ppx_deriving *)
[@@@ocaml.warning "-A"]

type 's statement =
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
      ; st: sizedtype
      ; trans: transformation }
  | FunDef of
      { returntype: unsizedtype option
      ; name: string
      ; arguments: (adtype * string * unsizedtype) list
      ; body: 's }

and 's prog =
  { functionsb: 's
  ; paramsb: 's
  ; datab: 's
  ; modelb: 's
  ; gqb: 's
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, hash]

type stmt_loc = {sloc: string; stmt: stmt_loc statement}
[@@deriving sexp, hash]
