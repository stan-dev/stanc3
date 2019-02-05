(** The Middle Intermediate Representation, which program transformations
    operate on *)

open Core_kernel

(*
   XXX Missing:
   * TODO? foreach loops - matrix vs array (fine because of get_base1?)
   * TODO during optimization:
       - mark for loops with known bounds
       - mark FnApps as containing print or reject
*)

let _counter = ref 0

let gensym () =
  _counter := !_counter + 1 ;
  sprintf "sym%d" !_counter

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

type constraint_check =
  {cfname: string; cvarname: string; ctype: sizedtype; cargs: expr list}

and 's statement =
  | Assignment of expr * expr
  | NRFnApp of string * expr list
  | Check of constraint_check
  | MarkLocation of string
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
  | Decl of {adtype: adtype; vident: string; st: sizedtype}
  | FunDef of
      { returntype: unsizedtype option
      ; name: string
      ; arguments: (adtype * string * unsizedtype) list
      ; body: 's }
[@@deriving sexp, hash]

type tvdecl =
  {tvident: string; tvtype: sizedtype; tvtrans: transformation; tvloc: string}
[@@deriving sexp]

type tvtable = (string, tvdecl) Map.Poly.t [@@deriving sexp]

type 's prog =
  { functionsb: 's
  ; datavars: tvtable
  ; tdatab: tvtable * 's
  ; modelb: tvtable * 's
  ; gqb: tvtable * 's
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp]

type stmt_loc = {sloc: string sexp_opaque; stmt: stmt_loc statement}
[@@deriving sexp, hash]

(* ===================== Some helper functions ====================== *)

(** Dives into any number of nested blocks and lists, but will not recurse other
    places statements occur in the MIR (e.g. loop bodies) *)
let rec map_toplevel_stmts f {sloc; stmt} =
  match stmt with
  | Block ls -> {stmt= Block (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | SList ls -> {stmt= SList (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | _ -> f {sloc; stmt}

let tvdecl_to_decl {tvident; tvtype; _} = (tvident, tvtype)
