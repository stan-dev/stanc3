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

type litType = Int | Real | Str

and operator =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Or
  | And
  | Equals
  | NEquals
  | Less
  | Leq
  | Greater
  | Geq

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
  | FunApp of string * expr list
  | BinOp of expr * operator * expr
  | TernaryIf of expr * expr * expr
  | Indexed of expr * index list
[@@deriving sexp, hash, map, compare]

type adtype = Ast.autodifftype [@@deriving sexp, hash, map]
type sizedtype = expr Ast.sizedtype [@@deriving sexp, hash, map]
type unsizedtype = Ast.unsizedtype [@@deriving sexp, hash, map]

(* This directive silences some spurious warnings from ppx_deriving *)
[@@@ocaml.warning "-A"]

type constraint_check =
  {ccfunname: string; ccvid: string; cctype: sizedtype; ccargs: expr list}

and fun_arg_decl = (adtype * string * unsizedtype) list

and 's statement =
  | Assignment of expr * expr
  | TargetPE of expr
  | NRFunApp of string * expr list
  | Check of constraint_check
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
  | Decl of {decl_adtype: adtype; decl_id: string; decl_type: unsizedtype}
  | FunDef of
      { fdrt: unsizedtype option
      ; fdname: string
      ; fdargs: fun_arg_decl
      ; fdbody: 's }
[@@deriving sexp, hash, map]

(** A "top var" is a global variable visible to the I/O of Stan.
   Local vs. Global vardecls
   There are "local" (i.e. not top-level; not read in or written out anywhere) variable
   declarations that do not allow transformations. These are the only kind allowed in
   the model block, and any declarations in a Block will also be local.
   There are also then top-level ones, which are the only thing you can
   write in both the parameters and data block. The generated quantities block allows both
   types of variable declarations and, worse, mixes in top-level ones with normal ones.
   We'll need to scan the list of declarations for top-level ones and essentially remove them
   from the block. The AST has an `is_global` flag that also tracks this.
*)
type top_var_decl =
  {tvident: string; tvtype: sizedtype; tvtrans: transformation; tvloc: string}
[@@deriving sexp]

type top_var_table = (string, top_var_decl) Map.Poly.t [@@deriving sexp]

type 's prog =
  { functionsb: 's
  ; datavars: top_var_table
  ; tdatab: top_var_table * 's
  ; modelb: top_var_table * 's
  ; gqb: top_var_table * 's
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, map]

type stmt_loc =
  {sloc: string sexp_opaque [@compare.ignore]; stmt: stmt_loc statement}
[@@deriving sexp, hash, map]

type stmt_loc_num =
  { slocn: string sexp_opaque [@compare.ignore]
  ; stmtn: stmt_loc_num statement
  ; num: int }
[@@deriving sexp, hash, map]

(* ===================== Some helper functions ====================== *)

(** Dives into any number of nested blocks and lists, but will not recurse other
    places statements occur in the MIR (e.g. loop bodies) *)
let rec map_toplevel_stmts f {sloc; stmt} =
  match stmt with
  | Block ls -> {stmt= Block (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | SList ls -> {stmt= SList (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | _ -> f {sloc; stmt}

let tvdecl_to_decl {tvident; tvtype; tvloc; _} = (tvident, tvtype, tvloc)

let rec unnumbered_statement_of_numbered_statement {stmtn; slocn; _} =
  { stmt= map_statement unnumbered_statement_of_numbered_statement stmtn
  ; sloc= slocn }

let numbered_statement_of_unnumbered_statement {stmt; sloc} =
  failwith "TODO: not yet implemented"

(** Forgetful function from numbered to unnumbered programs *)
let unnumbered_prog_of_numbered_prog p =
  map_prog unnumbered_statement_of_numbered_statement p

(** Decorate statements in Mir with unique numbers *)
let numbered_prog_of_unnumbered_prog p =
  map_prog numbered_statement_of_unnumbered_statement p
