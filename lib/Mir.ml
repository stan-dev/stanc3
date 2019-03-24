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

type litType = Int | Real | Str [@@deriving sexp, hash]

and 'e index =
  | All
  | Single of 'e
  (*
  | MatrixSingle of 'e
 *)
  | Upfrom of 'e
  | Downfrom of 'e
  | Between of 'e * 'e
  | MultiIndex of 'e

(** XXX
*)
and 'e expr =
  | Var of string
  | Lit of litType * string
  | FunApp of string * 'e list
  | TernaryIf of 'e * 'e * 'e
  (* XXX And and Or nodes*)
  | Indexed of 'e * 'e index list
[@@deriving sexp, hash, map]

type unsizedtype = Ast.unsizedtype [@@deriving sexp, hash]
type 'e sizedtype = 'e Ast.sizedtype [@@deriving sexp, hash, map]
type autodifftype = Ast.autodifftype [@@deriving sexp, hash]

(* This directive silences some spurious warnings from ppx_deriving *)
[@@@ocaml.warning "-A"]

type fun_arg_decl = (autodifftype * string * unsizedtype) list

and ('e, 's) statement =
  | Assignment of 'e * 'e
  | TargetPE of 'e
  | NRFunApp of string * 'e list
  | Check of string * 'e list
  (* XXX get rid of this and special case in gen*)
  | Break
  | Continue
  | Return of 'e option
  | Skip
  | IfElse of 'e * 's * 's option
  | While of 'e * 's
  (* XXX Collapse with For? *)
  | For of {loopvar: string; lower: 'e; upper: 'e; body: 's}
  (* A Block for now corresponds tightly with a C++ block:
     variables declared within it have local scope and are garbage collected
     when the block ends.*)
  | Block of 's list
  (* An SList does not share any of Block's semantics - it is just multiple
     (ordered!) statements*)
  | SList of 's list
  | Decl of
      { decl_adtype: autodifftype
      ; decl_id: string
      ; decl_type: 'e sizedtype }
  | FunDef of
      { fdrt: unsizedtype option
      ; fdname: string
      ; fdargs: fun_arg_decl
      ; fdbody: 's }
[@@deriving sexp, hash, map]

type paramblock = Parameters | TransformedParameters | GeneratedQuantities
[@@deriving sexp, hash]

type ('e, 's) prog =
  { functions_block: 's
  ; prepare_data: 's (* data & transformed data decls and statements *)
  ; prepare_params: 's (* param & transformed param decls and statements *)
  ; log_prob: 's (* model block, assuming above blocks run in scope*)
  ; generate_quantities:
      's
      (* also assumes prepare_data and prepare_params run*)
  ; transform_inits: 's
  ; output_vars: (string * ('e list * paramblock)) list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp]

type expr_typed_located =
  { texpr_type: Ast.unsizedtype
  ; texpr_loc: Ast.location_span sexp_opaque [@compare.ignore]
  ; texpr: expr_typed_located expr
  ; texpr_adlevel: autodifftype }
[@@deriving sexp, hash, map, of_sexp]

type stmt_loc =
  { sloc: Ast.location_span sexp_opaque [@compare.ignore]
  ; stmt: (expr_typed_located, stmt_loc) statement }
[@@deriving hash, map, of_sexp]

let rec sexp_of_expr_typed_located {texpr; _} =
  sexp_of_expr sexp_of_expr_typed_located texpr

let rec sexp_of_stmt_loc {stmt; _} =
  match stmt with
  | SList ls -> sexp_of_list sexp_of_stmt_loc ls
  | s -> sexp_of_statement sexp_of_expr_typed_located sexp_of_stmt_loc s

type typed_prog = (expr_typed_located, stmt_loc) prog [@@deriving sexp]

(* ===================== Some helper functions and values ====================== *)
let no_loc = {Ast.filename= ""; line_num= 0; col_num= 0; included_from= None}
let no_span = {Ast.begin_loc= no_loc; end_loc= no_loc}

let internal_expr =
  { texpr= Var "UHOH"
  ; texpr_loc= no_span
  ; texpr_type= UInt
  ; texpr_adlevel= DataOnly }

let zero = {internal_expr with texpr= Lit (Int, "0"); texpr_type= UInt}
