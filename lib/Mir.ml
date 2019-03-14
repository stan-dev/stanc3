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

type litType = Int | Real | Str [@@deriving sexp, hash, compare]

type operator =
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
[@@deriving sexp, hash, compare]

type transformation = expr Ast.transformation

and index =
  | All
  | Single of expr
  (*
  | MatrixSingle of expr
 *)
  | Upfrom of expr
  | Downfrom of expr
  | Between of expr * expr
  | MultiIndex of expr

(** XXX
*)
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

type fun_arg_decl = (adtype * string * unsizedtype) list

and 's statement =
  | Assignment of expr * expr
  | TargetPE of expr
  | NRFunApp of string * expr list
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
  { functions_block: 's list
  ; data_vars: top_var_table
  ; tdata_vars: top_var_table
  ; prepare_data: 's list
  ; params: top_var_table
  ; tparams: top_var_table
  ; prepare_params:
      's list
      (* XXX too intimately tied up with stan reader.hpp and writer.hpp in codegen
     TODO: codegen parameter constraining and unconstraining in prepare_params
  *)
  ; log_prob: 's list
  ; gen_quant_vars: top_var_table
  ; generate_quantities: 's list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, map]

type stmt_loc =
  {sloc: string sexp_opaque [@compare.ignore]; stmt: stmt_loc statement}
[@@deriving sexp, hash]

type stmt_loc_num =
  {slocn: string sexp_opaque [@compare.ignore]; stmtn: int statement}
[@@deriving sexp, hash]

(* ===================== Some helper functions ====================== *)

(** Dives into any number of nested blocks and lists, but will not recurse other
    places statements occur in the MIR (e.g. loop bodies) *)
let rec map_toplevel_stmts f {sloc; stmt} =
  match stmt with
  | Block ls -> {stmt= Block (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | SList ls -> {stmt= SList (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | _ -> f {sloc; stmt}

let tvdecl_to_decl {tvident; tvtype; tvloc; _} = (tvident, tvtype, tvloc)

let rec map_stmt_loc (f : 'a statement -> 'a statement)
    ({sloc; stmt} : stmt_loc) =
  let recurse = map_stmt_loc f in
  {sloc; stmt= map_statement recurse (f stmt)}

let map_stmt_loc_num (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t)
    (f : int -> 'a statement -> 'a statement) (s : stmt_loc_num) =
  let rec map_stmt_num' (cur_node : int) ({slocn; stmtn} : stmt_loc_num) :
      stmt_loc =
    let find_node i = Map.find_exn flowgraph_to_mir i in
    let recurse i = map_stmt_num' i (find_node i) in
    {sloc= slocn; stmt= map_statement recurse (f cur_node stmtn)}
  in
  map_stmt_num' 1 s

let stmt_loc_of_stmt_loc_num
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) (s : stmt_loc_num) =
  map_stmt_loc_num flowgraph_to_mir (fun _ s' -> s') s

let statement_stmt_loc_of_statement_stmt_loc_num
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) s =
  (stmt_loc_of_stmt_loc_num flowgraph_to_mir {stmtn= s; slocn= ""}).stmt

(** Forgetful function from numbered to unnumbered programs *)
let unnumbered_prog_of_numbered_prog
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) p =
  map_prog (stmt_loc_of_stmt_loc_num flowgraph_to_mir) p
