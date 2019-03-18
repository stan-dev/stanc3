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

type 'e index =
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
  | Indexed of 'e * 'e index list
[@@deriving sexp, hash, map, compare]

type unsizedtype = Ast.unsizedtype [@@deriving sexp, hash]
type autodifftype = Ast.autodifftype [@@deriving sexp, hash]

let no_loc = {Ast.filename= ""; line_num= 0; col_num= 0; included_from= None}
let no_span = {Ast.begin_loc= no_loc; end_loc= no_loc}

(* This directive silences some spurious warnings from ppx_deriving *)
[@@@ocaml.warning "-A"]

type fun_arg_decl = (autodifftype * string * unsizedtype) list

and ('e, 's) statement =
  | Assignment of 'e * 'e
  | TargetPE of 'e
  | NRFunApp of string * 'e list
  | Check of string * 'e list
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
  | Decl of {decl_adtype: autodifftype; decl_id: string; decl_type: unsizedtype}
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
type 'e top_var_decl =
  { tvident: string
  ; tvtype: 'e Ast.sizedtype
  ; tvtrans: 'e Ast.transformation
  ; tvloc: Ast.location_span sexp_opaque [@compare.ignore] }
[@@deriving sexp, map]

type 'e top_var_table = (string, 'e top_var_decl) Map.Poly.t [@@deriving sexp]

(* For some reason, ppx_deriving cannot seem to generate this map automatically, so
   we write it by hand *)
let map_top_var_table g = Map.Poly.map ~f:(map_top_var_decl g)

type ('e, 's) prog =
  { functions_block: 's list
  ; data_vars: 'e top_var_table
  ; tdata_vars: 'e top_var_table
  ; prepare_data: 's list
  ; params: 'e top_var_table
  ; tparams: 'e top_var_table
  ; prepare_params:
      's list
      (* XXX too intimately tied up with stan reader.hpp and writer.hpp in codegen
     TODO: codegen parameter constraining and unconstraining in prepare_params
  *)
  ; log_prob: 's list
  ; gen_quant_vars: 'e top_var_table
  ; generate_quantities: 's list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp, map]

type expr_typed_located =
  { texpr_type: Ast.unsizedtype
  ; texpr_loc: Ast.location_span sexp_opaque [@compare.ignore]
  ; texpr: expr_typed_located expr
  ; texpr_adlevel: autodifftype }
[@@deriving sexp, hash]

type stmt_loc =
  { sloc: Ast.location_span sexp_opaque [@compare.ignore]
  ; stmt: (expr_typed_located, stmt_loc) statement }
[@@deriving sexp, hash]

type stmt_loc_num =
  { slocn: Ast.location_span sexp_opaque [@compare.ignore]
  ; stmtn: (expr_typed_located, int) statement }
[@@deriving sexp, hash]

type typed_prog = (expr_typed_located, stmt_loc) prog [@@deriving sexp]

(* ===================== Some helper functions and values ====================== *)

(** Dives into any number of nested blocks and lists, but will not recurse other
    places statements occur in the MIR (e.g. loop bodies) *)
let rec map_toplevel_stmts f {sloc; stmt} =
  match stmt with
  | Block ls -> {stmt= Block (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | SList ls -> {stmt= SList (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | _ -> f {sloc; stmt}

let tvdecl_to_decl {tvident; tvtype; tvloc; _} = (tvident, tvtype, tvloc)

let rec map_rec_stmt_loc
    (f :
         (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement) ({sloc; stmt} : stmt_loc) =
  let recurse = map_rec_stmt_loc f in
  {sloc; stmt= f (map_statement (fun x -> x) recurse stmt)}

let map_rec_state_stmt_loc
    (f :
         's
      -> (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement * 's) (state : 's)
    ({sloc; stmt} : stmt_loc) : stmt_loc * 's =
  let cur_state = ref state in
  let g stmt =
    let stmt, state = f !cur_state stmt in
    let _ = cur_state := state in
    stmt
  in
  let stmt = map_rec_stmt_loc g {sloc; stmt} in
  let state = !cur_state in
  (stmt, state)

let map_rec_stmt_loc_num (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t)
    (f :
         int
      -> (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement) (s : stmt_loc_num) =
  let rec map_rec_stmt_loc_num' (cur_node : int)
      ({slocn; stmtn} : stmt_loc_num) : stmt_loc =
    let find_node i = Map.find_exn flowgraph_to_mir i in
    let recurse i = map_rec_stmt_loc_num' i (find_node i) in
    {sloc= slocn; stmt= f cur_node (map_statement (fun x -> x) recurse stmtn)}
  in
  map_rec_stmt_loc_num' 1 s

let map_rec_state_stmt_loc_num
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t)
    (f :
         int
      -> 's
      -> (expr_typed_located, stmt_loc) statement
      -> (expr_typed_located, stmt_loc) statement * 's) (state : 's)
    (s : stmt_loc_num) : stmt_loc * 's =
  let cur_state = ref state in
  let g i stmt =
    let stmt, state = f i !cur_state stmt in
    let _ = cur_state := state in
    stmt
  in
  let stmt = map_rec_stmt_loc_num flowgraph_to_mir g s in
  let state = !cur_state in
  (stmt, state)

let stmt_loc_of_stmt_loc_num
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) (s : stmt_loc_num) =
  map_rec_stmt_loc_num flowgraph_to_mir (fun _ s' -> s') s

let statement_stmt_loc_of_statement_stmt_loc_num
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) s =
  (stmt_loc_of_stmt_loc_num flowgraph_to_mir {stmtn= s; slocn= no_span}).stmt

(** Forgetful function from numbered to unnumbered programs *)
let unnumbered_prog_of_numbered_prog
    (flowgraph_to_mir : (int, stmt_loc_num) Map.Poly.t) p =
  map_prog (stmt_loc_of_stmt_loc_num flowgraph_to_mir) p

let internal_expr =
  { texpr= Var "UHOH"
  ; texpr_loc= no_span
  ; texpr_type= UInt
  ; texpr_adlevel= DataOnly }

let zero = {internal_expr with texpr= Lit (Int, "0"); texpr_type= UInt}
