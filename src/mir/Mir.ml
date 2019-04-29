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

(** Source code locations *)
type location =
  { filename: string
  ; line_num: int
  ; col_num: int
  ; included_from: location option }
[@@deriving sexp]

(** Delimited locations *)
type location_span = {begin_loc: location; end_loc: location} [@@deriving sexp]

let merge_spans left right = {begin_loc= left.begin_loc; end_loc= right.end_loc}

(** Arithmetic and logical operators *)
type operator =
  | Plus
  | PPlus
  | Minus
  | PMinus
  | Times
  | Divide
  | Modulo
  | LDivide
  | EltTimes
  | EltDivide
  | Pow
  | Or
  | And
  | Equals
  | NEquals
  | Less
  | Leq
  | Greater
  | Geq
  | PNot
  | Transpose
[@@deriving sexp, hash, compare]

(** Unsized types for function arguments and for decorating expressions
    during type checking; we have a separate type here for Math library
    functions as these functions can be overloaded, so do not have a unique
    type in the usual sense. Still, we want to assign a unique type to every
    expression during type checking.  *)
type unsizedtype =
  | UInt
  | UReal
  | UVector
  | URowVector
  | UMatrix
  | UArray of unsizedtype
  | UFun of (autodifftype * unsizedtype) list * returntype
  | UMathLibraryFunction
[@@deriving sexp, hash]

(** Flags for data only arguments to functions *)
and autodifftype = DataOnly | AutoDiffable [@@deriving sexp, hash, compare]

and returntype = Void | ReturnType of unsizedtype [@@deriving sexp, hash]

(** Sized types, for variable declarations *)
type 'e sizedtype =
  | SInt
  | SReal
  | SVector of 'e
  | SRowVector of 'e
  | SMatrix of 'e * 'e
  | SArray of 'e sizedtype * 'e
[@@deriving sexp, compare, map, hash]

(** remove_size [st] discards size information from a sizedtype
    to return an unsizedtype. *)
let rec remove_size = function
  | SInt -> UInt
  | SReal -> UReal
  | SVector _ -> UVector
  | SRowVector _ -> URowVector
  | SMatrix _ -> UMatrix
  | SArray (t, _) -> UArray (remove_size t)

type litType = Int | Real | Str [@@deriving sexp, hash]

(**  *)
type fun_kind = StanLib | CompilerInternal | UserDefined
[@@deriving compare, sexp, hash]

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
[@@deriving sexp, hash, map]

and 'e expr =
  | Var of string
  | Lit of litType * string
  | FunApp of fun_kind * string * 'e list
  | TernaryIf of 'e * 'e * 'e
  | EAnd of 'e * 'e
  | EOr of 'e * 'e
  | Indexed of 'e * 'e index list
[@@deriving sexp, hash, map]

(* This directive silences some spurious warnings from ppx_deriving *)
[@@@ocaml.warning "-A"]

type fun_arg_decl = (autodifftype * string * unsizedtype) list
[@@deriving sexp, hash, map]

type 's fun_def =
  { fdrt: unsizedtype option
  ; fdname: string
  ; fdargs: fun_arg_decl
  ; fdbody: 's
  ; fdloc: location_span [@compare.ignore] }
[@@deriving sexp, hash, map]

and 'e lvalue = string * 'e index list

and ('e, 's) statement =
  | Assignment of 'e lvalue * 'e
  | TargetPE of 'e
  | NRFunApp of fun_kind * string * 'e list
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
  (* SList has no semantics, just programming convenience *)
  | SList of 's list
  | Decl of
      { decl_adtype: autodifftype
      ; decl_id: string
      ; decl_type: 'e sizedtype }
[@@deriving sexp, hash, map]

type io_block =
  | Data
  | Parameters
  | TransformedParameters
  | GeneratedQuantities
[@@deriving sexp, hash]

type 'e io_var = string * ('e sizedtype * io_block) [@@deriving sexp]

type ('e, 's) prog =
  { functions_block: 's fun_def list
  ; input_vars: 'e io_var list
  ; prepare_data: 's list (* data & transformed data decls and statements *)
  ; log_prob: 's list (*assumes data & params are in scope and ready*)
  ; generate_quantities: 's list (* assumes data & params ready & in scope*)
  ; transform_inits: 's list
  ; output_vars: 'e io_var list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp]

type 'm with_expr = {expr: 'm with_expr expr; emeta: 'm}

and mtype_loc_ad =
  { mtype: unsizedtype
  ; mloc: location_span sexp_opaque [@compare.ignore]
  ; madlevel: autodifftype }
[@@deriving sexp]

type ('e, 'm) stmt_with =
  {stmt: ('e with_expr, ('e, 'm) stmt_with) statement; smeta: 'm}

and stmt_loc =
  (mtype_loc_ad, (location_span sexp_opaque[@compare.ignore])) stmt_with
[@@deriving sexp]

type expr_no_meta = unit with_expr
type stmt_no_meta = (expr_no_meta, unit) stmt_with
type typed_prog = (mtype_loc_ad with_expr, stmt_loc) prog [@@deriving sexp]

(* == Pretty printers ======================================================= *)

let pp_builtin_syntax = Fmt.(string |> styled `Yellow)
let pp_keyword = Fmt.(string |> styled `Blue)
let angle_brackets pp_v ppf v = Fmt.pf ppf "@[<1><%a>@]" pp_v v
let label str pp_v ppf v = Fmt.pf ppf "%s=%a" str pp_v v

let pp_autodifftype ppf = function
  | DataOnly -> pp_keyword ppf "data "
  | AutoDiffable -> ()

let rec pp_unsizedtype ppf = function
  | UInt -> pp_keyword ppf "int"
  | UReal -> pp_keyword ppf "real"
  | UVector -> pp_keyword ppf "vector"
  | URowVector -> pp_keyword ppf "row_vector"
  | UMatrix -> pp_keyword ppf "matrix"
  | UArray ut -> (Fmt.brackets pp_unsizedtype) ppf ut
  | UFun (argtypes, rt) ->
      Fmt.pf ppf {|%a => %a|}
        Fmt.(list (pair ~sep:comma pp_autodifftype pp_unsizedtype) ~sep:comma)
        argtypes pp_returntype rt
  | UMathLibraryFunction ->
      (angle_brackets Fmt.string) ppf "Stan Math function"

and pp_returntype ppf = function
  | Void -> Fmt.string ppf "void"
  | ReturnType ut -> pp_unsizedtype ppf ut

let rec pp_sizedtype pp_e ppf st =
  match st with
  | SInt -> Fmt.string ppf "int"
  | SReal -> Fmt.string ppf "real"
  | SVector expr -> Fmt.pf ppf {|vector%a|} (Fmt.brackets pp_e) expr
  | SRowVector expr -> Fmt.pf ppf {|row_vector%a|} (Fmt.brackets pp_e) expr
  | SMatrix (d1_expr, d2_expr) ->
      Fmt.pf ppf {|matrix%a|}
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | SArray (st, expr) ->
      Fmt.pf ppf {|array%a|}
        Fmt.(
          pair ~sep:comma (fun ppf st -> pp_sizedtype pp_e ppf st) pp_e
          |> brackets)
        (st, expr)

let rec pp_expr pp_e ppf = function
  | Var varname -> Fmt.string ppf varname
  | Lit (Str, str) -> Fmt.pf ppf "%S" str
  | Lit (_, str) -> Fmt.string ppf str
  | FunApp (_, name, args) ->
      Fmt.string ppf name ;
      Fmt.(list pp_e ~sep:Fmt.comma |> parens) ppf args
  | TernaryIf (pred, texpr, fexpr) ->
      Fmt.pf ppf {|@[%a@ %a@,%a@,%a@ %a@]|} pp_e pred pp_builtin_syntax "?"
        pp_e texpr pp_builtin_syntax ":" pp_e fexpr
  | Indexed (expr, indices) ->
      pp_indexed pp_e ppf (Fmt.strf "%a" pp_e expr, indices)

and pp_indexed pp_e ppf (ident, indices) =
  Fmt.pf ppf {|@[%s%a@]|} ident
    Fmt.(list (pp_index pp_e) ~sep:comma |> brackets)
    indices

and pp_index pp_e ppf = function
  | All -> Fmt.char ppf ':'
  | Single index -> pp_e ppf index
  | Upfrom index -> Fmt.pf ppf {|%a:|} pp_e index
  | Downfrom index -> Fmt.pf ppf {|:%a|} pp_e index
  | Between (lower, upper) -> Fmt.pf ppf {|%a:%a|} pp_e lower pp_e upper
  | MultiIndex index -> Fmt.pf ppf {|%a|} pp_e index

let pp_fun_arg_decl ppf (autodifftype, name, unsizedtype) =
  Fmt.pf ppf "%a%a %s" pp_autodifftype autodifftype pp_unsizedtype unsizedtype
    name

let pp_fun_def pp_s ppf = function
  | {fdrt; fdname; fdargs; fdbody} -> (
    match fdrt with
    | Some rt ->
        Fmt.pf ppf {|@[<v2>%a %s%a {@ %a@]@ }|} pp_unsizedtype rt fdname
          Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
          fdargs pp_s fdbody
    | None ->
        Fmt.pf ppf {|@[<v2>%s %s%a {@ %a@]@ }|} "void" fdname
          Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
          fdargs pp_s fdbody )

let rec pp_statement pp_e pp_s ppf = function
  | Assignment ((assignee, idcs), rhs) ->
      Fmt.pf ppf {|@[<h>%a :=@ %a;@]|} (pp_indexed pp_e) (assignee, idcs) pp_e
        rhs
  | TargetPE expr ->
      Fmt.pf ppf {|@[<h>%a +=@ %a;@]|} pp_keyword "target" pp_e expr
  | NRFunApp (fn_kind, name, args) ->
      Fmt.pf ppf {|@[%s%a;@]|} name Fmt.(list pp_e ~sep:comma |> parens) args
  | Break -> pp_keyword ppf "break;"
  | Continue -> pp_keyword ppf "continue;"
  | Skip -> pp_keyword ppf "skip;"
  | Return (Some expr) -> Fmt.pf ppf {|%a %a;|} pp_keyword "return" pp_e expr
  | Return _ -> pp_keyword ppf "return;"
  | IfElse (pred, s_true, Some s_false) ->
      Fmt.pf ppf {|@[<v2>@[%a(%a)@] {@;%a@]@;@[<v2>@[} %a@] {@;%a@]@;}|}
        pp_builtin_syntax "if" pp_e pred pp_s s_true pp_builtin_syntax "else"
        pp_s s_false
  | IfElse (pred, s_true, _) ->
      Fmt.pf ppf {|@[<v2>@[%a(%a)@] {@;%a@]@;}|} pp_builtin_syntax "if" pp_e
        pred pp_s s_true
  | While (pred, stmt) ->
      Fmt.pf ppf {|@[<v2>@[%a(%a)@] {@;%a@]@;}|} pp_builtin_syntax "while" pp_e
        pred pp_s stmt
  | For {loopvar; lower; upper; body} ->
      Fmt.pf ppf {|@[<v2>@[%a(%s in %a:%a)@] {@;%a@]@;}|} pp_builtin_syntax
        "for" loopvar pp_e lower pp_e upper pp_s body
  | Block stmts -> Fmt.pf ppf {|@[<v>%a@]|} Fmt.(list pp_s ~sep:Fmt.cut) stmts
  | SList stmts -> Fmt.(list pp_s ~sep:Fmt.cut |> vbox) ppf stmts
  | Decl {decl_adtype; decl_id; decl_type} ->
      Fmt.pf ppf {|%a%a %s;|} pp_autodifftype decl_adtype (pp_sizedtype pp_e)
        decl_type decl_id

let pp_io_block ppf = function
  | Data -> Fmt.string ppf "data"
  | Parameters -> Fmt.string ppf "parameters"
  | TransformedParameters -> Fmt.string ppf "transformed_parameters"
  | GeneratedQuantities -> Fmt.string ppf "generated_quantities"

let pp_io_var pp_e ppf (name, (sized_ty, io_block)) =
  Fmt.pf ppf "@[<h>%a %a %s;@]" pp_io_block io_block (pp_sizedtype pp_e)
    sized_ty name

let pp_block label pp_elem ppf elems =
  Fmt.pf ppf {|@[<v2>%a {@ %a@]@ }|} pp_keyword label
    Fmt.(list ~sep:cut pp_elem)
    elems ;
  Format.pp_force_newline ppf ()

let pp_io_var_block label pp_e = pp_block label (pp_io_var pp_e)

let pp_input_vars pp_e ppf {input_vars; _} =
  pp_io_var_block "input_vars" pp_e ppf input_vars

let pp_output_vars pp_e ppf {output_vars; _} =
  pp_io_var_block "output_vars" pp_e ppf output_vars

let pp_functions_block pp_s ppf {functions_block; _} =
  pp_block "functions" pp_s ppf functions_block

let pp_prepare_data pp_s ppf {prepare_data; _} =
  pp_block "prepare_data" pp_s ppf prepare_data

let pp_log_prob pp_s ppf {log_prob; _} = pp_block "log_prob" pp_s ppf log_prob

let pp_generate_quantities pp_s ppf {generate_quantities; _} =
  pp_block "generate_quantities" pp_s ppf generate_quantities

let pp_transform_inits pp_s ppf {transform_inits; _} =
  pp_block "transform_inits" pp_s ppf transform_inits

let pp_prog pp_e pp_s ppf prog =
  Format.open_vbox 0 ;
  pp_functions_block (pp_fun_def pp_s) ppf prog ;
  Fmt.cut ppf () ;
  pp_input_vars pp_e ppf prog ;
  Fmt.cut ppf () ;
  pp_prepare_data pp_s ppf prog ;
  Fmt.cut ppf () ;
  pp_log_prob pp_s ppf prog ;
  Fmt.cut ppf () ;
  pp_generate_quantities pp_s ppf prog ;
  Fmt.cut ppf () ;
  pp_transform_inits pp_s ppf prog ;
  Fmt.cut ppf () ;
  pp_output_vars pp_e ppf prog ;
  Format.close_box ()

let rec pp_expr_typed_located ppf {expr; _} =
  pp_expr pp_expr_typed_located ppf expr

let rec pp_stmt_loc ppf {stmt; _} =
  pp_statement pp_expr_typed_located pp_stmt_loc ppf stmt

let rec sexp_of_expr_typed_located {expr; _} =
  sexp_of_expr sexp_of_expr_typed_located expr

let rec sexp_of_stmt_loc {stmt; _} =
  sexp_of_statement sexp_of_expr_typed_located sexp_of_stmt_loc stmt

let pp_typed_prog ppf prog = pp_prog pp_expr_typed_located pp_stmt_loc ppf prog

(* ===================== Some helper functions and values ====================== *)
let no_loc = {filename= ""; line_num= 0; col_num= 0; included_from= None}
let no_span = {begin_loc= no_loc; end_loc= no_loc}

type internal_fn =
  | FnLength
  | FnMakeArray
  | FnMakeRowVec
  | FnNegInf
  | FnReadData
  | FnReadParam
  | FnConstrain
  | FnUnconstrain
  | FnCheck
  | FnPrint
  | FnReject
[@@deriving sexp]

let mk_string_of sexp_of x = Sexp.to_string (sexp_of x) ^ "__"
let string_of_internal_fn = mk_string_of sexp_of_internal_fn

let mk_of_string of_sexp x =
  try
    String.chop_suffix_exn ~suffix:"__" x |> Sexp.of_string |> of_sexp |> Some
  with
  | Sexplib.Conv.Of_sexp_error _ -> None
  | Invalid_argument _ -> None

let internal_fn_of_string = mk_of_string internal_fn_of_sexp
let internal_meta = {mloc= no_span; mtype= UInt; madlevel= DataOnly}
let loop_bottom = {expr= Lit (Int, "0"); emeta= internal_meta}
let string_of_operator = mk_string_of sexp_of_operator
let operator_of_string = mk_of_string operator_of_sexp

(* -- Locations and spans --------------------------------------------------- *)

(** Render a location as a string *)
let rec string_of_location loc =
  let open Format in
  let included_from_str =
    match loc.included_from with
    | None -> ""
    | Some loc2 -> sprintf ", included from\n%s" (string_of_location loc2)
  in
  sprintf "file %s, line %d, column %d%s" loc.filename loc.line_num loc.col_num
    included_from_str

(** Render a location_span as a string *)
let string_of_location_span loc_sp =
  match loc_sp with
  | {begin_loc; end_loc} ->
      let bf = begin_loc.filename in
      let ef = end_loc.filename in
      let bl = begin_loc.line_num in
      let el = end_loc.line_num in
      let bc = begin_loc.col_num in
      let ec = end_loc.col_num in
      let open Format in
      let file_line_col_string =
        if bf = ef then
          sprintf "file %s, %s" bf
            ( if bl = el then
              sprintf "line %d, %s" bl
                ( if bc = ec then sprintf "column %d" bc
                else sprintf "columns %d-%d" bc ec )
            else sprintf "line %d, column %d to line %d, column %d" bl bc el ec
            )
        else
          sprintf "file %s, line %d, column %d to file %s, line %d, column %d"
            bf bl bc ef el ec
      in
      let included_from_str =
        match begin_loc.included_from with
        | None -> ""
        | Some loc -> sprintf ", included from\n%s" (string_of_location loc)
      in
      sprintf "%s%s" file_line_col_string included_from_str

(* The following module signatures define the parts of the compiler we 
   can abstract over.  

   The `Frontend` module defines two types of errors (syntactic and semantic)
   and exposes functions to parse a file or string to MIR typed programs. 

   These functions return a result with the typed program as success or 
   a list of frontend errors. The module also exposes a way of rendering 
   these errors for use from the compiler

   The `Optimize` module exposes a type 'level' which represents the optimization
   options, a function for parsing the level from a string and a function
   which performs the actual optimization.

   The `Backend` module exposes the backend representation type and two functions
   that take MIR typed programs to that represenation or to a string.

   The `Compiler.Make` functor allows you to construct a `Compiler.S` from
   modules fulfilling these signatures.
*)

module type Frontend = sig
  (* options for specifying the behaviour of the frontend *)
  type frontend_opts

  val frontend_opts_of_string : (frontend_opts, string) result
  val default_frontend_opts : frontend_opts

  (* the type of semantic errors *)
  type semantic_error

  (* the type of syntax errors *)
  type syntax_error
  type frontend_error = (syntax_error, semantic_error) Either.t

  val render_error : frontend_error -> string

  val mir_of_file :
       opts:frontend_opts
    -> file:string
    -> (typed_prog, frontend_error list) result

  val mir_of_string :
       opts:frontend_opts
    -> str:string
    -> (typed_prog, frontend_error list) result
end

module type Optimize = sig
  (* variant of possible optimization levels *)
  type optimization_opts

  (* parse level from string, for use in e.g. command line argument parser *)
  val optimization_opts_of_string :
    string -> (optimization_opts, string) result

  val default_optimization_opts : optimization_opts
  val optimize : opts:optimization_opts -> typed_prog -> typed_prog
end

module type Backend = sig
  type backend_opts

  val backend_opts_of_string : string -> (backend_opts, string) result
  val default_backend_opts : backend_opts

  (* the type of backend representation *)
  type repr

  val mir_to_repr : opts:backend_opts -> typed_prog -> repr
  val mir_to_string : opts:backend_opts -> typed_prog -> string
end

module Compiler = struct
  module type S = sig
    type semantic_error
    type syntax_error
    type frontend_error
    type compiler_opts_error
    type compiler_opts

    val default_compiler_opts : compiler_opts

    val compiler_opts_of_string :
      string -> (compiler_opts, compiler_opts_error list) result

    val compile_from_file :
      opts:compiler_opts -> file:string -> (string, frontend_error list) result
  end

  module Make (F : Frontend) (O : Optimize) (B : Backend) :
    S
    with type semantic_error := F.semantic_error
     and type syntax_error := F.syntax_error
     and type frontend_error := F.frontend_error = struct
    type compiler_opts =
      { frontend_opts: F.frontend_opts
      ; optimization_opts: O.optimization_opts
      ; backend_opts: B.backend_opts }

    let default_compiler_opts =
      { frontend_opts= F.default_frontend_opts
      ; optimization_opts= O.default_optimization_opts
      ; backend_opts= B.default_backend_opts }

    type compiler_opts_error =
      | Frontend_opts_error of string
      | Optimize_opts_error of string
      | Backend_opts_error of string

    let compiler_opts_of_string str = Error [Frontend_opts_error "todo"]

    let compile_from_file ~opts ~file =
      F.mir_of_file ~opts:opts.frontend_opts ~file
      |> Result.map ~f:(O.optimize ~opts:opts.optimization_opts)
      |> Result.map ~f:(B.mir_to_string ~opts:opts.backend_opts)
  end
end
