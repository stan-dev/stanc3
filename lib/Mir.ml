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

and 'e expr =
  | Var of string
  | Lit of litType * string
  | FunApp of string * 'e list
  | TernaryIf of 'e * 'e * 'e
  | And of 'e * 'e
  | Or of 'e * 'e
  | Indexed of 'e * 'e index list
[@@deriving sexp, hash, map]

let pp_builtin_syntax = Fmt.(string |> styled `Yellow)

let rec pp_expr pp_e ppf = function
  | Var varname -> Fmt.string ppf varname
  | And (lhs, rhs) -> Fmt.pf ppf "%a && %a" pp_e lhs pp_e rhs
  | Or (lhs, rhs) -> Fmt.pf ppf "%a || %a" pp_e lhs pp_e rhs
  | Lit (Str, str) -> Fmt.pf ppf "%S" str
  | Lit (_, str) -> Fmt.string ppf str
  | FunApp (name, args) ->
      Fmt.string ppf name ;
      Fmt.(list pp_e ~sep:Fmt.comma |> parens) ppf args
  | TernaryIf (pred, texpr, fexpr) ->
      Fmt.pf ppf {|@[%a@ %a@,%a@,%a@ %a@]|} pp_e pred pp_builtin_syntax "?"
        pp_e texpr pp_builtin_syntax ":" pp_e fexpr
  | Indexed (expr, indices) ->
      Fmt.pf ppf {|@[%a%a@]|} pp_e expr
        Fmt.(list (pp_index pp_e) ~sep:comma |> brackets)
        indices

and pp_index pp_e ppf = function
  | All -> Fmt.char ppf ':'
  | Single index -> pp_e ppf index
  | Upfrom index -> Fmt.pf ppf {|%a:|} pp_e index
  | Downfrom index -> Fmt.pf ppf {|:%a|} pp_e index
  | Between (lower, upper) -> Fmt.pf ppf {|%a:%a|} pp_e lower pp_e upper
  | MultiIndex index -> Fmt.pf ppf {|%a|} pp_e index

type operator = Ast.operator
type unsizedtype = Ast.unsizedtype [@@deriving sexp, hash]
type 'e sizedtype = 'e Ast.sizedtype [@@deriving sexp, hash, map]
type autodifftype = Ast.autodifftype [@@deriving sexp, hash]

let angle_brackets pp_v ppf v = Fmt.pf ppf "@[<1><%a>@]" pp_v v
let label str pp_v ppf v = Fmt.pf ppf "%s=%a" str pp_v v
let pp_keyword = Fmt.(string |> styled `Blue)

let pp_autodifftype ppf = function
  | Ast.DataOnly -> pp_keyword ppf "data "
  | Ast.AutoDiffable -> ()

let rec pp_unsizedtype ppf = function
  | Ast.UInt -> pp_keyword ppf "int"
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
  | Ast.Void -> Fmt.string ppf "void"
  | Ast.ReturnType ut -> pp_unsizedtype ppf ut

(* This directive silences some spurious warnings from ppx_deriving *)
[@@@ocaml.warning "-A"]

type fun_arg_decl = (autodifftype * string * unsizedtype) list

and ('e, 's) statement =
  | Assignment of 'e * 'e
  | TargetPE of 'e
  | NRFunApp of string * 'e list
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
  | FunDef of
      { fdrt: unsizedtype option
      ; fdname: string
      ; fdargs: fun_arg_decl
      ; fdbody: 's }
[@@deriving sexp, hash, map]

let pp_fun_arg_decl ppf (autodifftype, name, unsizedtype) =
  Fmt.pf ppf "%a%a %s" pp_autodifftype autodifftype pp_unsizedtype unsizedtype
    name

let pp_transformation pp_e ppf = function
  | Ast.Identity -> ()
  | Lower expr -> (pp_e |> label "lower" |> angle_brackets) ppf expr
  | Upper expr -> (pp_e |> label "upper" |> angle_brackets) ppf expr
  | LowerUpper (lower_expr, upper_expr) ->
      ( Fmt.(pair ~sep:comma (pp_e |> label "lower") (pp_e |> label "upper"))
      |> angle_brackets )
        ppf (lower_expr, upper_expr)
  | Offset expr -> (pp_e |> label "offet" |> angle_brackets) ppf expr
  | Multiplier expr -> (pp_e |> label "multiplier" |> angle_brackets) ppf expr
  | OffsetMultiplier (offset_expr, mult_expr) ->
      ( Fmt.(
          pair ~sep:comma (pp_e |> label "offset") (pp_e |> label "multiplier"))
      |> angle_brackets )
        ppf (offset_expr, mult_expr)
  | Ordered -> (angle_brackets Fmt.string) ppf "ordered"
  | PositiveOrdered -> (angle_brackets Fmt.string) ppf "positive_ordered"
  | Simplex -> (angle_brackets Fmt.string) ppf "simplex"
  | UnitVector -> (angle_brackets Fmt.string) ppf "unit_vector"
  | CholeskyCorr -> (angle_brackets Fmt.string) ppf "cholesky_factor_corr"
  | CholeskyCov -> (angle_brackets Fmt.string) ppf "cholesky_factor_cov"
  | Correlation -> (angle_brackets Fmt.string) ppf "corr_matrix"
  | Covariance -> (angle_brackets Fmt.string) ppf "cov_matrix"

let rec pp_sizedtype pp_e ppf (st, trans) =
  match st with
  | Ast.SInt -> Fmt.pf ppf {|%s%a|} "int" (pp_transformation pp_e) trans
  | Ast.SReal -> Fmt.pf ppf {|%s%a|} "real" (pp_transformation pp_e) trans
  | Ast.SVector expr ->
      Fmt.pf ppf {|vector%a%a|} (pp_transformation pp_e) trans
        (Fmt.brackets pp_e) expr
  | Ast.SRowVector expr ->
      Fmt.pf ppf {|row_vector%a%a|} (pp_transformation pp_e) trans
        (Fmt.brackets pp_e) expr
  | Ast.SMatrix (d1_expr, d2_expr) ->
      Fmt.pf ppf {|matrix%a%a|} (pp_transformation pp_e) trans
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets)
        (d1_expr, d2_expr)
  | Ast.SArray (st, expr) ->
      Fmt.pf ppf {|array%a%a|} (pp_transformation pp_e) trans
        Fmt.(
          pair ~sep:comma
            (fun ppf st -> pp_sizedtype pp_e ppf (st, Ast.Identity))
            pp_e
          |> brackets)
        (st, expr)

let rec pp_statement pp_e pp_s ppf = function
  | Assignment (assignee, expr) ->
      Fmt.pf ppf {|@[<h>%a :=@ %a;@]|} pp_e assignee pp_e expr
  | TargetPE expr ->
      Fmt.pf ppf {|@[<h>%a +=@ %a;@]|} pp_keyword "target" pp_e expr
  | NRFunApp (name, args) ->
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
        (decl_type, Ast.Identity) decl_id
  | FunDef {fdrt; fdname; fdargs; fdbody} -> (
    match fdrt with
    | Some rt ->
        Fmt.pf ppf {|@[<v2>%a %s%a {@ %a@]@ }|} pp_unsizedtype rt fdname
          Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
          fdargs pp_s fdbody
    | None ->
        Fmt.pf ppf {|@[<v2>%s %s%a {@ %a@]@ }|} "void" fdname
          Fmt.(list pp_fun_arg_decl ~sep:comma |> parens)
          fdargs pp_s fdbody )

type io_block =
  | Data
  | Parameters
  | TransformedParameters
  | GeneratedQuantities
[@@deriving sexp, hash]

let pp_io_block ppf = function
  | Data -> Fmt.string ppf "data"
  | Parameters -> Fmt.string ppf "parameters"
  | TransformedParameters -> Fmt.string ppf "transformed_parameters"
  | GeneratedQuantities -> Fmt.string ppf "generated_quantities"

type 'e io_var = string * ('e sizedtype * io_block) [@@deriving sexp]

let pp_io_var pp_e ppf (name, (sized_ty, io_block)) =
  Fmt.pf ppf "@[<h>%a %a %s;@]" pp_io_block io_block (pp_sizedtype pp_e)
    (sized_ty, Ast.Identity) name

let pp_block label pp_elem ppf elems =
  Fmt.pf ppf {|@[<v2>%a {@ %a@]@ }|} pp_keyword label
    Fmt.(list ~sep:cut pp_elem)
    elems ;
  Format.pp_force_newline ppf ()

let pp_io_var_block label pp_e = pp_block label (pp_io_var pp_e)

type ('e, 's) prog =
  { functions_block: 's list
  ; input_vars: 'e io_var list
  ; prepare_data: 's list (* data & transformed data decls and statements *)
  ; log_prob: 's list (*assumes data & params are in scope and ready*)
  ; generate_quantities: 's list (* assumes data & params ready & in scope*)
  ; transform_inits: 's list
  ; output_vars: 'e io_var list
  ; prog_name: string
  ; prog_path: string }
[@@deriving sexp]

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
  pp_functions_block pp_s ppf prog ;
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

type expr_typed_located =
  { texpr_type: Ast.unsizedtype
  ; texpr_loc: Ast.location_span sexp_opaque [@compare.ignore]
  ; texpr: expr_typed_located expr
  ; texpr_adlevel: autodifftype }
[@@deriving sexp, hash, map, of_sexp]

let rec pp_expr_typed_located ppf {texpr; _} =
  pp_expr pp_expr_typed_located ppf texpr

type stmt_loc =
  { sloc: Ast.location_span sexp_opaque [@compare.ignore]
  ; stmt: (expr_typed_located, stmt_loc) statement }
[@@deriving hash, map, of_sexp]

let rec pp_stmt_loc ppf {stmt; _} =
  pp_statement pp_expr_typed_located pp_stmt_loc ppf stmt

let rec sexp_of_expr_typed_located {texpr; _} =
  sexp_of_expr sexp_of_expr_typed_located texpr

let rec sexp_of_stmt_loc {stmt; _} =
  sexp_of_statement sexp_of_expr_typed_located sexp_of_stmt_loc stmt

type typed_prog = (expr_typed_located, stmt_loc) prog [@@deriving sexp]

let pp_typed_prog ppf prog = pp_prog pp_expr_typed_located pp_stmt_loc ppf prog

(* ===================== Some helper functions and values ====================== *)
let no_loc = {Ast.filename= ""; line_num= 0; col_num= 0; included_from= None}
let no_span = {Ast.begin_loc= no_loc; end_loc= no_loc}

let internal_expr =
  { texpr= Var "UHOH"
  ; texpr_loc= no_span
  ; texpr_type= UInt
  ; texpr_adlevel= DataOnly }

let zero = {internal_expr with texpr= Lit (Int, "0"); texpr_type= UInt}

(* Internal function names *)
let fn_length = "Length__"
let fn_make_array = "MakeArray__"
let fn_make_rowvec = "MakeRowVec__"
let fn_negative_infinity = "negative_infinity"
let fn_read_data = "ReadData__"
let fn_read_param = "ReadParam__"
let fn_constrain = "Constrain__"
let fn_unconstrain = "Unconstrain__"
let fn_check = "Check__"
let fn_print = "Print__"
let fn_reject = "Reject__"
