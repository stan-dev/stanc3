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
  | Indexed of 'e * 'e index list
[@@deriving sexp, hash]

let rec pp_expr pp_e ppf = function 
  | Var varname ->
      Fmt.string ppf varname 

  | Lit(Str,str) ->
      Fmt.pf ppf "%S" str

  | Lit(_,str) ->
      Fmt.string ppf str

  | FunApp(name,args) ->
      Fmt.string ppf name;
      Fmt.(
        list pp_e ~sep:Fmt.comma
        |> parens
      ) ppf args

  | TernaryIf(pred,texpr,fexpr) -> 
    Fmt.pf ppf 
      {|@[%a@ ?@,%a@,:@ %a@]|}
      pp_e pred
      pp_e texpr
      pp_e fexpr

  | Indexed(expr,indices) ->
    Fmt.pf ppf 
      {|@[%a[%a]@]|}
      pp_e expr 
      Fmt.(list (pp_index pp_e) ~sep:comma) indices

and pp_index pp_e ppf = function 
  | All -> 
      Fmt.char ppf ':'

  | Single index -> 
      pp_e ppf index

  | Upfrom index -> 
      Fmt.pf ppf {|%a:|}
        pp_e index

  | Downfrom index ->
      Fmt.pf ppf {|:%a|}
        pp_e index

  | Between(lower,upper) ->
      Fmt.pf ppf {|%a:%a|}
        pp_e lower 
        pp_e upper

  | MultiIndex index -> 
      (* TODO: I'm not sure what a multi-index is so this formatting probably 
          makes no sense... *)
      Fmt.pf ppf {|~%a|}
        pp_e index


type unsizedtype = Ast.unsizedtype [@@deriving sexp, hash]

type autodifftype = Ast.autodifftype [@@deriving sexp, hash]

type returntype = Ast.returntype [@@deriving sexp, hash]

let pp_autodifftype ppf = function 
  | Ast.DataOnly -> 
      Fmt.string ppf "dataonly"

  | Ast.AutoDiffable -> 
      Fmt.string ppf "autodiffable"

let rec pp_unsizedtype ppf = function 
  | Ast.UInt -> 
      Fmt.string ppf "int"
  | UReal -> 
      Fmt.string ppf "real"

  | UVector -> 
      Fmt.string ppf "vector"

  | URowVector -> 
      Fmt.string ppf "row_vector"

  | UMatrix -> 
      Fmt.string ppf "matrix"

  | UArray ut ->
      (Fmt.brackets pp_unsizedtype) ppf ut
      
  | UFun(argtypes, rt) ->
      Fmt.pf ppf
        {|%a@,=>@ %a|}
        Fmt.(list (pair ~sep:comma pp_autodifftype pp_unsizedtype) ~sep:comma) argtypes
        pp_returntype rt


  | UMathLibraryFunction -> 
      Fmt.string ppf "<Stan Math function>"

and pp_returntype ppf = function 
  | Ast.Void ->
      Fmt.string ppf "void"

  | Ast.ReturnType ut -> 
      pp_unsizedtype ppf ut

let no_loc = 
  { Ast.filename = ""
  ; line_num = 0
  ; col_num = 0
  ; included_from = None
  }

let no_span = 
  { Ast.begin_loc = no_loc
  ; end_loc = no_loc
  }

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
[@@deriving sexp, hash]

let pp_fun_arg_decl ppf (autodifftype,name,unsizedtype) = 
  Fmt.pf ppf 
    "%a@ %s@ %a"
    pp_autodifftype autodifftype
    name
    pp_unsizedtype unsizedtype

let pp_statement pp_e pp_s ppf = function 
  | Assignment (assignee,expr) -> 
      Fmt.pf ppf
        {|%a@ :=@,%a|}
        pp_e assignee
        pp_e expr

  | TargetPE expr -> 
      Fmt.pf ppf 
        {|target@ +=@,%a|}
        pp_e expr 

  | NRFunApp(name,args) -> 
      Fmt.pf ppf 
        {|%s[@(<hov>%a@])|}
        name 
        Fmt.(list pp_e ~sep:comma) args

  | Check(ident, exprs) -> 
      (* TODO: I'm not sure what a `Check` statement is so this may not make
        sense *)
      Fmt.pf ppf 
        {|check(%s)@[<hov>{%a@]}|}
        ident 
        Fmt.(list pp_e ~sep:comma) exprs

  | Break -> 
      Fmt.string ppf "break"

  | Continue -> 
      Fmt.string ppf "continue"

  | Skip -> 
      Fmt.string ppf "continue"

  | Return (Some expr) -> 
      Fmt.pf ppf 
        {|return@ %a|}
        pp_e expr

  | Return _ -> 
      Fmt.string ppf "return"

  | IfElse(pred,s_true,Some s_false) -> 
      Fmt.pf ppf 
        {|if(%a){@[<2>%a@]}@ else @ {@[<2>%a@]}|}
        pp_e pred
        pp_s s_true
        pp_s s_false

  | IfElse(pred,s_true,_) -> 
      Fmt.pf ppf 
        {|if(%a){@[<2>%a@]}|}
        pp_e pred
        pp_s s_true

  | While(pred,stmt) -> 
      Fmt.pf ppf 
        {|while(%a){@[<2>%a@]}|}
        pp_e pred
        pp_s stmt
  
  | For {loopvar; lower; upper; body} -> 
      Fmt.pf ppf 
        {|for(%s in %a:%a)@ {@[<2>%a@]}|}
        loopvar
        pp_e lower 
        pp_e upper 
        pp_s body
  | Block stmts -> 
      Fmt.pf ppf 
        {|{@[<2>%a@]}|}
        Fmt.(list pp_s ~sep:Fmt.cut) stmts

  | SList stmts -> 
      Fmt.(list pp_s ~sep:Fmt.cut) ppf stmts

  | Decl {decl_adtype; decl_id; decl_type} -> 
      Fmt.pf ppf 
        {|%a@ %a@ %s|}
        pp_autodifftype decl_adtype
        pp_unsizedtype decl_type
        decl_id

  | FunDef {fdrt; fdname; fdargs; fdbody} -> 
      match fdrt with 
      | Some rt -> 
          Fmt.pf ppf 
            {|%a@ %s%a@ {@[<2>%a@]}|}
            pp_unsizedtype rt 
            fdname
            Fmt.(list pp_fun_arg_decl ~sep:comma |> parens) fdargs
            pp_s fdbody

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
  { tvident : string
  ; tvtype : 'e Ast.sizedtype
  ; tvtrans : 'e Ast.transformation
  ; tvloc : Ast.location_span sexp_opaque [@compare.ignore] }
[@@deriving sexp]

let angle_brackets pp_v ppf v = 
  Fmt.pf ppf "@[<1><%a>@]" pp_v v

let label str pp_v ppf v = 
  Fmt.pf ppf "%s=%a" str pp_v v

let pp_transformation pp_e ppf = function 
  | Ast.Identity -> 
      ()
  
  | Lower expr -> 
      (pp_e |> label "lower" |> angle_brackets) ppf expr

  | Upper expr -> 
      (pp_e |> label "upper" |> angle_brackets) ppf expr

  | LowerUpper(lower_expr,upper_expr) -> 
      (Fmt.(pair ~sep:comma 
              (pp_e |> label "lower")
              (pp_e |> label "upper")
          )
          |> angle_brackets) ppf (lower_expr,upper_expr)

  | Offset expr ->
      (pp_e |> label "offet" |> angle_brackets) ppf expr

  | Multiplier expr ->
      (pp_e |> label "multiplier" |> angle_brackets) ppf expr

  | OffsetMultiplier(offset_expr,mult_expr) ->
      (Fmt.(pair ~sep:comma 
              (pp_e |> label "offset")
              (pp_e |> label "multiplier")
          )
          |> angle_brackets) ppf (offset_expr,mult_expr)

  | Ordered -> 
      Fmt.string ppf "<ordered>"

  | PositiveOrdered ->
      Fmt.string ppf "<positive_ordered>"

  | Simplex ->
      Fmt.string ppf "<simplex>"

  | UnitVector ->
      Fmt.string ppf "<unit_vector>"

  | CholeskyCorr ->
      Fmt.string ppf "<cholesky_factor_corr>"

  | CholeskyCov -> 
      Fmt.string ppf "<cholesky_factor_cov>"

  | Correlation -> 
      Fmt.string ppf "<corr_matrix>"

  | Covariance ->
      Fmt.string ppf "<cov_matrix>"

let rec pp_sizedtype pp_e ppf (st,trans)= 
  match st with
  | Ast.SInt ->
      Fmt.pf ppf 
        {|%s%a|}
        "int"
        (pp_transformation pp_e) trans

  | Ast.SReal ->
      Fmt.pf ppf 
        {|%s%a|}
        "real"
        (pp_transformation pp_e) trans

  | Ast.SVector expr -> 
      Fmt.pf ppf 
        {|vector%a%a|}
        (pp_transformation pp_e) trans
        (Fmt.brackets pp_e) expr
        

  | Ast.SRowVector expr -> 
      Fmt.pf ppf 
        {|row_vector%a%a|}
        (pp_transformation pp_e) trans
        (Fmt.brackets pp_e) expr
        

  | Ast.SMatrix(d1_expr,d2_expr) -> 
      Fmt.pf ppf 
        {|matrix%a%a|}
        (pp_transformation pp_e) trans
        Fmt.(pair ~sep:comma pp_e pp_e |> brackets) (d1_expr,d2_expr)
        

  | Ast.SArray(st,expr) -> 
      Fmt.pf ppf 
        {|array%a%a|}
        (pp_transformation pp_e) trans
        Fmt.(pair ~sep:comma 
              (fun ppf st -> 
                pp_sizedtype pp_e ppf (st,Ast.Identity)
              ) pp_e 
              |> brackets
            ) (st,expr)

let pp_top_var_decl pp_e ppf { tvident; tvtype; tvtrans; _ } = 
  Fmt.pf ppf 
    {|%a@ %s|}
    (pp_sizedtype pp_e) (tvtype,tvtrans)
    tvident

type 'e top_var_table = (string, 'e top_var_decl) Map.Poly.t [@@deriving sexp]

let pp_top_var_table pp_e ppf (tbl : 'e top_var_table) = 
  Map.Poly.data tbl
  |> Fmt.pf ppf 
    {|@[<v>%a@]|}
    Fmt.(list ~sep:cut (pp_top_var_decl pp_e))
      

type ('e, 's) prog =
  { functions_block: 's list
  ; data_vars : 'e top_var_table
  ; tdata_vars : 'e top_var_table
  ; prepare_data : 's list
  ; params : 'e top_var_table
  ; tparams : 'e top_var_table
  ; prepare_params : 's list
    (* XXX too intimately tied up with stan reader.hpp and writer.hpp in codegen
      TODO: codegen parameter constraining and unconstraining in prepare_params
    *)
  ; log_prob : 's list
  ; gen_quant_vars : 'e top_var_table
  ; generate_quantities : 's list
  ; prog_name : string
  ; prog_path : string }
[@@deriving sexp]

let pp_statement_block label pp_s ppf  = 
  Fmt.pf ppf 
    "%s@ @[<v 2>{%a@]}"
    label 
    Fmt.(list ~sep:cut pp_s)

let pp_decl_block label pp_e ppf =
  Fmt.pf ppf 
    "%s@ @[<v 2>{%a@]}"
    label 
    (pp_top_var_table pp_e)

let pp_prog pp_e pp_s ppf prog = 
  Fmt.pf ppf 
    "%a@;%a@;%a@;%a@;%a@;%a@;%a@;%a@;%a@;%a@;"
    (pp_statement_block "functions" pp_s) prog.functions_block
    (pp_decl_block "data" pp_e) prog.data_vars
    (pp_decl_block "transformed_data" pp_e) prog.tdata_vars
    (pp_statement_block "prepare_data" pp_s) prog.prepare_data
    (pp_decl_block "parameters" pp_e) prog.params
    (pp_decl_block "transformed_parameters" pp_e) prog.tparams
    (pp_statement_block "prepare_parameters" pp_s) prog.prepare_params
    (pp_statement_block "log_prob" pp_s) prog.log_prob
    (pp_decl_block "generated_quantities_variables" pp_e) prog.gen_quant_vars
    (pp_statement_block "generated_quantities" pp_s) prog.generate_quantities

type expr_typed_located =
  { texpr_type: Ast.unsizedtype
  ; texpr_loc: Ast.location_span sexp_opaque [@compare.ignore]
  ; texpr: expr_typed_located expr
  ; texpr_adlevel: autodifftype }
[@@deriving sexp, hash]

let rec pp_texpr_typed_located ppf { texpr; _ }  = 
  pp_expr pp_texpr_typed_located ppf texpr

type stmt_loc =
  { sloc: Ast.location_span sexp_opaque [@compare.ignore]
  ; stmt: (expr_typed_located, stmt_loc) statement }
[@@deriving sexp, hash]

let rec pp_stmt_loc ppf { stmt; _ }  = 
  pp_statement pp_texpr_typed_located pp_stmt_loc ppf stmt

type typed_prog = (expr_typed_located, stmt_loc) prog [@@deriving sexp]

let pp_typed_prog : typed_prog Fmt.t = 
  pp_prog pp_texpr_typed_located pp_stmt_loc 

(* ===================== Some helper functions and values ====================== *)

(** Dives into any number of nested blocks and lists, but will not recurse other
    places statements occur in the MIR (e.g. loop bodies) *)
let rec map_toplevel_stmts f {sloc; stmt} =
  match stmt with
  | Block ls -> {stmt= Block (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | SList ls -> {stmt= SList (List.map ~f:(map_toplevel_stmts f) ls); sloc}
  | _ -> f {sloc; stmt}

let tvdecl_to_decl {tvident; tvtype; tvloc; _} = (tvident, tvtype, tvloc)

let internal_expr =
  { texpr= Var "UHOH"
  ; texpr_loc= no_span
  ; texpr_type= UInt
  ; texpr_adlevel= DataOnly }

let zero = 
  { internal_expr with texpr= Lit (Int, "0")
  ; texpr_type= UInt }
