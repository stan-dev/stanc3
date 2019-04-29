open Core_kernel
open Mir

(* XXX fix exn *)
let unwrap_return_exn = function
  | Some (ReturnType ut) -> ut
  | x -> raise_s [%message "Unexpected return type " (x : returntype option)]

let trans_fn_kind = function
  | Ast.StanLib -> Mir.StanLib
  | UserDefined -> UserDefined

let rec op_to_funapp op args =
  { expr= FunApp (StanLib, string_of_operator op, trans_exprs args)
  ; emeta=
      { mtype= Semantic_check.operator_return_type op args |> unwrap_return_exn
      ; mloc= Ast.expr_loc_lub args
      ; madlevel= Ast.expr_ad_lub args } }

and trans_expr {Ast.expr; Ast.emeta} =
  let mtype = emeta.Ast.type_
  and mloc = emeta.loc
  and madlevel = emeta.ad_level in
  match expr with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, op, rhs) -> op_to_funapp op [lhs; rhs]
  | PrefixOp (op, e) | Ast.PostfixOp (e, op) -> op_to_funapp op [e]
  | _ ->
      let expr =
        match expr with
        | Ast.TernaryIf (cond, ifb, elseb) ->
            TernaryIf (trans_expr cond, trans_expr ifb, trans_expr elseb)
        | Variable {name; _} -> Var name
        | IntNumeral x -> Lit (Int, x)
        | RealNumeral x -> Lit (Real, x)
        | FunApp (fn_kind, {name; _}, args) ->
            FunApp (trans_fn_kind fn_kind, name, trans_exprs args)
        | Ast.CondDistApp ({name; _}, args) ->
            FunApp (Mir.StanLib, name, trans_exprs args)
        | GetLP | GetTarget -> Var "target"
        | ArrayExpr eles ->
            FunApp
              ( Mir.CompilerInternal
              , string_of_internal_fn FnMakeArray
              , trans_exprs eles )
        | RowVectorExpr eles ->
            FunApp
              ( Mir.CompilerInternal
              , string_of_internal_fn FnMakeRowVec
              , trans_exprs eles )
        | Indexed (lhs, indices) ->
            Indexed (trans_expr lhs, List.map ~f:trans_idx indices)
        | Paren _ | BinOp _ | PrefixOp _ | PostfixOp _ ->
            raise_s [%message "Impossible!"]
      in
      {expr; emeta= {mtype; mloc; madlevel}}

and trans_idx = function
  | Ast.All -> All
  | Ast.Upfrom e -> Upfrom (trans_expr e)
  | Ast.Downfrom e -> Downfrom (trans_expr e)
  | Ast.Between (lb, ub) -> Between (trans_expr lb, trans_expr ub)
  | Ast.Single e -> (
    match e.emeta.type_ with
    | UInt -> Single (trans_expr e)
    | UArray _ -> MultiIndex (trans_expr e)
    | _ ->
        raise_s
          [%message "Expecting int or array" (e.emeta.type_ : unsizedtype)] )

and trans_exprs = List.map ~f:trans_expr

let trans_sizedtype = map_sizedtype trans_expr

let neg_inf =
  { expr= FunApp (StanLib, string_of_internal_fn FnNegInf, [])
  ; emeta= {mtype= UReal; mloc= no_span; madlevel= DataOnly} }

let trans_arg (adtype, ut, ident) = (adtype, ident.Ast.name, ut)

let truncate_dist ast_obs t =
  let trunc cond_op (x : Ast.typed_expression) y =
    let smeta = x.Ast.emeta.loc in
    { smeta
    ; stmt=
        IfElse
          ( op_to_funapp cond_op [ast_obs; x]
          , {smeta; stmt= TargetPE neg_inf}
          , y ) }
  in
  match t with
  | Ast.NoTruncate -> []
  | TruncateUpFrom lb -> [trunc Less lb None]
  | TruncateDownFrom ub -> [trunc Greater ub None]
  | TruncateBetween (lb, ub) -> [trunc Less lb (Some (trunc Greater ub None))]

let unquote s =
  if s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.drop_suffix (String.drop_prefix s 1) 1
  else s

(* hack(sean): strings aren't real
   XXX add UString to MIR and maybe AST.
*)
let mkstring mloc s =
  {expr= Lit (Str, s); emeta= {mtype= UReal; mloc; madlevel= DataOnly}}

let trans_printables mloc (ps : Ast.typed_expression Ast.printable list) =
  List.map
    ~f:(function
      | Ast.PString s -> mkstring mloc (unquote s)
      | Ast.PExpr e -> trans_expr e )
    ps

(** [add_index expression index] returns an expression that (additionally)
    indexes into the input [expression] by [index].*)
let add_int_index e i =
  let mtype =
    Semantic_check.inferred_unsizedtype_of_indexed e.emeta.mloc e.emeta.mtype
      [(i, UInt)]
  in
  let mir_i = trans_idx i in
  let expr =
    match e.expr with
    | Var _ -> Indexed (e, [mir_i])
    | Indexed (e, indices) -> Indexed (e, indices @ [mir_i])
    | _ -> raise_s [%message "These should go away with Ryan's LHS"]
  in
  {expr; emeta= {e.emeta with mtype}}

(** [mkfor] returns a MIR For statement that iterates over the given expression
    [iteratee]. *)
let mkfor bodyfn iteratee smeta =
  let idx s =
    Ast.Single
      (Ast.mk_typed_expression
         ~expr:(Ast.Variable {name= s; id_loc= smeta})
         ~loc:smeta ~type_:UInt ~ad_level:DataOnly)
  in
  let loopvar, reset = Util.gensym_enter () in
  let lower = {expr= Lit (Int, "0"); emeta= internal_meta} in
  let upper =
    { expr= FunApp (StanLib, string_of_internal_fn FnLength, [iteratee])
    ; emeta= internal_meta }
  in
  let stmt = Block [bodyfn (add_int_index iteratee (idx loopvar))] in
  reset () ;
  {stmt= For {loopvar; lower; upper; body= {stmt; smeta}}; smeta}

(** [for_scalar unsizedtype...] generates a For statement that loops
    over the scalars in the underlying [unsizedtype].

    We can call [bodyfn] directly on scalars, make a direct For loop
    around Eigen types, or for Arrays we call mkfor but inserting a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_scalar bodyfn var smeta =
  match var.emeta.mtype with
  | UInt | UReal -> bodyfn var
  | UVector | URowVector | UMatrix -> mkfor bodyfn var smeta
  | UArray _ -> mkfor (fun e -> for_scalar bodyfn e smeta) var smeta
  | UFun _ | UMathLibraryFunction ->
      raise_s [%message "Can't for over " (var : mtype_loc_ad with_expr)]

(** [for_eigen unsizedtype...] generates a For statement that loops
    over the eigen types in the underlying [unsizedtype]; i.e. just iterating
    overarrays and running bodyfn on any eign types found within.

    We can call [bodyfn] directly on scalars and Eigen types;
    for Arrays we call mkfor but insert a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_eigen bodyfn var smeta =
  match var.emeta.mtype with
  | UInt | UReal | UVector | URowVector | UMatrix -> bodyfn var
  | UArray _ -> mkfor (fun e -> for_eigen bodyfn e smeta) var smeta
  | UFun _ | UMathLibraryFunction ->
      raise_s [%message "Can't for over " (var : expr_typed_located)]

(* These types signal the context for a declaration during statement translation.
   They are only interpreted by trans_decl.*)
type readaction = ReadData | ReadParam [@@deriving sexp]
type constrainaction = Check | Constrain | Unconstrain [@@deriving sexp]

let constrainaction_fname c =
  string_of_internal_fn
    ( match c with
    | Check -> FnCheck
    | Constrain -> FnConstrain
    | Unconstrain -> FnUnconstrain )

type decl_context =
  { dread: readaction option
  ; dconstrain: constrainaction option
  ; dadlevel: autodifftype }

let rec unsizedtype_to_string = function
  | UMatrix -> "matrix"
  | UVector -> "vector"
  | URowVector -> "row_vector"
  | UReal -> "real"
  | UInt -> "int"
  | UArray t -> unsizedtype_to_string t
  | t ->
      raise_s
        [%message "Another place where it's weird to get " (t : unsizedtype)]

(* Well, when you put it like this it does seem a little crazy *)
let constraint_to_string t (c : constrainaction) =
  match t with
  | Ast.Ordered -> "ordered"
  | PositiveOrdered -> "positive_ordered"
  | Simplex -> "simplex"
  | UnitVector -> "unit_vector"
  | CholeskyCorr -> "cholesky_factor_corr"
  | CholeskyCov -> "cholesky_factor"
  | Correlation -> "corr_matrix"
  | Covariance -> "cov_matrix"
  | Lower _ -> (
    match c with
    | Check -> "greater_or_equal"
    | Constrain | Unconstrain -> "lb" )
  | Upper _ -> (
    match c with Check -> "less_or_equal" | Constrain | Unconstrain -> "ub" )
  | LowerUpper _ -> (
    match c with
    | Check ->
        raise_s
          [%message "LowerUpper is really two other checks tied together"]
    | Constrain | Unconstrain -> "lub" )
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> (
    match c with Check -> "" | Constrain | Unconstrain -> "offset_multiplier" )
  | Identity -> ""

let constraint_forl = function
  | Ast.Identity | Offset _ | Ast.Multiplier _ | Ast.OffsetMultiplier _
   |Lower _ | Upper _ | LowerUpper _ ->
      for_scalar
  | Ordered | PositiveOrdered | Simplex | UnitVector | CholeskyCorr
   |CholeskyCov | Correlation | Covariance ->
      for_eigen

let extract_constraint_args = function
  | Ast.Lower a | Upper a | Offset a | Multiplier a -> [a]
  | LowerUpper (a1, a2) | OffsetMultiplier (a1, a2) -> [a1; a2]
  | Ordered | PositiveOrdered | Simplex | UnitVector | CholeskyCorr
   |CholeskyCov | Correlation | Covariance | Identity ->
      []

let rec gen_check decl_type decl_id decl_trans smeta adlevel =
  let forl = constraint_forl decl_trans in
  let chk fn args =
    let mtype = remove_size decl_type in
    forl
      (fun id ->
        { stmt=
            NRFunApp
              ( CompilerInternal
              , string_of_internal_fn FnCheck
              , fn :: id :: trans_exprs args )
        ; smeta } )
      {expr= Var decl_id; emeta= {mtype; mloc= smeta; madlevel= adlevel}}
      smeta
  in
  let constraint_str =
    mkstring smeta (constraint_to_string decl_trans Check)
  in
  let args = extract_constraint_args decl_trans in
  match decl_trans with
  | Identity | Offset _ | Multiplier _ | OffsetMultiplier (_, _) -> []
  | LowerUpper (lb, ub) ->
      gen_check decl_type decl_id (Ast.Lower lb) smeta adlevel
      @ gen_check decl_type decl_id (Ast.Upper ub) smeta adlevel
  | _ -> [chk constraint_str args]

(* use nested funapp for each call to read_data with just the name and size? *)
let gen_constraint dconstrain t arg =
  let mkstring = mkstring arg.emeta.mloc in
  match Option.map ~f:(constraint_to_string t) dconstrain with
  | None | Some "" -> arg
  | Some constraint_str ->
      let dc = Option.value_exn dconstrain in
      let fname = constrainaction_fname dc in
      let args =
        arg :: mkstring constraint_str
        :: mkstring (unsizedtype_to_string arg.emeta.mtype)
        :: trans_exprs (extract_constraint_args t)
      in
      {expr= FunApp (CompilerInternal, fname, args); emeta= arg.emeta}

let rec base_type = function
  | SArray (t, _) -> base_type t
  | SVector _ | SRowVector _ | SMatrix _ -> UReal
  | x -> remove_size x

let expr_to_lhs {expr; _} =
  let throw () =
    raise_s
      [%message "Was expecting LHS, got " (expr : expr_typed_located expr)]
  in
  match expr with
  | Var v -> (v, [])
  | Indexed (v, indices) ->
      ((match v.expr with Var v -> v | _ -> throw ()), indices)
  | _ -> throw ()

let mkparamread id var dconstrain sizedtype transform smeta =
  let read_base var =
    { expr=
        FunApp
          ( CompilerInternal
          , string_of_internal_fn FnReadParam
          , [mkstring var.emeta.mloc id] )
    ; emeta= {var.emeta with mtype= base_type sizedtype} }
  in
  let vident, indices = expr_to_lhs var in
  let constrain var =
    { stmt=
        Assignment
          ( (vident, indices)
          , gen_constraint dconstrain transform (read_base var) )
    ; smeta }
  in
  for_eigen constrain var smeta

let mkdataread id var sizedtype smeta =
  let read_base var =
    { expr=
        FunApp
          ( CompilerInternal
          , string_of_internal_fn FnReadData
          , [mkstring var.emeta.mloc id] )
    ; emeta= {var.emeta with mtype= base_type sizedtype} }
  in
  let vident, indices = expr_to_lhs var in
  let read_assign var =
    {stmt= Assignment ((vident, indices), read_base var); smeta}
  in
  for_scalar read_assign var smeta

let trans_decl {dread; dconstrain; dadlevel} smeta sizedtype transform
    identifier initial_value =
  let with_smeta stmt = {stmt; smeta} in
  let decl_id = identifier.Ast.name in
  let rhs = Option.map ~f:trans_expr initial_value in
  let assign rhs = {stmt= Assignment ((decl_id, []), rhs); smeta} in
  let decl_type = trans_sizedtype sizedtype in
  let decl_var =
    { expr= Var decl_id
    ; emeta= {mtype= remove_size sizedtype; madlevel= dadlevel; mloc= smeta} }
  in
  let read_stmt =
    match (dread, dconstrain) with
    | Some ReadData, Some Check -> mkdataread decl_id decl_var decl_type smeta
    | Some ReadParam, Some Constrain | Some ReadParam, Some Unconstrain ->
        mkparamread decl_id decl_var dconstrain decl_type transform smeta
    | None, _ -> Option.value_map ~default:{stmt= Skip; smeta} ~f:assign rhs
    | _ ->
        raise_s
          [%message
            "unexpected dread, constrain combo: "
              ((dread, dconstrain) : readaction option * constrainaction option)]
  in
  let decl_adtype =
    match rhs with
    | Some {emeta= {madlevel; _}; _} -> madlevel
    | None -> dadlevel
  in
  let decl = Decl {decl_adtype; decl_id; decl_type} |> with_smeta in
  let checks =
    (* XXX checks should be performed after assignment as NRFunApp*)
    match dconstrain with
    | Some Check -> gen_check decl_type decl_id transform smeta dadlevel
    | _ -> []
  in
  decl :: read_stmt :: checks
  |> List.filter ~f:(function {stmt= Skip; _} -> false | _ -> true)

let unwrap_block = function
  | [({stmt= Block _; _} as b)] -> b
  | x -> raise_s [%message "Expecting a block, not" (x : stmt_loc list)]

let rec trans_stmt declc (ts : Ast.typed_statement) =
  let stmt_typed = ts.stmt and smeta = ts.smeta.loc in
  let trans_stmt = trans_stmt {declc with dread= None; dconstrain= None} in
  let trans_single_stmt s = trans_stmt s |> List.hd_exn in
  let swrap stmt = [{stmt; smeta}] in
  let mloc = smeta in
  match stmt_typed with
  | Ast.Assignment {assign_indices; assign_rhs; assign_identifier; assign_op}
    ->
      let wrap_expr expr_typed =
        Ast.mk_typed_expression ~expr:expr_typed ~loc:smeta
          ~ad_level:assign_rhs.emeta.ad_level ~type_:assign_rhs.emeta.type_
      in
      let assignee = wrap_expr @@ Ast.Variable assign_identifier in
      let assignee =
        match assign_indices with
        | [] -> assignee
        | lst -> wrap_expr @@ Ast.Indexed (assignee, lst)
      in
      let rhs =
        match assign_op with
        | Ast.Assign | Ast.ArrowAssign -> trans_expr assign_rhs
        | Ast.OperatorAssign op -> op_to_funapp op [assignee; assign_rhs]
      in
      Assignment
        ((assign_identifier.name, List.map ~f:trans_idx assign_indices), rhs)
      |> swrap
  | Ast.NRFunApp (fn_kind, {name; _}, args) ->
      NRFunApp (trans_fn_kind fn_kind, name, trans_exprs args) |> swrap
  | Ast.IncrementLogProb e | Ast.TargetPE e -> TargetPE (trans_expr e) |> swrap
  | Ast.Tilde {arg; distribution; args; truncation} ->
      let add_dist =
        (* XXX distribution name suffix? *)
        (* XXX Reminder to differentiate between tilde, which drops constants, and
         vanilla target +=, which doesn't. Can use _unnormalized or something.*)
        TargetPE
          { expr= FunApp (StanLib, distribution.name, trans_exprs (arg :: args))
          ; emeta= {mloc; madlevel= Ast.expr_ad_lub (arg :: args); mtype= UReal}
          }
      in
      truncate_dist arg truncation @ [{smeta; stmt= add_dist}]
  | Ast.Print ps ->
      NRFunApp
        ( CompilerInternal
        , string_of_internal_fn FnPrint
        , trans_printables smeta ps )
      |> swrap
  | Ast.Reject ps ->
      NRFunApp
        ( CompilerInternal
        , string_of_internal_fn FnReject
        , trans_printables smeta ps )
      |> swrap
  | Ast.IfThenElse (cond, ifb, elseb) ->
      IfElse
        ( trans_expr cond
        , trans_single_stmt ifb
        , Option.map ~f:trans_single_stmt elseb )
      |> swrap
  | Ast.While (cond, body) ->
      While (trans_expr cond, trans_single_stmt body) |> swrap
  | Ast.For {loop_variable; lower_bound; upper_bound; loop_body} ->
      For
        { loopvar= loop_variable.Ast.name
        ; lower= trans_expr lower_bound
        ; upper= trans_expr upper_bound
        ; body= trans_single_stmt loop_body }
      |> swrap
  | Ast.ForEach (loopvar, iteratee, body) ->
      let newsym = Util.gensym () in
      let wrap expr = {expr; emeta= {mloc; mtype= UInt; madlevel= DataOnly}} in
      let iteratee = trans_expr iteratee
      and indexing_var = wrap (Var newsym) in
      let assign_loopvar =
        Assignment
          ( (loopvar.name, [])
          , Indexed (iteratee, [Single indexing_var]) |> wrap )
      in
      let assign_loopvar = {stmt= assign_loopvar; smeta} in
      let body =
        match trans_single_stmt body with
        | {stmt= Block body_stmts; smeta} ->
            {stmt= Block (assign_loopvar :: body_stmts); smeta}
        | {stmt; smeta} -> {stmt= Block [assign_loopvar; {stmt; smeta}]; smeta}
      in
      For
        (* XXX Do loops in MIR actually start at 1? *)
        { loopvar= newsym
        ; lower= wrap @@ Lit (Int, "0")
        ; upper=
            wrap
            @@ FunApp
                 ( Mir.CompilerInternal
                 , string_of_internal_fn FnLength
                 , [iteratee] )
        ; body }
      |> swrap
  | Ast.FunDef _ ->
      raise_s
        [%message
          "Found function definition statement outside of function block"]
  | Ast.VarDecl
      {sizedtype; transformation; identifier; initial_value; is_global} ->
      ignore is_global ;
      trans_decl declc smeta sizedtype transformation identifier initial_value
  | Ast.Block stmts -> Block (List.concat_map ~f:trans_stmt stmts) |> swrap
  | Ast.Return e -> Return (Some (trans_expr e)) |> swrap
  | Ast.ReturnVoid -> Return None |> swrap
  | Ast.Break -> Break |> swrap
  | Ast.Continue -> Continue |> swrap
  | Ast.Skip -> Skip |> swrap

let trans_fun_def declc (ts : Ast.typed_statement) =
  let stmt_typed = ts.stmt and sloc = ts.smeta.loc in
  let trans_stmt = trans_stmt {declc with dread= None; dconstrain= None} in
  let stmt =
    match stmt_typed with
    | Ast.FunDef {returntype; funname; arguments; body} ->
        { fdrt=
            (match returntype with Void -> None | ReturnType ut -> Some ut)
        ; fdname= funname.name
        ; fdargs= List.map ~f:trans_arg arguments
        ; fdbody= trans_stmt body |> unwrap_block
        ; fdloc= sloc }
    | _ ->
        raise_s
          [%message
            "Found non-function definition statement in function block"]
  in
  stmt

let trans_prog filename
    { Ast.functionblock
    ; datablock
    ; transformeddatablock
    ; parametersblock
    ; transformedparametersblock
    ; modelblock
    ; generatedquantitiesblock } : typed_prog =
  (*
     1. prepare_params: add read_param calls (same call should constrain?)
          maybe read(constrained()), constrain(read()), or read("constraint", ...)
     1. prepare_params: add tparams 's; add checks
     2. transform_inits: add read_param calls (same call should unconstrain?)
     3. prepare_data: add read_data calls and checks
     4. prepare_data: add tdata 's and checks
     5. add write() calls to generate_quantities for params, tparams...
             shit these are conditional depending on the flag.
             add the flag to the call to write?
           apparently tdata aren't written anywhere

     during code gen:
     get_param_names: scan prepare_params for Decl at top level
     constrained_param_names: needs to tell between tparams and gqs and not
     unconstrained param names: same, but also some funky
        adjustments for unconstrained space: ???
*)
  let map f list_op = Option.value ~default:[] list_op |> List.concat_map ~f in
  let grab_names_sizes paramblock block =
    let get_name_size s =
      match s.Ast.stmt with
      | Ast.VarDecl {sizedtype; identifier; _} ->
          Some (identifier.name, (trans_sizedtype sizedtype, paramblock))
      | _ -> None
    in
    List.map ~f:get_name_size (Option.value ~default:[] block)
  in
  let output_vars =
    [ grab_names_sizes Parameters parametersblock
    ; grab_names_sizes TransformedParameters transformedparametersblock
    ; grab_names_sizes GeneratedQuantities generatedquantitiesblock ]
    |> List.concat |> List.filter_opt
  and input_vars = grab_names_sizes Data datablock |> List.filter_opt in
  let prepare_data =
    map
      (trans_stmt
         {dread= Some ReadData; dconstrain= Some Check; dadlevel= DataOnly})
      datablock
    @ map
        (trans_stmt {dread= None; dconstrain= Some Check; dadlevel= DataOnly})
        transformeddatablock
  in
  let prepare_params =
    map
      (trans_stmt
         { dread= Some ReadParam
         ; dconstrain= Some Constrain
         ; dadlevel= AutoDiffable })
      parametersblock
    @ map
        (trans_stmt
           {dread= None; dconstrain= Some Check; dadlevel= AutoDiffable})
        transformedparametersblock
  in
  let modelb =
    map
      (trans_stmt {dread= None; dconstrain= None; dadlevel= AutoDiffable})
      modelblock
  in
  let log_prob =
    prepare_params
    @
    match modelb with
    | [] -> []
    | hd :: _ -> [{stmt= Block modelb; smeta= hd.smeta}]
  in
  let generate_quantities =
    prepare_params
    @ map
        (trans_stmt {dread= None; dconstrain= Some Check; dadlevel= DataOnly})
        generatedquantitiesblock
  in
  let transform_inits =
    map
      (trans_stmt
         { dread= Some ReadParam
         ; dconstrain= Some Unconstrain
         ; dadlevel= DataOnly })
      parametersblock
  in
  { functions_block=
      (* Should this be AutoDiffable for functions here?*)
      Option.value_map functionblock ~default:[] ~f:(fun fundefs ->
          List.map fundefs ~f:(fun fundef ->
              trans_fun_def
                {dread= None; dconstrain= None; dadlevel= AutoDiffable}
                fundef ) )
  ; input_vars
  ; prepare_data
  ; log_prob
  ; generate_quantities
  ; transform_inits
  ; output_vars
  ; prog_name= !Semantic_check.model_name
  ; prog_path= filename }

(*===================== tests =========================================*)

let mir_from_string s =
  Parse.parse_string Parser.Incremental.program s
  |> Semantic_check.semantic_check_program |> trans_prog ""

let%expect_test "Prefix-Op-Example" =
  let mir =
    mir_from_string
      {|
        model {
          int i;
          if (i < -1)
            print("Badger");
        }
      |}
  in
  let op = mir.log_prob in
  print_s [%sexp (op : Mir.stmt_loc list)] ;
  (* Perhaps this is producing too many nested lists. XXX*)
  [%expect
    {|
      ((Block
        ((Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type SInt))
         (IfElse 
          (FunApp StanLib Less__ ((Var i) (FunApp StanLib PMinus__ ((Lit Int 1)))))
          (NRFunApp CompilerInternal FnPrint__ ((Lit Str Badger))) ())))) |}]

let%expect_test "read data" =
  let m = mir_from_string "data { matrix[10, 20] mat[5]; }" in
  print_s [%sexp (m.prepare_data : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype DataOnly) (decl_id mat)
      (decl_type (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5))))
     (For (loopvar sym1__) (lower (Lit Int 0))
      (upper (FunApp StanLib FnLength__ ((Var mat))))
      (body
       (Block
        ((For (loopvar sym2__) (lower (Lit Int 0))
          (upper
           (FunApp StanLib FnLength__ 
            ((Indexed (Var mat) ((Single (Var sym1__)))))))
          (body
           (Block 
            ((Assignment (mat ()) 
              (FunApp CompilerInternal FnReadData__ ((Lit Str mat))))))))))))) |}]

let%expect_test "read param" =
  let m = mir_from_string "parameters { matrix<lower=0>[10, 20] mat[5]; }" in
  print_s [%sexp (m.log_prob : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype AutoDiffable) (decl_id mat)
      (decl_type (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5))))
     (For (loopvar sym1__) (lower (Lit Int 0))
      (upper (FunApp StanLib FnLength__ ((Var mat))))
      (body
       (Block
        ((Assignment (mat ())
          (FunApp CompilerInternal FnConstrain__
           ((FunApp CompilerInternal FnReadParam__ ((Lit Str mat))) (Lit Str lb) 
            (Lit Str real) (Lit Int 0))))))))) |}]

let%expect_test "gen quant" =
  let m =
    mir_from_string "generated quantities { matrix<lower=0>[10, 20] mat[5]; }"
  in
  print_s [%sexp (m.generate_quantities : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype DataOnly) (decl_id mat)
      (decl_type (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5))))
     (For (loopvar sym1__) (lower (Lit Int 0))
      (upper (FunApp StanLib FnLength__ ((Var mat))))
      (body
       (Block
        ((For (loopvar sym2__) (lower (Lit Int 0))
          (upper
           (FunApp StanLib FnLength__ 
            ((Indexed (Var mat) ((Single (Var sym1__)))))))
          (body
           (Block
            ((NRFunApp CompilerInternal FnCheck__
              ((Lit Str greater_or_equal)
               (Indexed (Var mat) ((Single (Var sym1__)) (Single (Var sym2__))))
               (Lit Int 0)))))))))))) |}]
