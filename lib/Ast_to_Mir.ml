open Core_kernel
open Mir

(* XXX fix exn *)
let unwrap_return_exn = function
  | Some (Ast.ReturnType ut) -> ut
  | x ->
      raise_s [%message "Unexpected return type " (x : Ast.returntype option)]

let rec op_to_funapp op args =
  { texpr= FunApp (Operators.operator_name op, trans_exprs args)
  ; texpr_type= Operators.operator_return_type op args |> unwrap_return_exn
  ; texpr_loc= Ast.expr_loc_lub args
  ; texpr_adlevel= Ast.expr_ad_lub args }

and trans_expr
    { Ast.expr_typed
    ; expr_typed_type= texpr_type
    ; expr_typed_loc= texpr_loc
    ; expr_typed_ad_level= texpr_adlevel } =
  match expr_typed with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, op, rhs) -> op_to_funapp op [lhs; rhs]
  | PrefixOp (op, e) | Ast.PostfixOp (e, op) -> op_to_funapp op [e]
  | _ ->
      let texpr =
        match expr_typed with
        | Ast.TernaryIf (cond, ifb, elseb) ->
            TernaryIf (trans_expr cond, trans_expr ifb, trans_expr elseb)
        | Variable {name; _} -> Var name
        | IntNumeral x -> Lit (Int, x)
        | RealNumeral x -> Lit (Real, x)
        | FunApp ({name; _}, args) | Ast.CondDistApp ({name; _}, args) ->
            FunApp (name, trans_exprs args)
        | GetLP | GetTarget -> Var "target"
        | ArrayExpr eles -> FunApp (fn_make_array, trans_exprs eles)
        | RowVectorExpr eles -> FunApp (fn_make_rowvec, trans_exprs eles)
        | Indexed (lhs, indices) ->
            Indexed (trans_expr lhs, List.map ~f:trans_idx indices)
        | Paren _ | BinOp _ | PrefixOp _ | PostfixOp _ ->
            raise_s [%message "Impossible!"]
      in
      {texpr; texpr_type; texpr_loc; texpr_adlevel}

and trans_idx = function
  | Ast.All -> All
  | Ast.Upfrom e -> Upfrom (trans_expr e)
  | Ast.Downfrom e -> Downfrom (trans_expr e)
  | Ast.Between (lb, ub) -> Between (trans_expr lb, trans_expr ub)
  | Ast.Single e -> (
    match e.expr_typed_type with
    | Ast.UInt -> Single (trans_expr e)
    | Ast.UArray _ -> MultiIndex (trans_expr e)
    | _ ->
        raise_s
          [%message
            "Expecting int or array" (e.expr_typed_type : Ast.unsizedtype)] )

and trans_exprs = List.map ~f:trans_expr

let trans_sizedtype = Ast.map_sizedtype trans_expr

let neg_inf =
  { texpr_type= UReal
  ; texpr_loc= no_span
  ; texpr= FunApp (fn_negative_infinity, [])
  ; texpr_adlevel= Ast.DataOnly }

let trans_arg (adtype, ut, ident) = (adtype, ident.Ast.name, ut)

let truncate_dist ast_obs t =
  let trunc cond_op x y =
    let sloc = x.Ast.expr_typed_loc in
    { sloc
    ; stmt=
        IfElse
          (op_to_funapp cond_op [ast_obs; x], {sloc; stmt= TargetPE neg_inf}, y)
    }
  in
  match t with
  | Ast.NoTruncate -> []
  | TruncateUpFrom lb -> [trunc Ast.Less lb None]
  | TruncateDownFrom ub -> [trunc Ast.Greater ub None]
  | TruncateBetween (lb, ub) ->
      [trunc Ast.Less lb (Some (trunc Ast.Greater ub None))]

let unquote s =
  if s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.drop_suffix (String.drop_prefix s 1) 1
  else s

(* hack(sean): strings aren't real
   XXX add UString to MIR and maybe AST.
*)
let mkstring texpr_loc s =
  { texpr= Lit (Str, s)
  ; texpr_type= Ast.UReal
  ; texpr_loc
  ; texpr_adlevel= DataOnly }

let trans_printables texpr_loc (ps : Ast.typed_expression Ast.printable list) =
  List.map
    ~f:(function
      | Ast.PString s -> mkstring texpr_loc (unquote s)
      | Ast.PExpr e -> trans_expr e)
    ps

(** [add_index expression index] returns an expression that (additionally)
    indexes into the input [expression] by [index].*)
let add_int_index e i =
  let texpr_type =
    Semantic_check.inferred_unsizedtype_of_indexed e.texpr_loc e.texpr_type
      [(i, UInt)]
  in
  let mir_i = trans_idx i in
  let texpr =
    match e.texpr with
    | Var _ -> Indexed (e, [mir_i])
    | Indexed (e, indices) -> Indexed (e, indices @ [mir_i])
    | _ -> raise_s [%message "These should go away with Ryan's LHS"]
  in
  {e with texpr; texpr_type}

(** [mkfor] returns a MIR For statement that iterates over the given expression
    [iteratee]. *)
let mkfor bodyfn iteratee sloc =
  let idx s =
    let expr_typed = Ast.Variable {name= s; id_loc= sloc} in
    Ast.Single
      { Ast.expr_typed_loc= sloc
      ; expr_typed
      ; expr_typed_ad_level= DataOnly
      ; expr_typed_type= UInt }
  in
  let loopvar, reset = Util.gensym_enter () in
  let lower = {internal_expr with texpr= Lit (Int, "0")} in
  let upper = {internal_expr with texpr= FunApp (fn_length, [iteratee])} in
  let stmt = Block [bodyfn (add_int_index iteratee (idx loopvar))] in
  reset () ;
  {stmt= For {loopvar; lower; upper; body= {stmt; sloc}}; sloc}

(** [for_scalar unsizedtype...] generates a For statement that loops
    over the scalars in the underlying [unsizedtype].

    We can call [bodyfn] directly on scalars, make a direct For loop
    around Eigen types, or for Arrays we call mkfor but inserting a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_scalar bodyfn var sloc =
  match var.texpr_type with
  | Ast.UInt | UReal -> bodyfn var
  | UVector | URowVector | UMatrix -> mkfor bodyfn var sloc
  | UArray _ -> mkfor (fun e -> for_scalar bodyfn e sloc) var sloc
  | UFun _ | UMathLibraryFunction ->
      raise_s [%message "Can't for over " (var : expr_typed_located)]

(** [for_eigen unsizedtype...] generates a For statement that loops
    over the eigen types in the underlying [unsizedtype]; i.e. just iterating
    overarrays and running bodyfn on any eign types found within.

    We can call [bodyfn] directly on scalars and Eigen types;
    for Arrays we call mkfor but insert a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_eigen bodyfn var sloc =
  match var.texpr_type with
  | Ast.UInt | UReal | UVector | URowVector | UMatrix -> bodyfn var
  | UArray _ -> mkfor (fun e -> for_eigen bodyfn e sloc) var sloc
  | UFun _ | UMathLibraryFunction ->
      raise_s [%message "Can't for over " (var : expr_typed_located)]

(* These types signal the context for a declaration during statement translation.
   They are only interpreted by trans_decl.*)
type readaction = ReadData | ReadParam [@@deriving sexp]
type constrainaction = Check | Constrain | Unconstrain [@@deriving sexp]

let constrainaction_fname = function
  | Check -> fn_check
  | Constrain -> fn_constrain
  | Unconstrain -> fn_unconstrain

type decl_context =
  { dread: readaction option
  ; dconstrain: constrainaction option
  ; dadlevel: autodifftype }

let rec unsizedtype_to_string = function
  | Ast.UMatrix -> "matrix"
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

let rec gen_check decl_type decl_id decl_trans sloc adlevel =
  let forl = constraint_forl decl_trans in
  let chk fn args =
    let texpr_type = Ast.remove_size decl_type in
    forl
      (fun id ->
        {stmt= NRFunApp (fn_check, fn :: id :: trans_exprs args); sloc} )
      {texpr= Var decl_id; texpr_type; texpr_loc= sloc; texpr_adlevel= adlevel}
      sloc
  in
  let constraint_str = mkstring sloc (constraint_to_string decl_trans Check) in
  let args = extract_constraint_args decl_trans in
  match decl_trans with
  | Ast.Identity | Offset _ | Ast.Multiplier _ | Ast.OffsetMultiplier (_, _) ->
      []
  | LowerUpper (lb, ub) ->
      gen_check decl_type decl_id (Ast.Lower lb) sloc adlevel
      @ gen_check decl_type decl_id (Ast.Upper ub) sloc adlevel
  | _ -> [chk constraint_str args]

(* use nested funapp for each call to read_data with just the name and size? *)
let gen_constraint dconstrain t arg =
  let mkstring = mkstring arg.texpr_loc in
  match Option.map ~f:(constraint_to_string t) dconstrain with
  | None | Some "" -> arg
  | Some constraint_str ->
      let dc = Option.value_exn dconstrain in
      let fname = constrainaction_fname dc in
      let args =
        arg :: mkstring constraint_str
        :: mkstring (unsizedtype_to_string arg.texpr_type)
        :: trans_exprs (extract_constraint_args t)
      in
      {arg with texpr= FunApp (fname, args)}

let rec base_type = function
  | Ast.SArray (t, _) -> base_type t
  | SVector _ | SRowVector _ | SMatrix _ -> Ast.UReal
  | x -> Ast.remove_size x

let mkparamread id var dconstrain sizedtype transform sloc =
  let read_base var =
    { var with
      texpr= FunApp (fn_read_param, [mkstring var.texpr_loc id])
    ; texpr_type= base_type sizedtype }
  in
  let constrain var =
    { stmt=
        Assignment (var, gen_constraint dconstrain transform (read_base var))
    ; sloc }
  in
  for_eigen constrain var sloc

let mkdataread id var sizedtype sloc =
  let read_base var =
    { var with
      texpr= FunApp (fn_read_data, [mkstring var.texpr_loc id])
    ; texpr_type= base_type sizedtype }
  in
  let read_assign var = {stmt= Assignment (var, read_base var); sloc} in
  for_scalar read_assign var sloc

let trans_decl {dread; dconstrain; dadlevel} sloc sizedtype transform
    identifier initial_value =
  let with_sloc stmt = {stmt; sloc} in
  let decl_id = identifier.Ast.name in
  let rhs = Option.map ~f:trans_expr initial_value in
  let assign rhs =
    {stmt= Assignment ({rhs with texpr= Var decl_id}, rhs); sloc}
  in
  let decl_type = trans_sizedtype sizedtype in
  let decl_var =
    { texpr= Var decl_id
    ; texpr_type= Ast.remove_size sizedtype
    ; texpr_adlevel= dadlevel
    ; texpr_loc= sloc }
  in
  let read_stmt =
    match (dread, dconstrain) with
    | Some ReadData, Some Check -> mkdataread decl_id decl_var decl_type sloc
    | Some ReadParam, Some Constrain | Some ReadParam, Some Unconstrain ->
        mkparamread decl_id decl_var dconstrain decl_type transform sloc
    | None, _ -> Option.value_map ~default:{stmt= Skip; sloc} ~f:assign rhs
    | _ ->
        raise_s
          [%message
            "unexpected dread, constrain combo: "
              ((dread, dconstrain) : readaction option * constrainaction option)]
  in
  let decl_adtype =
    match rhs with
    | Some {texpr_adlevel; _} -> texpr_adlevel
    | None -> dadlevel
  in
  let decl = Decl {decl_adtype; decl_id; decl_type} |> with_sloc in
  let checks =
    (* XXX checks should be performed after assignment as NRFunApp*)
    match dconstrain with
    | Some Check -> gen_check decl_type decl_id transform sloc dadlevel
    | _ -> []
  in
  decl :: read_stmt :: checks
  |> List.filter ~f:(function {stmt= Skip; _} -> false | _ -> true)

let unwrap_block = function
  | [({stmt= Block _; _} as b)] -> b
  | x -> raise_s [%message "Expecting a block, not" (x : stmt_loc list)]

let rec trans_stmt declc {Ast.stmt_typed; stmt_typed_loc= sloc; _} =
  let trans_stmt = trans_stmt {declc with dread= None; dconstrain= None} in
  let trans_single_stmt s = trans_stmt s |> List.hd_exn in
  let swrap stmt = [{stmt; sloc}] in
  let texpr_loc = sloc in
  match stmt_typed with
  | Ast.Assignment {assign_indices; assign_rhs; assign_identifier; assign_op}
    ->
      let wrap_expr expr_typed =
        { Ast.expr_typed_loc= sloc
        ; expr_typed_ad_level= assign_rhs.expr_typed_ad_level
        ; expr_typed_type= assign_rhs.expr_typed_type
        ; expr_typed }
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
      Assignment (trans_expr assignee, rhs) |> swrap
  | Ast.NRFunApp ({name; _}, args) ->
      NRFunApp (name, trans_exprs args) |> swrap
  | Ast.IncrementLogProb e | Ast.TargetPE e -> TargetPE (trans_expr e) |> swrap
  | Ast.Tilde {arg; distribution; args; truncation} ->
      let add_dist =
        (* XXX distribution name suffix? *)
        (* XXX Reminder to differentiate between tilde, which drops constants, and
             vanilla target +=, which doesn't. Can use _unnormalized or something.*)
        TargetPE
          { texpr= FunApp (distribution.name, trans_exprs (arg :: args))
          ; texpr_loc
          ; texpr_adlevel= Ast.expr_ad_lub (arg :: args)
          ; texpr_type= UReal }
      in
      truncate_dist arg truncation @ [{sloc; stmt= add_dist}]
  | Ast.Print ps -> NRFunApp (fn_print, trans_printables sloc ps) |> swrap
  | Ast.Reject ps -> NRFunApp (fn_reject, trans_printables sloc ps) |> swrap
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
      let wrap texpr =
        {texpr; texpr_loc; texpr_type= UInt; texpr_adlevel= DataOnly}
      in
      let iteratee = trans_expr iteratee
      and indexing_var = wrap (Var newsym) in
      let assign_loopvar =
        Assignment
          ( Var loopvar.name |> wrap
          , Indexed (iteratee, [Single indexing_var]) |> wrap )
      in
      let assign_loopvar = {stmt= assign_loopvar; sloc} in
      let body =
        match trans_single_stmt body with
        | {stmt= Block body_stmts; sloc} ->
            {stmt= Block (assign_loopvar :: body_stmts); sloc}
        | {stmt; sloc} -> {stmt= Block [assign_loopvar; {stmt; sloc}]; sloc}
      in
      For
        (* XXX Do loops in MIR actually start at 1? *)
        { loopvar= newsym
        ; lower= wrap @@ Lit (Int, "0")
        ; upper= wrap @@ FunApp (fn_length, [iteratee])
        ; body }
      |> swrap
  | Ast.FunDef {returntype; funname; arguments; body} ->
      let fdbody = trans_stmt body |> unwrap_block in
      let fdrt =
        match returntype with Ast.Void -> None | ReturnType ut -> Some ut
      in
      let fdargs = List.map ~f:trans_arg arguments in
      FunDef {fdrt; fdname= funname.name; fdargs; fdbody} |> swrap
  | Ast.VarDecl
      {sizedtype; transformation; identifier; initial_value; is_global} ->
      ignore is_global ;
      trans_decl declc sloc sizedtype transformation identifier initial_value
  | Ast.Block stmts -> Block (List.concat_map ~f:trans_stmt stmts) |> swrap
  | Ast.Return e -> Return (Some (trans_expr e)) |> swrap
  | Ast.ReturnVoid -> Return None |> swrap
  | Ast.Break -> Break |> swrap
  | Ast.Continue -> Continue |> swrap
  | Ast.Skip -> Skip |> swrap

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
      match s.Ast.stmt_typed with
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
    | hd :: _ -> [{stmt= Block modelb; sloc= hd.sloc}]
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
      map
        (trans_stmt {dread= None; dconstrain= None; dadlevel= AutoDiffable})
        functionblock
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
         (IfElse (FunApp Less__ ((Var i) (FunApp PMinus__ ((Lit Int 1)))))
          (NRFunApp Print__ ((Lit Str Badger))) ())))) |}]

let%expect_test "read data" =
  let m = mir_from_string "data { matrix[10, 20] mat[5]; }" in
  print_s [%sexp (m.prepare_data : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype DataOnly) (decl_id mat)
      (decl_type (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5))))
     (For (loopvar sym1__) (lower (Lit Int 0))
      (upper (FunApp Length__ ((Var mat))))
      (body
       (Block
        ((For (loopvar sym2__) (lower (Lit Int 0))
          (upper (FunApp Length__ ((Indexed (Var mat) ((Single (Var sym1__)))))))
          (body
           (Block
            ((Assignment
              (Indexed (Var mat) ((Single (Var sym1__)) (Single (Var sym2__))))
              (FunApp ReadData__ ((Lit Str mat))))))))))))) |}]

let%expect_test "read param" =
  let m = mir_from_string "parameters { matrix<lower=0>[10, 20] mat[5]; }" in
  print_s [%sexp (m.log_prob : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype AutoDiffable) (decl_id mat)
      (decl_type (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5))))
     (For (loopvar sym1__) (lower (Lit Int 0))
      (upper (FunApp Length__ ((Var mat))))
      (body
       (Block
        ((Assignment (Indexed (Var mat) ((Single (Var sym1__))))
          (FunApp Constrain__
           ((FunApp ReadParam__ ((Lit Str mat))) (Lit Str lb) (Lit Str real)
            (Lit Int 0))))))))) |}]

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
      (upper (FunApp Length__ ((Var mat))))
      (body
       (Block
        ((For (loopvar sym2__) (lower (Lit Int 0))
          (upper (FunApp Length__ ((Indexed (Var mat) ((Single (Var sym1__)))))))
          (body
           (Block
            ((NRFunApp Check__
              ((Lit Str greater_or_equal)
               (Indexed (Var mat) ((Single (Var sym1__)) (Single (Var sym2__))))
               (Lit Int 0)))))))))))) |}]
