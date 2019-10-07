open Core_kernel
open Middle

(* XXX fix exn *)
let unwrap_return_exn = function
  | Some (ReturnType ut) -> ut
  | x -> raise_s [%message "Unexpected return type " (x : returntype option)]

let trans_fn_kind = function
  | Ast.StanLib -> StanLib
  | UserDefined -> UserDefined

let rec op_to_funapp op args =
  let argtypes =
    List.map ~f:(fun x -> (x.Ast.emeta.Ast.ad_level, x.emeta.type_)) args
  in
  { expr= FunApp (StanLib, string_of_operator op, trans_exprs args)
  ; emeta=
      { mtype= operator_return_type op argtypes |> unwrap_return_exn
      ; mloc= Ast.expr_loc_lub args
      ; madlevel= Ast.expr_ad_lub args } }

and trans_expr {Ast.expr; Ast.emeta} =
  let mtype = emeta.Ast.type_
  and mloc = emeta.loc
  and madlevel = emeta.ad_level in
  match expr with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, And, rhs) ->
      { expr= EAnd (trans_expr lhs, trans_expr rhs)
      ; emeta= {madlevel; mloc; mtype} }
  | BinOp (lhs, Or, rhs) ->
      { expr= EOr (trans_expr lhs, trans_expr rhs)
      ; emeta= {madlevel; mloc; mtype} }
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
        | FunApp (fn_kind, {name; _}, args)
         |CondDistApp (fn_kind, {name; _}, args) ->
            FunApp (trans_fn_kind fn_kind, name, trans_exprs args)
        | GetLP | GetTarget -> FunApp (StanLib, "target", [])
        | ArrayExpr eles ->
            FunApp
              ( CompilerInternal
              , string_of_internal_fn FnMakeArray
              , trans_exprs eles )
        | RowVectorExpr eles ->
            FunApp
              ( CompilerInternal
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
  | Ast.Downfrom e -> Between (loop_bottom, trans_expr e)
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
let trans_possiblysizedtype pst = map_possiblysizedtype trans_expr pst

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
      | Ast.PExpr e -> trans_expr e)
    ps

(* These types signal the context for a declaration during statement translation.
   They are only interpreted by trans_decl.*)
type constrainaction = Check | Constrain | Unconstrain [@@deriving sexp]

let constrainaction_fname c =
  string_of_internal_fn
    ( match c with
    | Check -> FnCheck
    | Constrain -> FnConstrain
    | Unconstrain -> FnUnconstrain )

type decl_context = {dconstrain: constrainaction option; dadlevel: autodifftype}

let check_constraint_to_string t (c : constrainaction) =
  match t with
  | Ordered -> "ordered"
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

let constrain_constraint_to_string t (c : constrainaction) =
  match t with
  | CholeskyCorr -> "cholesky_corr"
  | _ -> check_constraint_to_string t c

let constraint_forl = function
  | Identity | Offset _ | Multiplier _ | OffsetMultiplier _ | Lower _
   |Upper _ | LowerUpper _ ->
      for_scalar
  | Ordered | PositiveOrdered | Simplex | UnitVector | CholeskyCorr
   |CholeskyCov | Correlation | Covariance ->
      for_eigen

let extract_transform_args = function
  | Lower a | Upper a -> [a]
  | Offset a -> [{a with expr= Lit (Int, "0")}; a]
  | Multiplier a -> [a; {a with expr= Lit (Int, "1")}]
  | LowerUpper (a1, a2) | OffsetMultiplier (a1, a2) -> [a1; a2]
  | Covariance | Correlation | CholeskyCov | CholeskyCorr | Ordered
   |PositiveOrdered | Simplex | UnitVector | Identity ->
      []

let extra_constraint_args st = function
  | Lower _ | Upper _ | Offset _ | Multiplier _ | LowerUpper _
   |OffsetMultiplier _ | Ordered | PositiveOrdered | Simplex | UnitVector
   |Identity ->
      []
  | Covariance | Correlation | CholeskyCorr -> [List.hd_exn (eigen_size st)]
  | CholeskyCov -> eigen_size st

let param_size transform sizedtype =
  let rec shrink_eigen f st =
    match st with
    | SArray (t, d) -> SArray (shrink_eigen f t, d)
    | SVector d | SMatrix (d, _) -> SVector (f d)
    | SInt | SReal | SRowVector _ ->
        raise_s
          [%message
            "Expecting SVector or SMatrix, got "
              (st : mtype_loc_ad with_expr sizedtype)]
  in
  let rec shrink_eigen_mat f st =
    match st with
    | SArray (t, d) -> SArray (shrink_eigen_mat f t, d)
    | SMatrix (d1, d2) -> SVector (f d1 d2)
    | SInt | SReal | SRowVector _ | SVector _ ->
        raise_s
          [%message
            "Expecting SMatrix, got " (st : mtype_loc_ad with_expr sizedtype)]
  in
  let int num = {expr= Lit (Int, string_of_int num); emeta= internal_meta} in
  let k_choose_2 k =
    binop (binop k Times (binop k Minus (int 1))) Divide (int 2)
  in
  match transform with
  | Identity | Lower _ | Upper _
   |LowerUpper (_, _)
   |Offset _ | Multiplier _
   |OffsetMultiplier (_, _)
   |Ordered | PositiveOrdered | UnitVector ->
      sizedtype
  | Simplex -> shrink_eigen (fun d -> binop d Minus (int 1)) sizedtype
  | CholeskyCorr | Correlation -> shrink_eigen k_choose_2 sizedtype
  | CholeskyCov ->
      (* (N * (N + 1)) / 2 + (M - N) * N *)
      shrink_eigen_mat
        (fun m n ->
          binop
            (binop (k_choose_2 n) Plus n)
            Plus
            (binop (binop m Minus n) Times n) )
        sizedtype
  | Covariance -> shrink_eigen (fun k -> binop k Plus (k_choose_2 k)) sizedtype

let remove_possibly_exn pst action loc =
  match pst with
  | Sized st -> st
  | Unsized _ ->
      raise_s
        [%message
          "Error extracting sizedtype" ~action ~loc:(loc : location_span)]

let constrain_decl decl_type dconstrain t decl_id decl_var smeta =
  let st = remove_possibly_exn decl_type "constrain" smeta in
  let mkstring = mkstring decl_var.emeta.mloc in
  match Option.map ~f:(constrain_constraint_to_string t) dconstrain with
  | None | Some "" -> []
  | Some constraint_str ->
      let dc = Option.value_exn dconstrain in
      let fname = constrainaction_fname dc in
      let extra_args =
        match dconstrain with
        | Some Constrain -> extra_constraint_args st t
        | _ -> []
      in
      let args var =
        (var :: mkstring constraint_str :: extract_transform_args t)
        @ extra_args
      in
      let constrainvar var =
        {expr= FunApp (CompilerInternal, fname, args var); emeta= var.emeta}
      in
      [ (constraint_forl t) st
          (assign_indexed (remove_size st) decl_id smeta constrainvar)
          decl_var smeta ]

let rec check_decl decl_type' decl_id decl_trans smeta adlevel =
  let decl_type = remove_possibly_exn decl_type' "check" smeta in
  let chk fn args =
    let check_id id =
      let id_str =
        { expr= Lit (Str, Fmt.strf "%a" Pretty.pp_expr_typed_located id)
        ; emeta= internal_meta }
      in
      let fname = string_of_internal_fn FnCheck in
      let stmt =
        NRFunApp (CompilerInternal, fname, fn :: id_str :: id :: args)
      in
      {stmt; smeta}
    in
    let mtype = remove_size decl_type in
    for_eigen decl_type check_id
      {expr= Var decl_id; emeta= {mtype; mloc= smeta; madlevel= adlevel}}
      smeta
  in
  let args = extract_transform_args decl_trans in
  match decl_trans with
  | Identity | Offset _ | Multiplier _ | OffsetMultiplier (_, _) -> []
  | LowerUpper (lb, ub) ->
      check_decl decl_type' decl_id (Lower lb) smeta adlevel
      @ check_decl decl_type' decl_id (Upper ub) smeta adlevel
  | _ ->
      [chk (mkstring smeta (check_constraint_to_string decl_trans Check)) args]

let trans_decl {dconstrain; dadlevel} smeta decl_type transform identifier
    initial_value =
  let decl_id = identifier.Ast.name in
  let rhs = Option.map ~f:trans_expr initial_value in
  let dt = trans_possiblysizedtype decl_type in
  let decl_adtype = dadlevel in
  let decl_var =
    { expr= Var decl_id
    ; emeta=
        {mtype= remove_possible_size decl_type; madlevel= dadlevel; mloc= smeta}
    }
  in
  let decl = {stmt= Decl {decl_adtype; decl_id; decl_type= dt}; smeta} in
  let rhs_assignment =
    Option.map
      ~f:(fun e -> {stmt= Assignment ((decl_id, e.emeta.mtype, []), e); smeta})
      rhs
    |> Option.to_list
  in
  if is_user_ident decl_id then
    let checks =
      match dconstrain with
      | Some Check -> check_decl dt decl_id transform smeta dadlevel
      | _ -> []
    in
    let constrain_stmts =
      match dconstrain with
      | Some Constrain | Some Unconstrain ->
          constrain_decl dt dconstrain transform decl_id decl_var smeta
      | _ -> []
    in
    (decl :: rhs_assignment) @ constrain_stmts @ checks
  else decl :: rhs_assignment

let unwrap_block_or_skip = function
  | [({stmt= Block _; _} as b)] | [({stmt= Skip; _} as b)] -> b
  | x ->
      raise_s [%message "Expecting a block or skip, not" (x : stmt_loc list)]

let dist_name_suffix udf_names name =
  let is_udf_name s = List.exists ~f:(( = ) s) udf_names in
  match
    Middle.distribution_suffices
    |> List.filter ~f:(fun sfx ->
           is_stan_math_function_name (name ^ sfx) || is_udf_name (name ^ sfx)
       )
    |> List.hd
  with
  | Some hd -> hd
  | None -> raise_s [%message "Couldn't find distribution " name]

let%expect_test "dist name suffix" =
  dist_name_suffix [] "normal" |> print_endline ;
  [%expect {| _log |}]

let rec trans_stmt udf_names (declc : decl_context) (ts : Ast.typed_statement)
    =
  let stmt_typed = ts.stmt and smeta = ts.smeta.loc in
  let trans_stmt = trans_stmt udf_names {declc with dconstrain= None} in
  let trans_single_stmt s = trans_stmt s |> List.hd_exn in
  let swrap stmt = [{stmt; smeta}] in
  let mloc = smeta in
  match stmt_typed with
  | Ast.Assignment {assign_lhs; assign_rhs; assign_op} ->
      let rec get_lhs_base = function
        | {Ast.lval= Ast.LIndexed (l, _); _} -> get_lhs_base l
        | {lval= LVariable s; lmeta} -> (s, lmeta)
      in
      let assign_identifier, lmeta = get_lhs_base assign_lhs in
      let id_ad_level = lmeta.Ast.ad_level in
      let id_type_ = lmeta.Ast.type_ in
      let lhs_type_ = assign_lhs.Ast.lmeta.type_ in
      let lhs_ad_level = assign_lhs.Ast.lmeta.ad_level in
      let rec get_lhs_indices = function
        | {Ast.lval= Ast.LIndexed (l, i); _} -> get_lhs_indices l @ i
        | _ -> []
      in
      let assign_indices = get_lhs_indices assign_lhs in
      let assignee =
        { Ast.expr=
            ( match assign_indices with
            | [] -> Ast.Variable assign_identifier
            | _ ->
                Ast.Indexed
                  ( { expr= Ast.Variable assign_identifier
                    ; emeta=
                        { Ast.loc= no_span
                        ; ad_level= id_ad_level
                        ; type_= id_type_ } }
                  , assign_indices ) )
        ; emeta=
            { Ast.loc= assign_lhs.lmeta.loc
            ; ad_level= lhs_ad_level
            ; type_= lhs_type_ } }
      in
      let rhs =
        match assign_op with
        | Ast.Assign | Ast.ArrowAssign -> trans_expr assign_rhs
        | Ast.OperatorAssign op -> op_to_funapp op [assignee; assign_rhs]
      in
      Assignment
        ( ( assign_identifier.name
          , id_type_
          , List.map ~f:trans_idx assign_indices )
        , rhs )
      |> swrap
  | Ast.NRFunApp (fn_kind, {name; _}, args) ->
      NRFunApp (trans_fn_kind fn_kind, name, trans_exprs args) |> swrap
  | Ast.IncrementLogProb e | Ast.TargetPE e -> TargetPE (trans_expr e) |> swrap
  | Ast.Tilde {arg; distribution; args; truncation} ->
      let suffix = dist_name_suffix udf_names distribution.name in
      let kind =
        let possible_names =
          List.map ~f:(( ^ ) distribution.name)
            ("" :: Middle.distribution_suffices)
          |> String.Set.of_list
        in
        if List.exists ~f:(Set.mem possible_names) udf_names then UserDefined
        else StanLib
      in
      let name =
        distribution.name ^ Middle.proportional_to_distribution_infix ^ suffix
      in
      let add_dist =
        TargetPE
          { expr= FunApp (kind, name, trans_exprs (arg :: args))
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
      let body =
        match trans_single_stmt loop_body with
        | {stmt= Block _; _} as b -> b
        | x -> {x with stmt= Block [x]}
      in
      For
        { loopvar= loop_variable.Ast.name
        ; lower= trans_expr lower_bound
        ; upper= trans_expr upper_bound
        ; body }
      |> swrap
  | Ast.ForEach (loopvar, iteratee, body) ->
      let newsym = gensym () in
      let wrap expr = {expr; emeta= {mloc; mtype= UInt; madlevel= DataOnly}} in
      let iteratee' = trans_expr iteratee
      and indexing_var = wrap (Var newsym) in
      let indices =
        let single_one =
          Ast.Single
            { Ast.expr= Ast.IntNumeral "1"
            ; emeta= {iteratee.emeta with type_= UInt} }
        in
        match iteratee'.emeta.mtype with
        | UMatrix -> [single_one; single_one]
        | _ -> [single_one]
      in
      let decl_type =
        Semantic_check.inferred_unsizedtype_of_indexed_exn
          ~loc:iteratee'.emeta.mloc iteratee'.emeta.mtype indices
      in
      let decl_loopvar =
        Decl
          { decl_adtype= iteratee'.emeta.madlevel
          ; decl_id= loopvar.name
          ; decl_type= Unsized decl_type }
      in
      let decl_loopvar = {stmt= decl_loopvar; smeta} in
      let assign_loopvar =
        Assignment
          ( (loopvar.name, UInt, [])
          , Indexed (iteratee', [Single indexing_var]) |> wrap )
      in
      let assign_loopvar = {stmt= assign_loopvar; smeta} in
      let body_stmts =
        match trans_single_stmt body with
        | {stmt= Block body_stmts; _} -> body_stmts
        | b -> [b]
      in
      let body =
        {stmt= Block (decl_loopvar :: assign_loopvar :: body_stmts); smeta}
      in
      For
        { loopvar= newsym
        ; lower= loop_bottom
        ; upper=
            wrap
            @@ FunApp (StanLib, string_of_internal_fn FnLength, [iteratee'])
        ; body }
      |> swrap
  | Ast.FunDef _ ->
      raise_s
        [%message
          "Found function definition statement outside of function block"]
  | Ast.VarDecl
      {decl_type; transformation; identifier; initial_value; is_global} ->
      ignore is_global ;
      trans_decl declc smeta decl_type
        (map_transformation trans_expr transformation)
        identifier initial_value
  | Ast.Block stmts -> Block (List.concat_map ~f:trans_stmt stmts) |> swrap
  | Ast.Return e -> Return (Some (trans_expr e)) |> swrap
  | Ast.ReturnVoid -> Return None |> swrap
  | Ast.Break -> Break |> swrap
  | Ast.Continue -> Continue |> swrap
  | Ast.Skip -> Skip |> swrap

let trans_fun_def udf_names (ts : Ast.typed_statement) =
  match ts.stmt with
  | Ast.FunDef {returntype; funname; arguments; body} ->
      [ { fdrt=
            (match returntype with Void -> None | ReturnType ut -> Some ut)
        ; fdname= funname.name
        ; fdargs= List.map ~f:trans_arg arguments
        ; fdbody=
            trans_stmt udf_names
              {dconstrain= None; dadlevel= AutoDiffable}
              body
            |> unwrap_block_or_skip
        ; fdloc= ts.smeta.loc } ]
  | _ ->
      raise_s
        [%message "Found non-function definition statement in function block"]

let get_block block prog =
  match block with
  | Parameters -> prog.Ast.parametersblock
  | TransformedParameters -> prog.transformedparametersblock
  | GeneratedQuantities -> prog.generatedquantitiesblock

let migrate_checks_to_end_of_block stmts =
  let is_check = contains_fn (string_of_internal_fn FnCheck) in
  let checks, not_checks = List.partition_tf ~f:is_check stmts in
  not_checks @ checks

let trans_prog filename (p : Ast.typed_program) : typed_prog =
  let { Ast.functionblock
      ; datablock
      ; transformeddatablock
      ; parametersblock
      ; transformedparametersblock
      ; modelblock; _ } =
    p
  in
  let map f list_op = Option.value ~default:[] list_op |> List.concat_map ~f in
  let grab_fundef_names = function
    | {Ast.stmt= Ast.FunDef {funname; _}; _} -> [funname.name]
    | _ -> []
  in
  let udf_names = map grab_fundef_names functionblock in
  let trans_stmt = trans_stmt udf_names in
  let get_name_size s =
    match s.Ast.stmt with
    | Ast.VarDecl {decl_type= Sized st; identifier; transformation; _} ->
        [(identifier.name, trans_sizedtype st, transformation)]
    | _ -> []
  in
  let grab_names_sizes block =
    List.map ~f:get_name_size (Option.value ~default:[] (get_block block p))
    |> List.concat_map
         ~f:
           (List.map ~f:(fun (n, s, t) ->
                ( n
                , { out_constrained_st= s
                  ; out_unconstrained_st= param_size t s
                  ; out_block= block
                  ; out_trans= map_transformation trans_expr t } ) ))
  in
  let output_vars =
    grab_names_sizes Parameters
    @ grab_names_sizes TransformedParameters
    @ grab_names_sizes GeneratedQuantities
  and input_vars =
    map get_name_size datablock |> List.map ~f:(fun (n, st, _) -> (n, st))
  in
  let declc = {dconstrain= None; dadlevel= DataOnly} in
  let datab =
    map (trans_stmt {declc with dconstrain= Some Check}) datablock
    |> migrate_checks_to_end_of_block
  in
  let prepare_data =
    datab
    @ map (trans_stmt {declc with dconstrain= Some Check}) transformeddatablock
    |> migrate_checks_to_end_of_block
  in
  let modelb =
    map (trans_stmt {declc with dadlevel= AutoDiffable}) modelblock
  in
  let log_prob =
    map
      (trans_stmt {dconstrain= Some Constrain; dadlevel= AutoDiffable})
      parametersblock
    @ ( map
          (trans_stmt {dconstrain= Some Check; dadlevel= AutoDiffable})
          transformedparametersblock
      |> migrate_checks_to_end_of_block )
    @
    match modelb with
    | [] -> []
    | hd :: _ -> [{stmt= Block modelb; smeta= hd.smeta}]
  in
  let gen_from_block declc block =
    map (trans_stmt declc) (get_block block p)
  in
  let txparam_decls, txparam_stmts =
    gen_from_block declc TransformedParameters
    |> List.partition_tf ~f:(function {stmt= Decl _; _} -> true | _ -> false)
  in
  let compiler_if_return cond =
    { stmt= IfElse (cond, {stmt= Return None; smeta= no_span}, None)
    ; smeta= no_span }
  in
  let iexpr expr = {expr; emeta= internal_meta} in
  let fnot e = FunApp (StanLib, string_of_operator PNot, [e]) |> iexpr in
  let tparam_early_return =
    let to_var fv = iexpr (Var (string_of_flag_var fv)) in
    let v1 = to_var EmitTransformedParameters in
    let v2 = to_var EmitGeneratedQuantities in
    [compiler_if_return (fnot (EOr (v1, v2) |> iexpr))]
  in
  let gq_stmts =
    migrate_checks_to_end_of_block
      (gen_from_block {declc with dconstrain= Some Check} GeneratedQuantities)
  in
  let gq_early_return =
    [ compiler_if_return
        (fnot (Var (string_of_flag_var EmitGeneratedQuantities) |> iexpr)) ]
  in
  let generate_quantities =
    gen_from_block {declc with dconstrain= Some Constrain} Parameters
    @ txparam_decls @ tparam_early_return @ txparam_stmts @ gq_early_return
    @ gq_stmts
  in
  let transform_inits =
    gen_from_block {declc with dconstrain= Some Unconstrain} Parameters
  in
  { functions_block= map (trans_fun_def udf_names) functionblock
  ; input_vars
  ; prepare_data
  ; log_prob
  ; generate_quantities
  ; transform_inits
  ; output_vars
  ; prog_name= !Semantic_check.model_name
  ; prog_path= filename }
