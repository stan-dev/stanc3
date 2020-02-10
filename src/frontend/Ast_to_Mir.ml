open Core_kernel
open Middle

(* XXX fix exn *)
let unwrap_return_exn = function
  | Some (UnsizedType.ReturnType ut) -> ut
  | x ->
      raise_s
        [%message
          "Unexpected return type " (x : UnsizedType.returntype option)]

let trans_fn_kind = function
  | Ast.StanLib -> Fun_kind.StanLib
  | UserDefined -> UserDefined

let without_underscores = String.filter ~f:(( <> ) '_')

let drop_leading_zeros s =
  match String.lfindi ~f:(fun _ c -> c <> '0') s with
  | Some p when p > 0 -> (
    match s.[p] with
    | 'e' | '.' -> String.drop_prefix s (p - 1)
    | _ -> String.drop_prefix s p )
  | Some _ -> s
  | None -> "0"

let format_number s = s |> without_underscores |> drop_leading_zeros

let%expect_test "format_number0" =
  format_number "0_000." |> print_endline ;
  [%expect "0."]

let%expect_test "format_number1" =
  format_number ".123_456" |> print_endline ;
  [%expect ".123456"]

let rec op_to_funapp op args =
  let argtypes =
    List.map ~f:(fun x -> (x.Ast.emeta.Ast.ad_level, x.emeta.type_)) args
  in
  let type_ =
    Stan_math_signatures.operator_stan_math_return_type op argtypes
    |> unwrap_return_exn
  and loc = Ast.expr_loc_lub args
  and adlevel = Ast.expr_ad_lub args in
  Expr.
    { Fixed.pattern= FunApp (StanLib, Operator.to_string op, trans_exprs args)
    ; meta= Expr.Typed.Meta.create ~type_ ~adlevel ~loc () }

and trans_expr {Ast.expr; Ast.emeta} =
  let type_ = emeta.Ast.type_
  and loc = emeta.loc
  and adlevel = emeta.ad_level in
  match expr with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, And, rhs) ->
      Expr.
        { Fixed.pattern= EAnd (trans_expr lhs, trans_expr rhs)
        ; meta= Typed.Meta.create ~type_ ~adlevel ~loc () }
  | BinOp (lhs, Or, rhs) ->
      Expr.
        { Fixed.pattern= EOr (trans_expr lhs, trans_expr rhs)
        ; meta= Typed.Meta.create ~type_ ~adlevel ~loc () }
  | BinOp (lhs, op, rhs) -> op_to_funapp op [lhs; rhs]
  | PrefixOp (op, e) | Ast.PostfixOp (e, op) -> op_to_funapp op [e]
  | _ ->
      let pattern =
        match expr with
        | Ast.TernaryIf (cond, ifb, elseb) ->
            Expr.Fixed.Pattern.TernaryIf
              (trans_expr cond, trans_expr ifb, trans_expr elseb)
        | Variable {name; _} -> Var name
        | IntNumeral x -> Lit (Int, format_number x)
        | RealNumeral x -> Lit (Real, format_number x)
        | FunApp (fn_kind, {name; _}, args)
         |CondDistApp (fn_kind, {name; _}, args) ->
            FunApp (trans_fn_kind fn_kind, name, trans_exprs args)
        | GetLP | GetTarget -> FunApp (StanLib, "target", [])
        | ArrayExpr eles ->
            FunApp
              ( CompilerInternal
              , Internal_fun.to_string FnMakeArray
              , trans_exprs eles )
        | RowVectorExpr eles ->
            FunApp
              ( CompilerInternal
              , Internal_fun.to_string FnMakeRowVec
              , trans_exprs eles )
        | Indexed (lhs, indices) ->
            Indexed (trans_expr lhs, List.map ~f:trans_idx indices)
        | Paren _ | BinOp _ | PrefixOp _ | PostfixOp _ ->
            raise_s [%message "Impossible!"]
      and meta = Expr.Typed.Meta.create ~type_ ~adlevel ~loc () in
      {meta; pattern}

and trans_idx = function
  | Ast.All -> All
  | Ast.Upfrom e -> Upfrom (trans_expr e)
  | Ast.Downfrom e -> Between (Expr.Helpers.loop_bottom, trans_expr e)
  | Ast.Between (lb, ub) -> Between (trans_expr lb, trans_expr ub)
  | Ast.Single e -> (
    match e.emeta.type_ with
    | UInt -> Single (trans_expr e)
    | UArray _ -> MultiIndex (trans_expr e)
    | _ ->
        raise_s
          [%message "Expecting int or array" (e.emeta.type_ : UnsizedType.t)] )

and trans_exprs exprs = List.map ~f:trans_expr exprs

let trans_sizedtype = SizedType.map trans_expr
let trans_possiblysizedtype pst = Type.map trans_expr pst

let neg_inf =
  Expr.
    { Fixed.pattern= FunApp (StanLib, Internal_fun.to_string FnNegInf, [])
    ; meta=
        Typed.Meta.{type_= UReal; loc= Location_span.empty; adlevel= DataOnly}
    }

let trans_arg (adtype, ut, ident) = (adtype, ident.Ast.name, ut)

let truncate_dist ud_dists (id : Ast.identifier) ast_obs ast_args t =
  let cdf_suffices = ["_lcdf"; "_cdf_log"] in
  let ccdf_suffices = ["_lccdf"; "_ccdf_log"] in
  let find_function_info sfx =
    let possible_names =
      List.map ~f:(( ^ ) id.name) sfx |> String.Set.of_list
    in
    match List.find ~f:(fun (n, _) -> Set.mem possible_names n) ud_dists with
    | Some (name, tp) -> (Ast.UserDefined, name, tp)
    | None ->
        ( Ast.StanLib
        , Set.to_list possible_names |> List.hd_exn
        , if Stan_math_signatures.is_stan_math_function_name (id.name ^ "_lpmf")
          then UnsizedType.UInt
          else UnsizedType.UReal (* close enough *) )
  in
  let trunc cond_op (x : Ast.typed_expression) y =
    let smeta = x.Ast.emeta.loc in
    { Stmt.Fixed.meta= smeta
    ; pattern=
        IfElse
          ( op_to_funapp cond_op [ast_obs; x]
          , {Stmt.Fixed.meta= smeta; pattern= TargetPE neg_inf}
          , Some y ) }
  in
  let targetme loc e =
    {Stmt.Fixed.meta= loc; pattern= TargetPE (op_to_funapp Operator.PMinus [e])}
  in
  let funapp meta kind name args =
    { Ast.emeta= meta
    ; expr= Ast.FunApp (kind, {name; id_loc= Location_span.empty}, args) }
  in
  let inclusive_bound tp (lb : Ast.typed_expression) =
    let emeta = lb.emeta in
    if UnsizedType.is_int_type tp then
      Ast.
        { emeta
        ; expr= BinOp (lb, Operator.Minus, {emeta; expr= Ast.IntNumeral "1"})
        }
    else lb
  in
  match t with
  | Ast.NoTruncate -> []
  | TruncateUpFrom lb ->
      let fk, fn, tp = find_function_info ccdf_suffices in
      [ trunc Less lb
          (targetme lb.emeta.loc
             (funapp lb.emeta fk fn (inclusive_bound tp lb :: ast_args))) ]
  | TruncateDownFrom ub ->
      let fk, fn, _ = find_function_info cdf_suffices in
      [ trunc Greater ub
          (targetme ub.emeta.loc (funapp ub.emeta fk fn (ub :: ast_args))) ]
  | TruncateBetween (lb, ub) ->
      let fk, fn, tp = find_function_info cdf_suffices in
      [ trunc Less lb
          (trunc Greater ub
             (targetme ub.emeta.loc
                (funapp ub.emeta Ast.StanLib "log_diff_exp"
                   [ funapp ub.emeta fk fn (ub :: ast_args)
                   ; funapp ub.emeta fk fn (inclusive_bound tp lb :: ast_args)
                   ]))) ]

let unquote s =
  if s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.drop_suffix (String.drop_prefix s 1) 1
  else s

(* hack(sean): strings aren't real
   XXX add UString to MIR and maybe AST.
*)
let mkstring loc s =
  Expr.
    { Fixed.pattern= Lit (Str, s)
    ; meta= Typed.Meta.create ~type_:UReal ~loc ~adlevel:DataOnly () }

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
  Internal_fun.to_string
    ( match c with
    | Check -> FnCheck
    | Constrain -> FnConstrain
    | Unconstrain -> FnUnconstrain )

type decl_context =
  {dconstrain: constrainaction option; dadlevel: UnsizedType.autodifftype}

let check_constraint_to_string t (c : constrainaction) =
  match t with
  | Program.Ordered -> "ordered"
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
  | Program.CholeskyCorr -> "cholesky_corr"
  | _ -> check_constraint_to_string t c

let constraint_forl = function
  | Program.Identity | Offset _ | Multiplier _ | OffsetMultiplier _ | Lower _
   |Upper _ | LowerUpper _ ->
      Stmt.Helpers.for_scalar
  | Ordered | PositiveOrdered | Simplex | UnitVector | CholeskyCorr
   |CholeskyCov | Correlation | Covariance ->
      Stmt.Helpers.for_eigen

let extract_transform_args = function
  | Program.Lower a | Upper a -> [a]
  | Offset a -> [{a with Expr.Fixed.pattern= Lit (Int, "0")}; a]
  | Multiplier a -> [a; {a with pattern= Lit (Int, "1")}]
  | LowerUpper (a1, a2) | OffsetMultiplier (a1, a2) -> [a1; a2]
  | Covariance | Correlation | CholeskyCov | CholeskyCorr | Ordered
   |PositiveOrdered | Simplex | UnitVector | Identity ->
      []

let extra_constraint_args st = function
  | Program.Lower _ | Upper _ | Offset _ | Multiplier _ | LowerUpper _
   |OffsetMultiplier _ | Ordered | PositiveOrdered | Simplex | UnitVector
   |Identity ->
      []
  | Covariance | Correlation | CholeskyCorr ->
      [List.hd_exn (SizedType.dims_of st)]
  | CholeskyCov -> SizedType.dims_of st

let param_size transform sizedtype =
  let rec shrink_eigen f st =
    match st with
    | SizedType.SArray (t, d) -> SizedType.SArray (shrink_eigen f t, d)
    | SVector d | SMatrix (d, _) -> SVector (f d)
    | SInt | SReal | SRowVector _ ->
        raise_s
          [%message
            "Expecting SVector or SMatrix, got " (st : Expr.Typed.t SizedType.t)]
  in
  let rec shrink_eigen_mat f st =
    match st with
    | SizedType.SArray (t, d) -> SizedType.SArray (shrink_eigen_mat f t, d)
    | SMatrix (d1, d2) -> SVector (f d1 d2)
    | SInt | SReal | SRowVector _ | SVector _ ->
        raise_s
          [%message "Expecting SMatrix, got " (st : Expr.Typed.t SizedType.t)]
  in
  let k_choose_2 k =
    Expr.Helpers.(binop (binop k Times (binop k Minus (int 1))) Divide (int 2))
  in
  match transform with
  | Program.Identity | Lower _ | Upper _
   |LowerUpper (_, _)
   |Offset _ | Multiplier _
   |OffsetMultiplier (_, _)
   |Ordered | PositiveOrdered | UnitVector ->
      sizedtype
  | Simplex ->
      shrink_eigen (fun d -> Expr.Helpers.(binop d Minus (int 1))) sizedtype
  | CholeskyCorr | Correlation -> shrink_eigen k_choose_2 sizedtype
  | CholeskyCov ->
      (* (N * (N + 1)) / 2 + (M - N) * N *)
      shrink_eigen_mat
        (fun m n ->
          Expr.Helpers.(
            binop
              (binop (k_choose_2 n) Plus n)
              Plus
              (binop (binop m Minus n) Times n)) )
        sizedtype
  | Covariance ->
      shrink_eigen
        (fun k -> Expr.Helpers.(binop k Plus (k_choose_2 k)))
        sizedtype

let remove_possibly_exn pst action loc =
  match pst with
  | Type.Sized st -> st
  | Unsized _ ->
      raise_s
        [%message
          "Error extracting sizedtype" ~action ~loc:(loc : Location_span.t)]

let constrain_decl decl_type dconstrain t decl_id decl_var smeta =
  let st = remove_possibly_exn decl_type "constrain" smeta in
  let mkstring = mkstring (Expr.Typed.loc_of decl_var) in
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
        { var with
          Expr.Fixed.pattern= FunApp (CompilerInternal, fname, args var) }
      in
      [ (constraint_forl t) st
          (Stmt.Helpers.assign_indexed (SizedType.to_unsized st) decl_id smeta
             constrainvar)
          decl_var smeta ]

let rec check_decl decl_type' decl_id decl_trans smeta adlevel =
  let decl_type = remove_possibly_exn decl_type' "check" smeta in
  let chk fn args =
    let check_id id =
      let id_str =
        Expr.
          { Fixed.pattern= Lit (Str, Fmt.strf "%a" Typed.pp id)
          ; meta= Typed.Meta.empty }
      in
      let fname = Internal_fun.to_string FnCheck in
      let pattern =
        Stmt.Fixed.Pattern.NRFunApp
          (CompilerInternal, fname, fn :: id_str :: id :: args)
      in
      Stmt.Fixed.{meta= smeta; pattern}
    in
    let mtype = SizedType.to_unsized decl_type in
    Stmt.Helpers.for_eigen decl_type check_id
      Expr.
        { Fixed.pattern= Var decl_id
        ; meta= Typed.Meta.create ~type_:mtype ~loc:smeta ~adlevel () }
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
    Expr.
      { Fixed.pattern= Var decl_id
      ; meta=
          Typed.Meta.create ~adlevel:dadlevel ~loc:smeta
            ~type_:(Type.to_unsized decl_type)
            () }
  in
  let decl =
    Stmt.
      {Fixed.pattern= Decl {decl_adtype; decl_id; decl_type= dt}; meta= smeta}
  in
  let rhs_assignment =
    Option.map
      ~f:(fun e ->
        Stmt.Fixed.
          {pattern= Assignment ((decl_id, e.meta.type_, []), e); meta= smeta}
        )
      rhs
    |> Option.to_list
  in
  if Utils.is_user_ident decl_id then
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
  | [({Stmt.Fixed.pattern= Block _; _} as b)] | [({pattern= Skip; _} as b)] ->
      b
  | x ->
      raise_s
        [%message "Expecting a block or skip, not" (x : Stmt.Located.t list)]

let dist_name_suffix udf_names name =
  let is_udf_name s = List.exists ~f:(fun (n, _) -> n = s) udf_names in
  match
    Middle.Utils.distribution_suffices
    |> List.filter ~f:(fun sfx ->
           Stan_math_signatures.is_stan_math_function_name (name ^ sfx)
           || is_udf_name (name ^ sfx) )
    |> List.hd
  with
  | Some hd -> hd
  | None -> raise_s [%message "Couldn't find distribution " name]

let%expect_test "dist name suffix" =
  dist_name_suffix [] "normal" |> print_endline ;
  [%expect {| _log |}]

let rec trans_stmt ud_dists (declc : decl_context) (ts : Ast.typed_statement) =
  let stmt_typed = ts.stmt and smeta = ts.smeta.loc in
  let trans_stmt = trans_stmt ud_dists {declc with dconstrain= None} in
  let trans_single_stmt s =
    match trans_stmt s with
    | [s] -> s
    | s -> Stmt.Fixed.{pattern= SList s; meta= smeta}
  in
  let swrap pattern = [Stmt.Fixed.{meta= smeta; pattern}] in
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
                        { Ast.loc= Location_span.empty
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
        ( ( assign_identifier.Ast.name
          , id_type_
          , List.map ~f:trans_idx assign_indices )
        , rhs )
      |> swrap
  | Ast.NRFunApp (fn_kind, {name; _}, args) ->
      NRFunApp (trans_fn_kind fn_kind, name, trans_exprs args) |> swrap
  | Ast.IncrementLogProb e | Ast.TargetPE e -> TargetPE (trans_expr e) |> swrap
  | Ast.Tilde {arg; distribution; args; truncation} ->
      let suffix = dist_name_suffix ud_dists distribution.name in
      let kind =
        let possible_names =
          List.map ~f:(( ^ ) distribution.name)
            ("" :: Utils.distribution_suffices)
          |> String.Set.of_list
        in
        if List.exists ~f:(fun (n, _) -> Set.mem possible_names n) ud_dists
        then Fun_kind.UserDefined
        else StanLib
      in
      let name =
        distribution.name ^ Utils.proportional_to_distribution_infix ^ suffix
      in
      let add_dist =
        Stmt.Fixed.Pattern.TargetPE
          Expr.
            { Fixed.pattern= FunApp (kind, name, trans_exprs (arg :: args))
            ; meta=
                Typed.Meta.create ~type_:UReal ~loc:mloc
                  ~adlevel:(Ast.expr_ad_lub (arg :: args))
                  () }
      in
      truncate_dist ud_dists distribution arg args truncation @ swrap add_dist
  | Ast.Print ps ->
      NRFunApp
        ( CompilerInternal
        , Internal_fun.to_string FnPrint
        , trans_printables smeta ps )
      |> swrap
  | Ast.Reject ps ->
      NRFunApp
        ( CompilerInternal
        , Internal_fun.to_string FnReject
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
        | {pattern= Block _; _} as b -> b
        | x -> {x with pattern= Block [x]}
      in
      For
        { loopvar= loop_variable.Ast.name
        ; lower= trans_expr lower_bound
        ; upper= trans_expr upper_bound
        ; body }
      |> swrap
  | Ast.ForEach (loopvar, iteratee, body) ->
      let iteratee' = trans_expr iteratee in
      let body_stmts =
        match trans_single_stmt body with
        | {pattern= Block body_stmts; _} -> body_stmts
        | b -> [b]
      in
      let decl_type =
        match Expr.Typed.type_of iteratee' with
        | UMatrix -> UnsizedType.UReal
        | t ->
            Expr.Helpers.(infer_type_of_indexed t [Index.Single loop_bottom])
      in
      let decl_loopvar =
        Stmt.Fixed.
          { meta= smeta
          ; pattern=
              Decl
                { decl_adtype= Expr.Typed.adlevel_of iteratee'
                ; decl_id= loopvar.name
                ; decl_type= Unsized decl_type } }
      in
      let assignment var =
        Stmt.Fixed.
          { pattern= Assignment ((loopvar.name, decl_type, []), var)
          ; meta= smeta }
      in
      let bodyfn var =
        Stmt.Fixed.
          { pattern= Block (decl_loopvar :: assignment var :: body_stmts)
          ; meta= smeta }
      in
      [Stmt.Helpers.for_each bodyfn iteratee' smeta]
  | Ast.FunDef _ ->
      raise_s
        [%message
          "Found function definition statement outside of function block"]
  | Ast.VarDecl
      {decl_type; transformation; identifier; initial_value; is_global} ->
      ignore is_global ;
      trans_decl declc smeta decl_type
        (Program.map_transformation trans_expr transformation)
        identifier initial_value
  | Ast.Block stmts -> Block (List.concat_map ~f:trans_stmt stmts) |> swrap
  | Ast.Return e -> Return (Some (trans_expr e)) |> swrap
  | Ast.ReturnVoid -> Return None |> swrap
  | Ast.Break -> Break |> swrap
  | Ast.Continue -> Continue |> swrap
  | Ast.Skip -> Skip |> swrap

let trans_fun_def ud_dists (ts : Ast.typed_statement) =
  match ts.stmt with
  | Ast.FunDef {returntype; funname; arguments; body} ->
      [ Program.
          { fdrt=
              (match returntype with Void -> None | ReturnType ut -> Some ut)
          ; fdname= funname.name
          ; fdargs= List.map ~f:trans_arg arguments
          ; fdbody=
              trans_stmt ud_dists
                {dconstrain= None; dadlevel= AutoDiffable}
                body
              |> unwrap_block_or_skip
          ; fdloc= ts.smeta.loc } ]
  | _ ->
      raise_s
        [%message "Found non-function definition statement in function block"]

let get_block block prog =
  match block with
  | Program.Parameters -> prog.Ast.parametersblock
  | TransformedParameters -> prog.transformedparametersblock
  | GeneratedQuantities -> prog.generatedquantitiesblock

let migrate_checks_to_end_of_block stmts =
  let is_check = Stmt.Helpers.contains_fn FnCheck in
  let checks, not_checks = List.partition_tf ~f:is_check stmts in
  not_checks @ checks

let trans_prog filename (p : Ast.typed_program) : Program.Typed.t =
  let { Ast.functionblock
      ; datablock
      ; transformeddatablock
      ; parametersblock
      ; transformedparametersblock
      ; modelblock; _ } =
    p
  in
  let map f list_op = Option.value ~default:[] list_op |> List.concat_map ~f in
  let grab_fundef_names_and_types = function
    | {Ast.stmt= Ast.FunDef {funname; arguments= (_, type_, _) :: _; _}; _} ->
        [(funname.name, type_)]
    | _ -> []
  in
  let ud_dists = map grab_fundef_names_and_types functionblock in
  let trans_stmt = trans_stmt ud_dists in
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
                , Program.
                    { out_constrained_st= s
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
    | hd :: _ -> [{pattern= Block modelb; meta= hd.meta}]
  in
  let gen_from_block declc block =
    map (trans_stmt declc) (get_block block p)
  in
  let txparam_decls, txparam_stmts =
    gen_from_block declc TransformedParameters
    |> List.partition_tf ~f:(function
         | {pattern= Decl _; _} -> true
         | _ -> false )
  in
  let compiler_if_return cond =
    Stmt.Fixed.
      { pattern=
          IfElse (cond, {pattern= Return None; meta= Location_span.empty}, None)
      ; meta= Location_span.empty }
  in
  let iexpr pattern = Expr.{pattern; Fixed.meta= Typed.Meta.empty} in
  let fnot e = FunApp (StanLib, Operator.to_string PNot, [e]) |> iexpr in
  let tparam_early_return =
    let to_var fv = iexpr (Var (Flag_vars.to_string fv)) in
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
        (fnot (Var (Flag_vars.to_string EmitGeneratedQuantities) |> iexpr)) ]
  in
  let generate_quantities =
    gen_from_block {declc with dconstrain= Some Constrain} Parameters
    @ txparam_decls @ tparam_early_return @ txparam_stmts @ gq_early_return
    @ gq_stmts
  in
  let transform_inits =
    gen_from_block {declc with dconstrain= Some Unconstrain} Parameters
  in
  { functions_block= map (trans_fun_def ud_dists) functionblock
  ; input_vars
  ; prepare_data
  ; log_prob
  ; generate_quantities
  ; transform_inits
  ; output_vars
  ; prog_name= !Semantic_check.model_name
  ; prog_path= filename }
