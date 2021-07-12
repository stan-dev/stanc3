open Core_kernel
open Middle

(* XXX fix exn *)
let unwrap_return_exn = function
  | Some (UnsizedType.ReturnType ut) -> ut
  | x ->
      raise_s
        [%message
          "Unexpected return type " (x : UnsizedType.returntype option)]

let trans_fn_kind kind name =
  let fname = Utils.stdlib_distribution_name name in
  match kind with
  | Ast.StanLib suffix -> Fun_kind.StanLib (fname, suffix, Common.Helpers.AoS)
  | UserDefined suffix -> UserDefined (fname, suffix)

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
    { Fixed.pattern=
        FunApp
          ( StanLib (Operator.to_string op, FnPlain, Common.Helpers.AoS)
          , trans_exprs args )
    ; meta= Expr.Typed.Meta.create ~type_ ~adlevel ~loc () }

and trans_expr {Ast.expr; Ast.emeta} =
  let ewrap pattern =
    Expr.
      { Fixed.pattern
      ; meta=
          Typed.Meta.
            {type_= emeta.Ast.type_; adlevel= emeta.ad_level; loc= emeta.loc}
      }
  in
  match expr with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, And, rhs) -> EAnd (trans_expr lhs, trans_expr rhs) |> ewrap
  | BinOp (lhs, Or, rhs) -> EOr (trans_expr lhs, trans_expr rhs) |> ewrap
  | BinOp (lhs, op, rhs) -> op_to_funapp op [lhs; rhs]
  | PrefixOp (op, e) | Ast.PostfixOp (e, op) -> op_to_funapp op [e]
  | Ast.TernaryIf (cond, ifb, elseb) ->
      Expr.Fixed.Pattern.TernaryIf
        (trans_expr cond, trans_expr ifb, trans_expr elseb)
      |> ewrap
  | Variable {name; _} -> Var name |> ewrap
  | IntNumeral x -> Lit (Int, format_number x) |> ewrap
  | RealNumeral x -> Lit (Real, format_number x) |> ewrap
  | FunApp (fn_kind, {name; _}, args) | CondDistApp (fn_kind, {name; _}, args)
    ->
      FunApp (trans_fn_kind fn_kind name, trans_exprs args) |> ewrap
  | GetLP | GetTarget ->
      FunApp (StanLib ("target", FnTarget, Common.Helpers.AoS), []) |> ewrap
  | ArrayExpr eles ->
      FunApp (CompilerInternal FnMakeArray, trans_exprs eles) |> ewrap
  | RowVectorExpr eles ->
      FunApp (CompilerInternal FnMakeRowVec, trans_exprs eles) |> ewrap
  | Indexed (lhs, indices) ->
      Indexed (trans_expr lhs, List.map ~f:trans_idx indices) |> ewrap

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

let neg_inf =
  Expr.
    { Fixed.pattern= FunApp (CompilerInternal FnNegInf, [])
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
    | Some (name, tp) -> (Ast.UserDefined FnPlain, name, tp)
    | None ->
        ( Ast.StanLib FnPlain
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
                (funapp ub.emeta (Ast.StanLib FnPlain) "log_diff_exp"
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

type decl_context =
  {dconstrain: constrainaction option; dadlevel: UnsizedType.autodifftype}

let check_constraint_to_string t (c : constrainaction) =
  match t with
  | Program.Ordered -> Some "ordered"
  | PositiveOrdered -> Some "positive_ordered"
  | Simplex -> Some "simplex"
  | UnitVector -> Some "unit_vector"
  | CholeskyCorr -> Some "cholesky_factor_corr"
  | CholeskyCov -> Some "cholesky_factor"
  | Correlation -> Some "corr_matrix"
  | Covariance -> Some "cov_matrix"
  | Lower _ -> (
    match c with
    | Check -> Some "greater_or_equal"
    | Constrain | Unconstrain -> Some "lb" )
  | Upper _ -> (
    match c with
    | Check -> Some "less_or_equal"
    | Constrain | Unconstrain -> Some "ub" )
  | LowerUpper _ -> (
    match c with
    | Check ->
        raise_s
          [%message "LowerUpper is really two other checks tied together"]
    | Constrain | Unconstrain -> Some "lub" )
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> (
    match c with
    | Check -> None
    | Constrain | Unconstrain -> Some "offset_multiplier" )
  | Identity -> None

let constrain_constraint_to_string t (c : constrainaction) =
  match t with
  | Program.CholeskyCorr -> Some "cholesky_corr"
  | _ -> check_constraint_to_string t c

let constraint_forl = function
  | Program.Identity | Offset _ | Multiplier _ | OffsetMultiplier _ | Lower _
   |Upper _ | LowerUpper _ ->
      Stmt.Helpers.for_scalar
  | Ordered | PositiveOrdered | Simplex | UnitVector | CholeskyCorr
   |CholeskyCov | Correlation | Covariance ->
      Stmt.Helpers.for_eigen

let same_shape decl_id decl_var id var meta =
  if UnsizedType.is_scalar_type (Expr.Typed.type_of var) then []
  else
    [ Stmt.
        { Fixed.pattern=
            NRFunApp
              ( StanLib ("check_matching_dims", FnPlain, Common.Helpers.AoS)
              , Expr.Helpers.
                  [str "constraint"; str decl_id; decl_var; str id; var] )
        ; meta } ]

let check_transform_shape decl_id decl_var meta = function
  | Program.Offset e -> same_shape decl_id decl_var "offset" e meta
  | Multiplier e -> same_shape decl_id decl_var "multiplier" e meta
  | Lower e -> same_shape decl_id decl_var "lower" e meta
  | Upper e -> same_shape decl_id decl_var "upper" e meta
  | OffsetMultiplier (e1, e2) ->
      same_shape decl_id decl_var "offset" e1 meta
      @ same_shape decl_id decl_var "multiplier" e2 meta
  | LowerUpper (e1, e2) ->
      same_shape decl_id decl_var "lower" e1 meta
      @ same_shape decl_id decl_var "upper" e2 meta
  | Covariance | Correlation | CholeskyCov | CholeskyCorr | Ordered
   |PositiveOrdered | Simplex | UnitVector | Identity ->
      []

let copy_indices indexed (var : Expr.Typed.t) =
  if UnsizedType.is_scalar_type var.meta.type_ then var
  else
    match Expr.Helpers.collect_indices indexed with
    | [] -> var
    | indices ->
        Expr.Fixed.
          { pattern= Indexed (var, indices)
          ; meta=
              { var.meta with
                type_=
                  Expr.Helpers.infer_type_of_indexed var.meta.type_ indices }
          }

let extract_transform_args var = function
  | Program.Lower a | Upper a -> [copy_indices var a]
  | Offset a ->
      [copy_indices var a; {a with Expr.Fixed.pattern= Lit (Int, "1")}]
  | Multiplier a -> [{a with pattern= Lit (Int, "0")}; copy_indices var a]
  | LowerUpper (a1, a2) | OffsetMultiplier (a1, a2) ->
      [copy_indices var a1; copy_indices var a2]
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
    | SizedType.SArray (t, dim) -> SizedType.SArray (shrink_eigen f t, dim)
    | SVector (mem_type, row_expr) | SMatrix (mem_type, row_expr, _) ->
        SVector (mem_type, f row_expr)
    | SInt | SReal | SRowVector _ ->
        raise_s
          [%message
            "Expecting SVector or SMatrix, got " (st : Expr.Typed.t SizedType.t)]
  in
  let rec shrink_eigen_mat f st =
    match st with
    | SizedType.SArray (t, dim) -> SizedType.SArray (shrink_eigen_mat f t, dim)
    | SMatrix (mem_type, dim1, dim2) -> SVector (mem_type, f dim1 dim2)
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

let constrain_decl st dconstrain t decl_id decl_var smeta =
  match Option.bind ~f:(constrain_constraint_to_string t) dconstrain with
  | None -> []
  | Some constraint_str ->
      let dc = Option.value_exn dconstrain in
      let extra_args =
        match dconstrain with
        | Some Constrain -> extra_constraint_args st t
        | _ -> []
      in
      let args var = (var :: extract_transform_args var t) @ extra_args in
      let internal_fun =
        match dc with
        | Check -> Internal_fun.FnCheck constraint_str
        | Constrain -> FnConstrain constraint_str
        | Unconstrain -> FnUnconstrain constraint_str
      in
      let constrainvar var =
        { var with
          Expr.Fixed.pattern= FunApp (CompilerInternal internal_fun, args var)
        }
      in
      let unconstrained_decls, decl_id, ut =
        let ut = SizedType.to_unsized (param_size t st) in
        match dconstrain with
        | Some Unconstrain when t <> Identity ->
            ( [ Stmt.Fixed.
                  { pattern=
                      Decl
                        { decl_adtype= DataOnly
                        ; decl_id= decl_id ^ "_free__"
                        ; decl_type= Sized (param_size t st) }
                  ; meta= smeta } ]
            , decl_id ^ "_free__"
            , ut )
        | _ -> ([], decl_id, SizedType.to_unsized st)
      in
      unconstrained_decls
      @ [ (constraint_forl t) st
            (Stmt.Helpers.assign_indexed ut decl_id smeta constrainvar)
            decl_var smeta ]

let rec check_decl var decl_type' decl_id decl_trans smeta adlevel =
  let decl_type = remove_possibly_exn decl_type' "check" smeta in
  match decl_trans with
  | Program.Identity | Offset _ | Multiplier _ | OffsetMultiplier (_, _) -> []
  | LowerUpper (lb, ub) ->
      check_decl var decl_type' decl_id (Lower lb) smeta adlevel
      @ check_decl var decl_type' decl_id (Upper ub) smeta adlevel
  | _ -> (
    match check_constraint_to_string decl_trans Check with
    | Some fn ->
        let check_id id =
          let id_str = Expr.Helpers.str (Fmt.strf "%a" Expr.Typed.pp id) in
          let args = extract_transform_args id decl_trans in
          Stmt.Helpers.internal_nrfunapp (FnCheck fn) (id_str :: id :: args)
            smeta
        in
        [(constraint_forl decl_trans) decl_type check_id var smeta]
    | None -> [] )

let check_sizedtype name =
  let check x = function
    | {Expr.Fixed.pattern= Lit (Int, i); _} when float_of_string i >= 0. -> []
    | n ->
        [ Stmt.Helpers.internal_nrfunapp FnValidateSize
            Expr.Helpers.
              [str name; str (Fmt.strf "%a" Pretty_printing.pp_expression x); n]
            n.meta.loc ]
  in
  let rec sizedtype = function
    | SizedType.(SInt | SReal) as t -> ([], t)
    | SVector (mem_type, size_expr) ->
        let e = trans_expr size_expr in
        (check size_expr e, SizedType.SVector (mem_type, e))
    | SRowVector (mem_type, size_expr) ->
        let e = trans_expr size_expr in
        (check size_expr e, SizedType.SRowVector (mem_type, e))
    | SMatrix (mem_type, row, col) ->
        let expr_row = trans_expr row in
        let expr_col = trans_expr col in
        ( check row expr_row @ check col expr_col
        , SizedType.SMatrix (mem_type, expr_row, expr_col) )
    | SArray (t, s) ->
        let e = trans_expr s in
        let ll, t = sizedtype t in
        (check s e @ ll, SizedType.SArray (t, e))
  in
  function
  | Type.Sized st ->
      let ll, st = sizedtype st in
      (ll, Type.Sized st)
  | Unsized ut -> ([], Unsized ut)

let trans_decl {dconstrain; dadlevel} smeta decl_type transform identifier
    initial_value =
  let decl_id = identifier.Ast.name in
  let rhs = Option.map ~f:trans_expr initial_value in
  let size_checks, dt = check_sizedtype identifier.name decl_type in
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
    let constrain_checks =
      match dconstrain with
      | Some Constrain | Some Unconstrain ->
          raise_s [%message "This should never happen."]
      | Some Check ->
          check_transform_shape decl_id decl_var smeta transform
          @ check_decl decl_var dt decl_id transform smeta dadlevel
      | None -> []
    in
    size_checks @ (decl :: rhs_assignment) @ constrain_checks
  else size_checks @ (decl :: rhs_assignment)

let unwrap_block_or_skip = function
  | [({Stmt.Fixed.pattern= Block _; _} as b)] -> Some b
  | [{pattern= Skip; _}] -> None
  | x ->
      raise_s
        [%message "Expecting a block or skip, not" (x : Stmt.Located.t list)]

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
        | {Ast.lval= Ast.LVariable _; _} -> []
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
      NRFunApp (trans_fn_kind fn_kind name, trans_exprs args) |> swrap
  | Ast.IncrementLogProb e | Ast.TargetPE e -> TargetPE (trans_expr e) |> swrap
  | Ast.Tilde {arg; distribution; args; truncation} ->
      let suffix =
        Stan_math_signatures.dist_name_suffix ud_dists distribution.name
      in
      let name = distribution.name ^ suffix in
      let kind =
        let possible_names =
          List.map ~f:(( ^ ) distribution.name) Utils.distribution_suffices
          |> String.Set.of_list
        in
        if List.exists ~f:(fun (n, _) -> Set.mem possible_names n) ud_dists
        then Fun_kind.UserDefined (name, FnLpdf true)
        else StanLib (name, FnLpdf true, Common.Helpers.AoS)
      in
      let add_dist =
        Stmt.Fixed.Pattern.TargetPE
          Expr.
            { Fixed.pattern= FunApp (kind, trans_exprs (arg :: args))
            ; meta=
                Typed.Meta.create ~type_:UReal ~loc:mloc
                  ~adlevel:(Ast.expr_ad_lub (arg :: args))
                  () }
      in
      truncate_dist ud_dists distribution arg args truncation @ swrap add_dist
  | Ast.Print ps ->
      NRFunApp (CompilerInternal FnPrint, trans_printables smeta ps) |> swrap
  | Ast.Reject ps ->
      NRFunApp (CompilerInternal FnReject, trans_printables smeta ps) |> swrap
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
      Stmt.Helpers.[ensure_var (for_each bodyfn) iteratee' smeta]
  | Ast.FunDef _ ->
      raise_s
        [%message
          "Found function definition statement outside of function block"]
  | Ast.VarDecl
      {decl_type; transformation; identifier; initial_value; is_global= _} ->
      trans_decl declc smeta decl_type
        (Program.map_transformation trans_expr transformation)
        identifier initial_value
  | Ast.Block stmts -> Block (List.concat_map ~f:trans_stmt stmts) |> swrap
  | Ast.Profile (name, stmts) ->
      Profile (name, List.concat_map ~f:trans_stmt stmts) |> swrap
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
          ; fdsuffix=
              Fun_kind.(suffix_from_name funname.name |> map_suffix Fn.ignore)
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

let trans_sizedtype_decl declc tr name =
  let check fn x n =
    Stmt.Helpers.internal_nrfunapp fn
      Expr.Helpers.
        [str name; str (Fmt.strf "%a" Pretty_printing.pp_expression x); n]
      n.meta.loc
  in
  let grab_size fn n = function
    | Ast.({expr= IntNumeral i; _}) as s when float_of_string i >= 2. ->
        ([], trans_expr s)
    | Ast.({expr= IntNumeral _; _} | {expr= Variable _; _}) as s ->
        let e = trans_expr s in
        ([check fn s e], e)
    | s ->
        let e = trans_expr s in
        let decl_id = Fmt.strf "%s_%ddim__" name n in
        let decl =
          { Stmt.Fixed.pattern=
              Decl {decl_type= Sized SInt; decl_id; decl_adtype= DataOnly}
          ; meta= e.meta.loc }
        in
        let assign =
          { Stmt.Fixed.pattern= Assignment ((decl_id, UInt, []), e)
          ; meta= e.meta.loc }
        in
        let var =
          Expr.
            { Fixed.pattern= Var decl_id
            ; meta=
                Typed.Meta.
                  { type_= s.Ast.emeta.Ast.type_
                  ; adlevel= s.emeta.ad_level
                  ; loc= s.emeta.loc } }
        in
        ([decl; assign; check fn s var], var)
  in
  let rec go n = function
    | SizedType.(SInt | SReal) as t -> ([], t)
    | SVector (mem_type, dim) ->
        let fn =
          match (declc.dconstrain, tr) with
          | Some Constrain, Program.Simplex ->
              Internal_fun.FnValidateSizeSimplex
          | Some Constrain, UnitVector -> FnValidateSizeUnitVector
          | _ -> FnValidateSize
        in
        let l, dim = grab_size fn n dim in
        (l, SizedType.SVector (mem_type, dim))
    | SRowVector (mem_type, dim) ->
        let l, dim = grab_size FnValidateSize n dim in
        (l, SizedType.SRowVector (mem_type, dim))
    | SMatrix (mem_type, row_expr, col_expr) ->
        let l1, row = grab_size FnValidateSize n row_expr in
        let l2, col = grab_size FnValidateSize (n + 1) col_expr in
        let cf_cov =
          match (declc.dconstrain, tr) with
          | Some Constrain, CholeskyCov ->
              [ { Stmt.Fixed.pattern=
                    NRFunApp
                      ( StanLib
                          ( "check_greater_or_equal"
                          , FnPlain
                          , Common.Helpers.AoS )
                      , Expr.Helpers.
                          [ str ("cholesky_factor_cov " ^ name)
                          ; str
                              "num rows (must be greater or equal to num cols)"
                          ; row; col ] )
                ; meta= row.Expr.Fixed.meta.Expr.Typed.Meta.loc } ]
          | _ -> []
        in
        (l1 @ l2 @ cf_cov, SizedType.SMatrix (mem_type, row, col))
    | SArray (t, s) ->
        let l, s = grab_size FnValidateSize n s in
        let ll, t = go (n + 1) t in
        (l @ ll, SizedType.SArray (t, s))
  in
  go 1

let trans_block ud_dists declc block prog =
  let f stmt (accum1, accum2, accum3) =
    match stmt with
    | { Ast.stmt=
          VarDecl
            { decl_type= Sized type_
            ; identifier
            ; transformation
            ; initial_value
            ; is_global= true }
      ; smeta } ->
        let decl_id = identifier.Ast.name in
        let transform = Program.map_transformation trans_expr transformation in
        let rhs = Option.map ~f:trans_expr initial_value in
        let size, type_ =
          trans_sizedtype_decl declc transform identifier.name type_
        in
        let decl_adtype = declc.dadlevel in
        let decl_var =
          Expr.
            { Fixed.pattern= Var decl_id
            ; meta=
                Typed.Meta.create ~adlevel:declc.dadlevel ~loc:smeta.Ast.loc
                  ~type_:(SizedType.to_unsized type_)
                  () }
        in
        let decl =
          Stmt.
            { Fixed.pattern= Decl {decl_adtype; decl_id; decl_type= Sized type_}
            ; meta= smeta.loc }
        in
        let rhs_assignment =
          Option.map
            ~f:(fun e ->
              Stmt.Fixed.
                { pattern= Assignment ((decl_id, e.meta.type_, []), e)
                ; meta= smeta.loc } )
            rhs
          |> Option.to_list
        in
        let outvar =
          ( identifier.name
          , Program.
              { out_constrained_st= type_
              ; out_unconstrained_st= param_size transform type_
              ; out_block= block
              ; out_trans= transform } )
        in
        let stmts =
          if Utils.is_user_ident decl_id then
            let constrain_checks =
              match declc.dconstrain with
              | Some Constrain ->
                  check_transform_shape decl_id decl_var smeta.loc transform
              | Some Unconstrain ->
                  check_transform_shape decl_id decl_var smeta.loc transform
                  @ constrain_decl type_ declc.dconstrain transform decl_id
                      decl_var smeta.loc
              | Some Check ->
                  check_transform_shape decl_id decl_var smeta.loc transform
                  @ check_decl decl_var (Sized type_) decl_id transform
                      smeta.loc declc.dadlevel
              | None -> []
            in
            (decl :: rhs_assignment) @ constrain_checks
          else decl :: rhs_assignment
        in
        (outvar :: accum1, size @ accum2, stmts @ accum3)
    | stmt -> (accum1, accum2, trans_stmt ud_dists declc stmt @ accum3)
  in
  Option.value ~default:[] (get_block block prog)
  |> List.fold_right ~f ~init:([], [], [])

let stmt_contains_check stmt =
  let is_check = function
    | Fun_kind.CompilerInternal (Internal_fun.FnCheck _) -> true
    | _ -> false
  in
  Stmt.Helpers.contains_fn_kind is_check stmt

let migrate_checks_to_end_of_block stmts =
  let checks, not_checks = List.partition_tf ~f:stmt_contains_check stmts in
  not_checks @ checks

let trans_prog filename (p : Ast.typed_program) : Program.Typed.t =
  let {Ast.functionblock; datablock; transformeddatablock; modelblock; _} =
    p
  in
  let map f list_op =
    Option.value_map ~default:[] ~f:(List.concat_map ~f) list_op
  in
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
  let input_vars =
    map get_name_size datablock |> List.map ~f:(fun (n, st, _) -> (n, st))
  in
  let declc = {dconstrain= None; dadlevel= DataOnly} in
  let datab = map (trans_stmt {declc with dconstrain= Some Check}) datablock in
  let _, _, param =
    trans_block ud_dists
      {dconstrain= Some Constrain; dadlevel= AutoDiffable}
      Parameters p
  in
  let _, _, transform_inits =
    trans_block ud_dists {declc with dconstrain= Some Unconstrain} Parameters p
  in
  let out_param, paramsizes, param_gq =
    trans_block ud_dists {declc with dconstrain= Some Constrain} Parameters p
  in
  let _, _, txparam =
    trans_block ud_dists
      {dconstrain= Some Check; dadlevel= AutoDiffable}
      TransformedParameters p
  in
  let out_tparam, tparamsizes, txparam_gq =
    trans_block ud_dists
      {declc with dconstrain= Some Check}
      TransformedParameters p
  in
  let out_gq, gq_sizes, gq_stmts =
    trans_block ud_dists
      {declc with dconstrain= Some Check}
      GeneratedQuantities p
  in
  let output_vars = out_param @ out_tparam @ out_gq in
  let prepare_data =
    datab
    @ ( map
          (trans_stmt {declc with dconstrain= Some Check})
          transformeddatablock
      |> migrate_checks_to_end_of_block )
    @ paramsizes @ tparamsizes @ gq_sizes
  in
  let modelb =
    map (trans_stmt {declc with dadlevel= AutoDiffable}) modelblock
  in
  let log_prob =
    param
    @ (txparam |> migrate_checks_to_end_of_block)
    @
    match modelb with
    | [] -> []
    | hd :: _ -> [{pattern= Block modelb; meta= hd.meta}]
  in
  let txparam_decls, txparam_checks, txparam_stmts =
    txparam_gq
    |> List.partition3_map ~f:(function
         | {pattern= Decl _; _} as d -> `Fst d
         | s when stmt_contains_check s -> `Snd s
         | s -> `Trd s )
  in
  let compiler_if_return cond =
    Stmt.Fixed.
      { pattern=
          IfElse (cond, {pattern= Return None; meta= Location_span.empty}, None)
      ; meta= Location_span.empty }
  in
  let iexpr pattern = Expr.{pattern; Fixed.meta= Typed.Meta.empty} in
  let fnot e =
    FunApp (StanLib (Operator.to_string PNot, FnPlain, Common.Helpers.AoS), [e])
    |> iexpr
  in
  let tparam_early_return =
    let to_var fv = iexpr (Var (Flag_vars.to_string fv)) in
    let v1 = to_var EmitTransformedParameters in
    let v2 = to_var EmitGeneratedQuantities in
    [compiler_if_return (fnot (EOr (v1, v2) |> iexpr))]
  in
  let gq_early_return =
    [ compiler_if_return
        (fnot (Var (Flag_vars.to_string EmitGeneratedQuantities) |> iexpr)) ]
  in
  let generate_quantities =
    param_gq @ txparam_decls @ tparam_early_return @ txparam_stmts
    @ txparam_checks @ gq_early_return
    @ migrate_checks_to_end_of_block gq_stmts
  in
  let normalize_prog_name prog_name =
    if String.length prog_name > 0 && not (Char.is_alpha prog_name.[0]) then
      "_" ^ prog_name
    else prog_name
  in
  { functions_block= map (trans_fun_def ud_dists) functionblock
  ; input_vars
  ; prepare_data
  ; log_prob
  ; generate_quantities
  ; transform_inits
  ; output_vars
  ; prog_name= normalize_prog_name !Semantic_check.model_name
  ; prog_path= filename }
