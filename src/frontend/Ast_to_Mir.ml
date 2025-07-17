open Core
open Core.Poly
open Middle

let trans_fn_kind kind name =
  let fname = Utils.stdlib_distribution_name name in
  match kind with
  | Ast.StanLib suffix -> Fun_kind.StanLib (fname, suffix, AoS)
  | UserDefined suffix -> UserDefined (fname, suffix)

let without_underscores = String.filter ~f:(( <> ) '_')

let drop_leading_zeros s =
  match String.lfindi ~f:(fun _ c -> c <> '0') s with
  | Some p when p > 0 -> (
      match s.[p] with
      | 'e' | 'E' | '.' -> String.drop_prefix s (p - 1)
      | _ -> String.drop_prefix s p)
  | Some _ -> s
  | None -> "0"

let format_number s = s |> without_underscores |> drop_leading_zeros

let%expect_test "format_number0" =
  format_number "0_000." |> print_endline;
  [%expect "0."]

let%expect_test "format_number1" =
  format_number ".123_456" |> print_endline;
  [%expect ".123456"]

let rec op_to_funapp op args type_ =
  let loc = Ast.expr_loc_lub args in
  let adlevel =
    Ast.expr_ad_lub args |> Option.value_exn
    (* correctness inherited from typechecking *) in
  Expr.
    { Fixed.pattern=
        FunApp (StanLib (Operator.to_string op, FnPlain, AoS), trans_exprs args)
    ; meta= Expr.Typed.Meta.create ~type_ ~adlevel ~loc () }

and trans_expr {Ast.expr; Ast.emeta} =
  let ewrap pattern =
    Expr.
      { Fixed.pattern
      ; meta=
          Typed.Meta.
            {type_= emeta.Ast.type_; adlevel= emeta.ad_level; loc= emeta.loc} }
  in
  match expr with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, And, rhs) -> EAnd (trans_expr lhs, trans_expr rhs) |> ewrap
  | BinOp (lhs, Or, rhs) -> EOr (trans_expr lhs, trans_expr rhs) |> ewrap
  | BinOp (lhs, op, rhs) -> op_to_funapp op [lhs; rhs] emeta.type_
  | PrefixOp (op, e) | Ast.PostfixOp (e, op) -> op_to_funapp op [e] emeta.type_
  | Ast.TernaryIf (cond, ifb, elseb) ->
      Expr.Fixed.Pattern.TernaryIf
        (trans_expr cond, trans_expr ifb, trans_expr elseb)
      |> ewrap
  | Variable {name; _} -> Var name |> ewrap
  | IntNumeral x -> Lit (Int, format_number x) |> ewrap
  | RealNumeral x -> Lit (Real, format_number x) |> ewrap
  | ImagNumeral x -> Lit (Imaginary, format_number x) |> ewrap
  | FunApp (fn_kind, {name; _}, args) | CondDistApp (fn_kind, {name; _}, args)
    ->
      FunApp (trans_fn_kind fn_kind name, trans_exprs args) |> ewrap
  | GetTarget -> FunApp (StanLib ("target", FnTarget, AoS), []) |> ewrap
  | ArrayExpr eles ->
      FunApp (CompilerInternal FnMakeArray, trans_exprs eles) |> ewrap
  | RowVectorExpr eles ->
      FunApp (CompilerInternal FnMakeRowVec, trans_exprs eles) |> ewrap
  | Indexed (lhs, indices) ->
      Indexed (trans_expr lhs, List.map ~f:trans_idx indices) |> ewrap
  | TupleProjection (lhs, i) -> TupleProjection (trans_expr lhs, i) |> ewrap
  | TupleExpr eles ->
      FunApp (CompilerInternal FnMakeTuple, trans_exprs eles) |> ewrap
  | Promotion (e, ty, ad) -> Promotion (trans_expr e, ty, ad) |> ewrap

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
          Common.ICE.internal_compiler_error
            [%message "Expecting int or array" (e.emeta.type_ : UnsizedType.t)])

and trans_exprs exprs = List.map ~f:trans_expr exprs

let trans_sizedtype = SizedType.map trans_expr

let neg_inf =
  Expr.
    { Fixed.pattern= FunApp (CompilerInternal FnNegInf, [])
    ; meta=
        Typed.Meta.{type_= UReal; loc= Location_span.empty; adlevel= DataOnly}
    }

let trans_arg (adtype, ut, ident) = (adtype, ident.Ast.name, ut)

let truncate_dist ud_dists (id : Ast.identifier)
    (ast_obs : Ast.typed_expression) ast_args t =
  let cdf_suffix = "_lcdf" in
  let ccdf_suffix = "_lccdf" in
  let find_function_info sfx =
    let name = id.name ^ sfx in
    match List.find ~f:(fun (n, _) -> String.equal name n) ud_dists with
    | Some (name, tp) -> (Ast.UserDefined FnPlain, name, tp)
    | None ->
        ( Ast.StanLib FnPlain
        , name
        , if Stan_math_signatures.is_stan_math_function_name (id.name ^ "_lpmf")
          then UnsizedType.UInt
          else UnsizedType.UReal (* close enough *) ) in
  let targetme loc e =
    { Stmt.Fixed.meta= loc
    ; pattern= TargetPE (Expr.Helpers.unary_op Operator.PMinus e) } in
  let trunc cond_op extrema (x : Expr.Typed.t) y =
    let smeta = x.meta.loc in
    let ast_obs =
      if UnsizedType.is_container ast_obs.Ast.emeta.type_ then
        Ast.mk_typed_expression
          ~expr:
            (FunApp
               ( Ast.StanLib FnPlain
               , Ast.{name= extrema; id_loc= smeta}
               , [ast_obs] ))
          ~loc:smeta ~type_:UnsizedType.UReal ~ad_level:ast_obs.emeta.ad_level
      else ast_obs in
    { Stmt.Fixed.meta= smeta
    ; pattern=
        IfElse
          ( Expr.Helpers.binop (trans_expr ast_obs) cond_op x
          , {Stmt.Fixed.meta= smeta; pattern= TargetPE neg_inf}
          , Some y ) } in
  let funapp meta kind name args =
    Expr.{Fixed.pattern= FunApp (trans_fn_kind kind name, args); meta} in
  let maybe_promote_to_real tp lb : Expr.Typed.t =
    match (tp, Expr.Typed.type_of lb) with
    | UnsizedType.UInt, _ -> lb
    | _, UInt ->
        { pattern= Promotion (lb, UReal, lb.meta.adlevel)
        ; meta= {lb.meta with type_= UReal} }
    | _ -> lb in
  let inclusive_bound tp (lb : Expr.Typed.t) =
    if UnsizedType.is_int_type tp then
      Expr.Helpers.binop lb Minus Expr.Helpers.one
    else maybe_promote_to_real tp lb in
  let size_adjust e =
    if
      (not (UnsizedType.is_container ast_obs.Ast.emeta.type_))
      || List.exists
           ~f:(fun (i : Ast.typed_expression) ->
             UnsizedType.is_container i.emeta.type_)
           ast_args
    then e
    else
      (* Container y but scalar args - need to multiply by size(y) *)
      let trans_ast_obs = trans_expr ast_obs in
      let type_ = {trans_ast_obs.meta with type_= UnsizedType.UReal} in
      Expr.Helpers.binop e Times
        (Expr.Helpers.internal_funapp FnLength [trans_ast_obs] type_) in
  match t with
  | Ast.NoTruncate -> []
  | TruncateUpFrom lb ->
      let fk, fn, tp = find_function_info ccdf_suffix in
      let lb = trans_expr lb in
      [ trunc Less "min" lb
          (targetme lb.meta.loc
             (size_adjust
                (funapp lb.meta fk fn
                   (inclusive_bound tp lb :: trans_exprs ast_args)))) ]
  | TruncateDownFrom ub ->
      let fk, fn, tp = find_function_info cdf_suffix in
      let ub = trans_expr ub in
      [ trunc Greater "max" ub
          (targetme ub.meta.loc
             (size_adjust
                (funapp ub.meta fk fn
                   (maybe_promote_to_real tp ub :: trans_exprs ast_args)))) ]
  | TruncateBetween (lb, ub) ->
      let fk, fn, tp = find_function_info cdf_suffix in
      let lb, ub = (trans_expr lb, trans_expr ub) in
      let expr args =
        funapp ub.meta (Ast.StanLib FnPlain) "log_diff_exp"
          [ funapp ub.meta fk fn (maybe_promote_to_real tp ub :: args)
          ; funapp ub.meta fk fn (inclusive_bound tp lb :: args) ] in
      let statement =
        match
          List.findi
            ~f:(fun (_ : int) (e : Ast.typed_expression) ->
              UnsizedType.is_container e.emeta.type_)
            ast_args
        with
        (* If any of the arguments (besides the data) are vectors, need to generate a loop
           This can go away if https://github.com/stan-dev/stan/issues/1154 is implemented
        *)
        | Some (i, _) ->
            let ast_args = trans_exprs ast_args in
            (* avoid recomputing in each iteration of the loop *)
            let temp_decls, ast_args, symbol_reset =
              Stmt.Helpers.temp_vars ast_args in
            let bound =
              let e = List.nth_exn ast_args i in
              Expr.Helpers.internal_funapp FnLength [e]
                {e.meta with type_= UnsizedType.UInt} in
            let bodyfn (idx : Expr.Typed.t) =
              let args =
                List.map
                  ~f:(fun (e : Expr.Typed.t) ->
                    if UnsizedType.is_container e.meta.type_ then
                      Expr.Helpers.add_int_index e (Index.Single idx)
                    else e)
                  ast_args in
              targetme ub.meta.loc (size_adjust (expr args)) in
            let loop = Stmt.Helpers.mk_for bound bodyfn ub.meta.loc in
            symbol_reset ();
            Stmt.{Fixed.pattern= Block (temp_decls @ [loop]); meta= loop.meta}
        | None ->
            targetme ub.meta.loc (size_adjust (expr (trans_exprs ast_args)))
      in
      [trunc Less "min" lb (trunc Greater "max" ub statement)]

let unquote s =
  if s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.drop_suffix (String.drop_prefix s 1) 1
  else s

let trans_printables mloc (ps : Ast.typed_expression Ast.printable list) =
  List.map
    ~f:(function
      | Ast.PString s ->
          { (Expr.Helpers.str (unquote s)) with
            meta=
              Expr.Typed.Meta.create ~type_:UReal ~loc:mloc ~adlevel:DataOnly ()
          }
      | Ast.PExpr e -> trans_expr e)
    ps

(** These types signal the context for a declaration during statement translation.
   They are only interpreted by trans_decl.*)
type transform_action = Check | Constrain | IgnoreTransform [@@deriving sexp]

type decl_context =
  {transform_action: transform_action; dadlevel: UnsizedType.autodifftype}

let same_shape decl_id decl_var id var meta =
  if UnsizedType.is_scalar_type (Expr.Typed.type_of var) then []
  else
    [ Stmt.
        { Fixed.pattern=
            NRFunApp
              ( StanLib ("check_matching_dims", FnPlain, AoS)
              , Expr.Helpers.
                  [str "constraint"; str decl_id; decl_var; str id; var] )
        ; meta } ]

let check_transform_shape decl_id decl_var meta = function
  | Transformation.Offset e -> same_shape decl_id decl_var "offset" e meta
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
   |PositiveOrdered | Simplex | UnitVector | SumToZero | Identity
   |TupleTransformation _ | StochasticRow | StochasticColumn ->
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
                type_= Expr.Helpers.infer_type_of_indexed var.meta.type_ indices
              } }

let extract_transform_args var = function
  | Transformation.Lower a | Upper a -> [copy_indices var a]
  | Offset a -> [copy_indices var a; {a with Expr.Fixed.pattern= Lit (Int, "1")}]
  | Multiplier a -> [{a with pattern= Lit (Int, "0")}; copy_indices var a]
  | LowerUpper (a1, a2) | OffsetMultiplier (a1, a2) ->
      [copy_indices var a1; copy_indices var a2]
  | Covariance | Correlation | CholeskyCov | CholeskyCorr | Ordered
   |PositiveOrdered | Simplex | UnitVector | SumToZero | Identity
   |TupleTransformation _ | StochasticRow | StochasticColumn ->
      []

(** Allows [shrink_helper] to operate either on each dimension independently
   or on both matrix dimensions at once *)
type size_change =
  | Univariate of (Expr.Typed.t -> Expr.Typed.t)
  | Multivariate of (Expr.Typed.t -> Expr.Typed.t -> Expr.Typed.t)

(** We need to compute somewhat arbitrary new sizes for the unconstrained
    parameters. This function handles the primary cases:

  - A vector of size N is transformed to a vector of size (f N)
  - A matrix of size N x M is transformed to a vector of size (f N) x (f_d2 M)
  - A matrix of size N x N is transformed to a vector of size (f N M)
  - Arrays of the above are handled recursively
*)
let rec shrink_helper (f : size_change) f_d2 st =
  let f_assert_univariate d =
    match f with
    | Univariate f -> f d
    | Multivariate _ ->
        Common.ICE.internal_compiler_error
          [%message
            "To shrink a vector, the first argument must be a univariate \
             function "
              (st : Expr.Typed.t SizedType.t)] in
  match st with
  | SizedType.SArray (t, d) -> SizedType.SArray (shrink_helper f f_d2 t, d)
  | SVector (mem_pattern, d) -> SVector (mem_pattern, f_assert_univariate d)
  | SRowVector (mem_pattern, d) ->
      SRowVector (mem_pattern, f_assert_univariate d)
  | SMatrix (mem_pattern, d1, d2) -> (
      match f with
      | Univariate f_d1 -> SMatrix (mem_pattern, f_d1 d1, f_d2 d2)
      | Multivariate f -> SVector (mem_pattern, f d1 d2))
  | SInt | SReal | SComplex | STuple _ | SComplexRowVector _
   |SComplexVector _ | SComplexMatrix _ ->
      Common.ICE.internal_compiler_error
        [%message
          "Expecting SVector or SMatrix, got " (st : Expr.Typed.t SizedType.t)]

let rec transform_sizedtype transformation sizedtype =
  (* Functions for computing the new sizetype after some transformation *)
  let shrink_eigen_mat f st =
    (* Matrices become vectors, with size computed by [f] *)
    shrink_helper (Multivariate f) Fn.id st in
  let shrink_eigen_vec f st =
    (* Matrices are mapped to vectors, only depending on their first dimension for sizing *)
    shrink_eigen_mat (fun x _ -> f x) st in
  let shrink_eigen f1 f2 st =
    (* Types don't change, just sizes *)
    shrink_helper (Univariate f1) f2 st in
  (* Helper functions for computing the new sizes *)
  let minus_one d = Expr.Helpers.(binop d Minus (int 1)) in
  let k_choose_2 k =
    Expr.Helpers.(binop (binop k Times (binop k Minus (int 1))) Divide (int 2))
  in
  match transformation with
  | Transformation.Identity | Lower _ | Upper _
   |LowerUpper (_, _)
   |Offset _ | Multiplier _
   |OffsetMultiplier (_, _)
   |Ordered | PositiveOrdered | UnitVector ->
      sizedtype
  | TupleTransformation tms ->
      let _, dims = SizedType.get_array_dims sizedtype in
      let subtypes_transforms = Utils.zip_stuple_trans_exn sizedtype tms in
      (* NB: [build_sarray] is a no-op if this was not originally an array *)
      SizedType.build_sarray dims
        (SizedType.STuple
           (List.map subtypes_transforms ~f:(fun (st, trans) ->
                transform_sizedtype trans st)))
  | SumToZero -> shrink_eigen minus_one minus_one sizedtype
  | Simplex | StochasticColumn -> shrink_eigen minus_one Fn.id sizedtype
  | StochasticRow -> shrink_eigen Fn.id minus_one sizedtype
  | CholeskyCorr | Correlation -> shrink_eigen_vec k_choose_2 sizedtype
  | Covariance ->
      shrink_eigen_vec
        (fun k -> Expr.Helpers.(binop k Plus (k_choose_2 k)))
        sizedtype
  | CholeskyCov ->
      (* choose(N, 2) + (M - N) * N *)
      shrink_eigen_mat
        (fun m n ->
          Expr.Helpers.(
            binop
              (binop (k_choose_2 n) Plus n)
              Plus
              (binop (binop m Minus n) Times n)))
        sizedtype

let rec check_decl var decl_type' decl_trans smeta adlevel =
  let check_tuple var trans_subtypes =
    List.concat_mapi
      ~f:(fun i (decl_type', decl_trans) ->
        let var = Expr.Helpers.add_tuple_index var (i + 1) in
        check_decl var decl_type' decl_trans smeta adlevel)
      trans_subtypes in
  match decl_trans with
  | Transformation.LowerUpper (lb, ub) ->
      check_decl var decl_type' (Lower lb) smeta adlevel
      @ check_decl var decl_type' (Upper ub) smeta adlevel
  | TupleTransformation transforms when Transformation.has_check decl_trans ->
      let _, dims = SizedType.get_array_dims decl_type' in
      let subtypes_transforms =
        Utils.zip_stuple_trans_exn decl_type' transforms in
      if List.is_empty dims then check_tuple var subtypes_transforms
      else
        [ Stmt.Helpers.mk_nested_for (List.rev dims)
            (fun loopvars ->
              let var =
                List.fold ~f:Expr.Helpers.add_int_index ~init:var
                  (List.map ~f:(fun e -> Index.Single e) (List.rev loopvars))
              in
              Stmt.Fixed.
                { meta= smeta
                ; pattern= Block (check_tuple var subtypes_transforms) })
            smeta ]
  | _ when Transformation.has_check decl_trans ->
      let check_id id =
        let var_name = Fmt.str "%a" Expr.Typed.pp id in
        let args = extract_transform_args id decl_trans in
        Stmt.Helpers.internal_nrfunapp
          (FnCheck {trans= decl_trans; var_name; var= id})
          args smeta in
      [check_id var]
  | _ -> []

let check_sizedtype name st =
  let check x = function
    | {Expr.Fixed.pattern= Lit (Int, i); _} when float_of_string i >= 0. -> []
    | n ->
        [ Stmt.Helpers.internal_nrfunapp FnValidateSize
            Expr.Helpers.
              [ str name
              ; str (Fmt.str "%a" Pretty_printing.pp_typed_expression x); n ]
            n.meta.loc ] in
  let rec sizedtype = function
    | SizedType.(SInt | SReal | SComplex) as t -> ([], t)
    | SVector (mem_pattern, s) ->
        let e = trans_expr s in
        (check s e, SizedType.SVector (mem_pattern, e))
    | SRowVector (mem_pattern, s) ->
        let e = trans_expr s in
        (check s e, SizedType.SRowVector (mem_pattern, e))
    | SMatrix (mem_pattern, r, c) ->
        let er = trans_expr r in
        let ec = trans_expr c in
        (check r er @ check c ec, SizedType.SMatrix (mem_pattern, er, ec))
    | SComplexVector s ->
        let e = trans_expr s in
        (check s e, SizedType.SComplexVector e)
    | SComplexRowVector s ->
        let e = trans_expr s in
        (check s e, SizedType.SComplexRowVector e)
    | SComplexMatrix (r, c) ->
        let er = trans_expr r in
        let ec = trans_expr c in
        (check r er @ check c ec, SizedType.SComplexMatrix (er, ec))
    | SArray (t, s) ->
        let e = trans_expr s in
        let ll, t = sizedtype t in
        (check s e @ ll, SizedType.SArray (t, e))
    | STuple subtypes ->
        let checks, subtypes = List.unzip (List.map ~f:sizedtype subtypes) in
        (List.concat checks, STuple subtypes) in
  let ll, st = sizedtype st in
  (ll, Type.Sized st)

(* The statements that constrain and check a variable, given its context *)
let var_constrain_check_stmts dconstrain loc adlevel decl_id decl_var trans
    type_ =
  match (dconstrain, type_) with
  | Some Constrain, Type.Sized _ ->
      check_transform_shape decl_id decl_var loc trans
  | Some Check, Type.Sized st ->
      check_transform_shape decl_id decl_var loc trans
      @ check_decl decl_var st trans loc adlevel
  | _ -> []

let create_decl_with_assign decl_id declc decl_type initial_value transform
    smeta =
  let rhs = Option.map ~f:trans_expr initial_value in
  let decl_adtype =
    UnsizedType.fill_adtype_for_type declc.dadlevel (Type.to_unsized decl_type)
  in
  let decl_var =
    Expr.
      { Fixed.pattern= Var decl_id
      ; meta=
          Typed.Meta.create ~adlevel:decl_adtype ~loc:smeta
            ~type_:(Type.to_unsized decl_type)
            () } in
  let decl =
    Stmt.
      { Fixed.pattern=
          Decl {decl_adtype; decl_id; decl_type; initialize= Default}
      ; meta= smeta } in
  let rhs_assignment =
    Option.map
      ~f:(fun (e : Expr.Typed.t) ->
        Stmt.Fixed.
          { pattern= Assignment (Stmt.Helpers.lvariable decl_id, e.meta.type_, e)
          ; meta= smeta })
      rhs
    |> Option.to_list in
  if Utils.is_user_ident decl_id then
    (decl :: rhs_assignment)
    @ var_constrain_check_stmts (Some declc.transform_action) smeta decl_adtype
        decl_id decl_var transform decl_type
  else decl :: rhs_assignment

let unwrap_block_or_skip = function
  | [({Stmt.Fixed.pattern= Block _; _} as b)] -> Some b
  | [{pattern= Skip; _}] -> None
  | x ->
      Common.ICE.internal_compiler_error
        [%message "Expecting a block or skip, not" (x : Stmt.Located.t list)]

let index_tuple (e : Ast.typed_expression) i =
  let emeta =
    match (e.emeta.type_, e.emeta.ad_level) with
    | UnsizedType.UTuple ts, UnsizedType.TupleAD ads ->
        Ast.
          { type_= List.nth_exn ts i
          ; ad_level= List.nth_exn ads i
          ; loc= e.emeta.loc }
    | _ ->
        Common.ICE.internal_compiler_error
          [%message
            "Attempted to index into a non-tuple during lowering"
              (e : Ast.typed_expression)] in
  Ast.{expr= TupleProjection (e, i + 1); emeta}

let rec trans_stmt ud_dists (declc : decl_context) (ts : Ast.typed_statement) =
  let stmt_typed = ts.stmt and smeta = ts.smeta.loc in
  let trans_stmt =
    trans_stmt ud_dists {declc with transform_action= IgnoreTransform} in
  let trans_single_stmt s =
    match trans_stmt s with
    | [s] -> s
    | s -> Stmt.Fixed.{pattern= SList s; meta= smeta} in
  let swrap pattern = [Stmt.Fixed.{meta= smeta; pattern}] in
  let mloc = smeta in
  match stmt_typed with
  | Ast.Assignment {assign_lhs= LTuplePack {lvals; _}; assign_rhs; assign_op} ->
      trans_packed_assign smeta trans_stmt lvals assign_rhs assign_op
  | Ast.Assignment {assign_lhs= LValue lhs; assign_rhs; assign_op} ->
      trans_single_assignment smeta lhs assign_rhs assign_op
  | Ast.NRFunApp (fn_kind, {name; _}, args) ->
      NRFunApp (trans_fn_kind fn_kind name, trans_exprs args) |> swrap
  | Ast.TargetPE e -> TargetPE (trans_expr e) |> swrap
  | Ast.JacobianPE e -> JacobianPE (trans_expr e) |> swrap
  | Ast.Tilde {arg; distribution; args; truncation; kind} ->
      let sfx =
        match kind with
        | UserDefined (FnLpdf _) | StanLib (FnLpdf _) -> "_lpdf"
        | UserDefined (FnLpmf _) | StanLib (FnLpmf _) -> "_lpmf"
        | _ ->
            Common.ICE.internal_compiler_error
              [%message
                "Impossible: tilde with non-distribution after typechecking"
                  (distribution : Ast.identifier)
                  (kind : Ast.fun_kind)] in
      let name = distribution.name ^ sfx in
      let add_dist =
        let adlevel =
          if
            UnsizedType.any_autodiff
              (List.map ~f:(fun x -> x.emeta.ad_level) (arg :: args))
          then UnsizedType.AutoDiffable
          else DataOnly in
        Stmt.Fixed.Pattern.TargetPE
          Expr.
            { Fixed.pattern=
                FunApp (trans_fn_kind kind name, trans_exprs (arg :: args))
            ; meta= Typed.Meta.create ~type_:UReal ~loc:mloc ~adlevel () } in
      swrap add_dist @ truncate_dist ud_dists distribution arg args truncation
  | Ast.Print ps ->
      NRFunApp (CompilerInternal FnPrint, trans_printables smeta ps) |> swrap
  | Ast.Reject ps ->
      NRFunApp (CompilerInternal FnReject, trans_printables smeta ps) |> swrap
  | Ast.FatalError ps ->
      NRFunApp (CompilerInternal FnFatalError, trans_printables smeta ps)
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
        | x -> {x with pattern= Block [x]} in
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
        | b -> [b] in
      let decl_type =
        match Expr.Typed.type_of iteratee' with
        | UMatrix -> UnsizedType.UReal
        | t -> Expr.Helpers.(infer_type_of_indexed t [Index.Single loop_bottom])
      in
      let decl_loopvar =
        Stmt.Fixed.
          { meta= smeta
          ; pattern=
              Decl
                { decl_adtype= Expr.Typed.adlevel_of iteratee'
                ; decl_id= loopvar.name
                ; decl_type= Unsized decl_type
                ; initialize= Default } } in
      let assignment var =
        Stmt.Fixed.
          { pattern=
              Assignment (Stmt.Helpers.lvariable loopvar.name, decl_type, var)
          ; meta= smeta } in
      let bodyfn var =
        Stmt.Fixed.
          { pattern= Block (decl_loopvar :: assignment var :: body_stmts)
          ; meta= smeta } in
      Stmt.Helpers.[ensure_var (for_each bodyfn) iteratee' smeta]
  | Ast.FunDef _ ->
      Common.ICE.internal_compiler_error
        [%message
          "Found function definition statement outside of function block"]
  | Ast.VarDecl {decl_type; transformation; variables; is_global= _} ->
      List.concat_map
        ~f:(fun {identifier; initial_value} ->
          let transform = Transformation.map trans_expr transformation in
          let decl_id = identifier.Ast.name in
          let size_checks, dt = check_sizedtype decl_id decl_type in
          size_checks
          @ create_decl_with_assign decl_id declc dt initial_value transform
              smeta)
        variables
  | Ast.Block stmts -> Block (List.concat_map ~f:trans_stmt stmts) |> swrap
  | Ast.Profile (name, stmts) ->
      Profile (name, List.concat_map ~f:trans_stmt stmts) |> swrap
  | Ast.Return e -> Return (Some (trans_expr e)) |> swrap
  | Ast.ReturnVoid -> Return None |> swrap
  | Ast.Break -> Break |> swrap
  | Ast.Continue -> Continue |> swrap
  | Ast.Skip -> Skip |> swrap

and trans_packed_assign loc trans_stmt lvals rhs assign_op =
  (* TODO tuple-unpacking: could be more efficient in case where rhs is a tuple expr and
     names don't overlap *)
  let smeta = Ast.{loc; return_type= Incomplete} in
  let sym, reset = Common.Gensym.enter () in
  let rhs_type = rhs.emeta.type_ in
  let temp =
    { Stmt.Fixed.pattern=
        Decl
          { decl_adtype= rhs.emeta.ad_level
          ; decl_id= sym
          ; decl_type= Unsized rhs_type
          ; initialize= Uninit }
    ; meta= rhs.emeta.loc } in
  let assign =
    { temp with
      pattern= Assignment ((LVariable sym, []), rhs_type, trans_expr rhs) }
  in
  let temp_expr =
    Ast.{rhs with expr= Variable {name= sym; id_loc= Location_span.empty}} in
  let assigns =
    List.mapi lvals ~f:(fun i lval ->
        trans_stmt
          Ast.
            { stmt=
                Assignment
                  { assign_lhs= lval
                  ; assign_op
                  ; assign_rhs= index_tuple temp_expr i }
            ; smeta })
    |> List.concat in
  reset ();
  [Stmt.Fixed.{pattern= Block (temp :: assign :: assigns); meta= loc}]

and trans_single_assignment smeta assign_lhs assign_rhs assign_op =
  let rec group_lvalue carry_idcs lv =
    (* Group up non-tuple indices
       e.g. x[1][2].1[3] -> x[1,2].1[3]
       Done by passing current stack of indices down until it hits a non-indexed
    *)
    match lv.Ast.lval with
    | LVariable _ ->
        if List.is_empty carry_idcs then lv
        else {lv with lval= LIndexed (lv, carry_idcs)}
    | LTupleProjection (lv', ix) ->
        let lv'' = {lv with lval= LTupleProjection (group_lvalue [] lv', ix)} in
        if List.is_empty carry_idcs then lv''
        else {lv with lval= LIndexed (lv'', carry_idcs)}
    | LIndexed (lv', idcs) ->
        (* When we group indices,
           the metadata of group-indexed LHS equals the metadata of the outermost indexed LHS *)
        {lv with Ast.lval= (group_lvalue (idcs @ carry_idcs) lv').lval} in
  let grouped_lhs = group_lvalue [] assign_lhs in
  let rec trans_lvalue lv =
    match lv.Ast.lval with
    | LVariable v -> Stmt.Helpers.lvariable v.name
    | LTupleProjection (lv, ix) ->
        (Stmt.Fixed.Pattern.LTupleProjection (trans_lvalue lv, ix), [])
    | LIndexed (lv, idcs) ->
        let lbase, idxs = trans_lvalue lv in
        (lbase, idxs @ List.map ~f:trans_idx idcs) in
  let lhs = trans_lvalue grouped_lhs in
  (* The type of the assignee if it weren't indexed
     e.g. in x[1,2] it's type(x), and in y.2 it's type(y.2)
  *)
  let unindexed_type =
    match grouped_lhs.Ast.lval with
    | LVariable _ | LTupleProjection _ -> grouped_lhs.Ast.lmeta.type_
    | LIndexed (lv, _) -> lv.Ast.lmeta.type_ in
  let rhs =
    match assign_op with
    | Ast.Assign -> trans_expr assign_rhs
    | Ast.OperatorAssign op ->
        let assignee = Ast.expr_of_lvalue grouped_lhs in
        op_to_funapp op [assignee; assign_rhs] assignee.emeta.type_ in
  [{pattern= Assignment (lhs, unindexed_type, rhs); meta= smeta}]

let trans_fun_def ud_dists (ts : Ast.typed_statement) =
  match ts.stmt with
  | Ast.FunDef {returntype; funname; arguments; body} ->
      [ Program.
          { fdrt= returntype
          ; fdname= funname.name
          ; fdsuffix= Fun_kind.(suffix_from_name funname.name |> without_propto)
          ; fdargs= List.map ~f:trans_arg arguments
          ; fdbody=
              trans_stmt ud_dists
                {transform_action= IgnoreTransform; dadlevel= AutoDiffable}
                body
              |> unwrap_block_or_skip
          ; fdloc= ts.smeta.loc } ]
  | _ ->
      Common.ICE.internal_compiler_error
        [%message "Found non-function definition statement in function block"]

let get_block block prog =
  match block with
  | Program.Parameters -> prog.Ast.parametersblock
  | TransformedParameters -> prog.transformedparametersblock
  | GeneratedQuantities -> prog.generatedquantitiesblock

let rec trans_sizedtype_decl declc tr name st =
  let check fn x n =
    Stmt.Helpers.internal_nrfunapp fn
      Expr.Helpers.
        [str name; str (Fmt.str "%a" Pretty_printing.pp_typed_expression x); n]
      n.meta.loc in
  let grab_size fn n = function
    | Ast.{expr= IntNumeral i; _} as s when float_of_string i >= 2. ->
        ([], trans_expr s)
    | Ast.({expr= IntNumeral _; _} | {expr= Variable _; _}) as s ->
        let e = trans_expr s in
        ([check fn s e], e)
    | s ->
        let e = trans_expr s in
        let decl_name =
          name
          |> String.substr_replace_all ~pattern:"[]" ~with_:"_brack"
          |> String.substr_replace_all ~pattern:"." ~with_:"_dot" in
        let decl_id = Fmt.str "%s_%ddim__" decl_name n in
        let decl =
          { Stmt.Fixed.pattern=
              Decl
                { decl_type= Sized SInt
                ; decl_id
                ; decl_adtype= DataOnly
                ; initialize= Default }
          ; meta= e.meta.loc } in
        let assign =
          { Stmt.Fixed.pattern=
              Assignment (Stmt.Helpers.lvariable decl_id, UInt, e)
          ; meta= e.meta.loc } in
        let var =
          Expr.
            { Fixed.pattern= Var decl_id
            ; meta=
                Typed.Meta.
                  { type_= s.Ast.emeta.Ast.type_
                  ; adlevel= s.emeta.ad_level
                  ; loc= s.emeta.loc } } in
        ([decl; assign; check fn s var], var) in
  let rec go n = function
    | SizedType.(SInt | SReal | SComplex) as t -> ([], t)
    | SVector (mem_pattern, s) ->
        let fn =
          match (declc.transform_action, tr) with
          | Constrain, Transformation.Simplex ->
              Internal_fun.FnValidateSizeSimplex
          | Constrain, UnitVector -> FnValidateSizeUnitVector
          | _ -> FnValidateSize in
        let l, s = grab_size fn n s in
        (l, SizedType.SVector (mem_pattern, s))
    | SRowVector (mem_pattern, s) ->
        let l, s = grab_size FnValidateSize n s in
        (l, SizedType.SRowVector (mem_pattern, s))
    | SComplexRowVector s ->
        let l, s = grab_size FnValidateSize n s in
        (l, SizedType.SComplexRowVector s)
    | SComplexVector s ->
        let l, s = grab_size FnValidateSize n s in
        (l, SizedType.SComplexVector s)
    | SMatrix (mem_pattern, r, c) ->
        let l1, r = grab_size FnValidateSize n r in
        let l2, c = grab_size FnValidateSize (n + 1) c in
        let cf_cov =
          match (declc.transform_action, tr) with
          | Constrain, CholeskyCov ->
              [ { Stmt.Fixed.pattern=
                    NRFunApp
                      ( StanLib ("check_greater_or_equal", FnPlain, AoS)
                      , Expr.Helpers.
                          [ str ("cholesky_factor_cov " ^ name)
                          ; str
                              "num rows (must be greater or equal to num cols)"
                          ; r; c ] )
                ; meta= r.Expr.Fixed.meta.Expr.Typed.Meta.loc } ]
          | _ -> [] in
        (l1 @ l2 @ cf_cov, SizedType.SMatrix (mem_pattern, r, c))
    | SComplexMatrix (r, c) ->
        let l1, r = grab_size FnValidateSize n r in
        let l2, c = grab_size FnValidateSize (n + 1) c in
        (l1 @ l2, SizedType.SComplexMatrix (r, c))
    | SArray (t, s) ->
        let l, s = grab_size FnValidateSize n s in
        let ll, t = go (n + 1) t in
        (l @ ll, SizedType.SArray (t, s))
    | STuple subtypes ->
        let former_array_indices =
          String.concat (List.init (n - 1) ~f:(fun _ -> "[]")) in
        let stmts, subtypes' =
          List.unzip
            (List.mapi
               (List.zip_exn subtypes Utils.(tuple_trans_exn tr))
               ~f:(fun ix (st, trans) ->
                 trans_sizedtype_decl declc trans
                   (name ^ former_array_indices ^ "." ^ string_of_int (ix + 1))
                   st)) in
        (List.concat stmts, SizedType.STuple subtypes') in
  go 1 st

let trans_block ud_dists declc block prog =
  let f stmt (accum1, accum2, accum3) =
    match stmt with
    | { Ast.stmt=
          VarDecl {decl_type= type_; variables; transformation; is_global= true}
      ; smeta } ->
        let outvars, sizes, stmts =
          List.unzip3
          @@ List.map
               ~f:(fun {identifier; initial_value} ->
                 let decl_id = identifier.Ast.name in
                 let transform = Transformation.map trans_expr transformation in
                 let size, type_ =
                   trans_sizedtype_decl declc transform identifier.name type_
                 in
                 let outvar =
                   ( decl_id
                   , smeta.Ast.loc
                   , Program.
                       { out_constrained_st= type_
                       ; out_unconstrained_st=
                           transform_sizedtype transform type_
                       ; out_block= block
                       ; out_trans= transform } ) in
                 let stmts =
                   create_decl_with_assign decl_id declc (Sized type_)
                     initial_value transform smeta.loc in
                 (outvar, size, stmts))
               variables in
        ( outvars @ accum1
        , List.concat sizes @ accum2
        , List.concat stmts @ accum3 )
    | stmt -> (accum1, accum2, trans_stmt ud_dists declc stmt @ accum3) in
  Ast.get_stmts (get_block block prog) |> List.fold_right ~f ~init:([], [], [])

let stmt_contains_check stmt =
  let is_check = function
    | Fun_kind.CompilerInternal (Internal_fun.FnCheck _) -> true
    | _ -> false in
  Stmt.Helpers.contains_fn_kind is_check stmt

let migrate_checks_to_end_of_block stmts =
  let checks, not_checks = List.partition_tf ~f:stmt_contains_check stmts in
  not_checks @ checks

let gather_declarations (b : Ast.typed_statement Ast.block option) =
  let data = Ast.get_stmts b in
  List.concat_map data ~f:(function
    | {stmt= VarDecl {decl_type= sizedtype; transformation; variables; _}; _} ->
        List.map
          ~f:(fun {identifier; _} ->
            ( SizedType.map trans_expr sizedtype
            , Transformation.map trans_expr transformation
            , identifier.name ))
          variables
    | _ -> [])

let trans_prog filename (p : Ast.typed_program) : Program.Typed.t =
  let {Ast.functionblock; datablock; transformeddatablock; modelblock; _} =
    Deprecation_analysis.remove_unneeded_forward_decls p in
  let map f list_op =
    Option.value_map ~default:[]
      ~f:(fun {Ast.stmts; _} -> List.concat_map ~f stmts)
      list_op in
  let grab_fundef_names_and_types = function
    | {Ast.stmt= Ast.FunDef {funname; arguments= (_, type_, _) :: _; _}; _} ->
        [(funname.name, type_)]
    | _ -> [] in
  let ud_dists = map grab_fundef_names_and_types functionblock in
  let trans_stmt = trans_stmt ud_dists in
  let get_name_size (s : Ast.typed_statement) =
    match s.Ast.stmt with
    | Ast.VarDecl {decl_type= st; variables; transformation; _} ->
        List.map
          ~f:(fun {identifier; _} ->
            ( identifier.name
            , trans_sizedtype st
            , transformation
            , s.Ast.smeta.loc ))
          variables
    | _ -> [] in
  let input_vars =
    map get_name_size datablock
    |> List.map ~f:(fun (n, st, _, loc) -> (n, loc, st)) in
  let declc = {transform_action= IgnoreTransform; dadlevel= DataOnly} in
  let datab = map (trans_stmt {declc with transform_action= Check}) datablock in
  let _, _, param =
    trans_block ud_dists
      {transform_action= Constrain; dadlevel= AutoDiffable}
      Parameters p in
  (* Backends will add to transform_inits and unconstrain_array as needed *)
  let transform_inits = [] in
  let unconstrain_array = [] in
  let out_param, paramsizes, param_gq =
    trans_block ud_dists {declc with transform_action= Constrain} Parameters p
  in
  let _, _, txparam =
    trans_block ud_dists
      {transform_action= Check; dadlevel= AutoDiffable}
      TransformedParameters p in
  let out_tparam, tparamsizes, txparam_gq =
    trans_block ud_dists
      {declc with transform_action= Check}
      TransformedParameters p in
  let out_gq, gq_sizes, gq_stmts =
    trans_block ud_dists
      {declc with transform_action= Check}
      GeneratedQuantities p in
  let output_vars = out_param @ out_tparam @ out_gq in
  let prepare_data =
    datab
    @ (map
         (trans_stmt {declc with transform_action= Check})
         transformeddatablock
      |> migrate_checks_to_end_of_block)
    @ paramsizes @ tparamsizes @ gq_sizes in
  let modelb = map (trans_stmt {declc with dadlevel= AutoDiffable}) modelblock in
  let log_prob =
    param
    @ (txparam |> migrate_checks_to_end_of_block)
    @
    match modelb with
    | [] -> []
    | hd :: _ -> [{pattern= Block modelb; meta= hd.meta}] in
  let txparam_decls, txparam_checks, txparam_stmts =
    txparam_gq
    |> List.partition3_map ~f:(function
         | {pattern= Decl _; _} as d -> `Fst d
         | s when stmt_contains_check s -> `Snd s
         | s -> `Trd s) in
  let compiler_if_return cond =
    Stmt.Fixed.
      { pattern=
          IfElse (cond, {pattern= Return None; meta= Location_span.empty}, None)
      ; meta= Location_span.empty } in
  let iexpr pattern = Expr.{pattern; Fixed.meta= Typed.Meta.empty} in
  let fnot e =
    FunApp (StanLib (Operator.to_string PNot, FnPlain, AoS), [e]) |> iexpr in
  let tparam_early_return =
    let to_var fv = iexpr (Var (Flag_vars.to_string fv)) in
    let v1 = to_var EmitTransformedParameters in
    let v2 = to_var EmitGeneratedQuantities in
    [compiler_if_return (fnot (EOr (v1, v2) |> iexpr))] in
  let gq_early_return =
    [ compiler_if_return
        (fnot (Var (Flag_vars.to_string EmitGeneratedQuantities) |> iexpr)) ]
  in
  let generate_quantities =
    param_gq @ txparam_decls @ tparam_early_return @ txparam_stmts
    @ txparam_checks @ gq_early_return
    @ migrate_checks_to_end_of_block gq_stmts in
  let normalize_prog_name prog_name =
    if String.length prog_name > 0 && not (Char.is_alpha prog_name.[0]) then
      "_" ^ prog_name
    else prog_name in
  { functions_block= map (trans_fun_def ud_dists) functionblock
  ; input_vars
  ; prepare_data
  ; log_prob
  ; reverse_mode_log_prob= []
  ; generate_quantities
  ; transform_inits
  ; unconstrain_array
  ; output_vars
  ; prog_name= normalize_prog_name !Typechecker.model_name
  ; prog_path= filename }
