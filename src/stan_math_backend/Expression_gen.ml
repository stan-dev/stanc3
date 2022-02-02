open Core_kernel
open Core_kernel.Poly
open Middle
open Fmt

let ends_with suffix s = String.is_suffix ~suffix s
let starts_with prefix s = String.is_prefix ~prefix s

let stan_namespace_qualify f =
  if String.is_suffix ~suffix:"functor__" f || String.contains f ':' then f
  else "stan::math::" ^ f

let is_stan_math f = ends_with "__" f || starts_with "stan::math::" f

(* retun true if the type of the expression
   is integer, real, or complex (e.g. not a container) *)
let is_scalar e =
  match Expr.Typed.type_of e with UInt | UReal | UComplex -> true | _ -> false

let is_matrix e = Expr.Typed.type_of e = UMatrix
let is_row_vector e = Expr.Typed.type_of e = URowVector
let pretty_print e = Fmt.to_to_string Expr.Typed.pp e

let pp_call ppf (name, pp_arg, args) =
  pf ppf "@[<hov 2>%s(@,%a)@]" name (list ~sep:comma pp_arg) args

let rec stantype_prim_str = function
  | UnsizedType.UInt -> "int"
  | UArray t -> stantype_prim_str t
  | _ -> "double"

let rec local_scalar ut ad =
  match (ut, ad) with
  | UnsizedType.UArray t, _ -> local_scalar t ad
  | _, UnsizedType.DataOnly | UInt, AutoDiffable -> stantype_prim_str ut
  | _, AutoDiffable -> "local_scalar_t__"

let minus_one e =
  { e with
    Expr.Fixed.pattern=
      FunApp
        ( StanLib (Operator.to_string Minus, FnPlain, AoS)
        , [e; Expr.Helpers.loop_bottom] ) }

let is_single_index = function Index.Single _ -> true | _ -> false

let dont_need_range_check = function
  | Index.Single Expr.Fixed.{pattern= Var id; _} -> not (Utils.is_user_ident id)
  | _ -> false

let promote_adtype =
  List.fold
    ~f:(fun accum expr ->
      match Expr.Typed.adlevel_of expr with
      | AutoDiffable -> AutoDiffable
      | _ -> accum )
    ~init:UnsizedType.DataOnly

let promote_unsizedtype es =
  let rec fold_type accum mtype =
    match (accum, mtype) with
    | UnsizedType.UReal, _ -> UnsizedType.UReal
    | _, UnsizedType.UReal -> UReal
    | UArray t1, UArray t2 -> UArray (fold_type t1 t2)
    | _, mtype -> mtype in
  List.map es ~f:Expr.Typed.type_of
  |> List.reduce ~f:fold_type
  |> Option.value ~default:UReal

let%expect_test "promote_unsized" =
  let e mtype =
    Expr.{Fixed.pattern= Var "x"; meta= Typed.Meta.{empty with type_= mtype}}
  in
  let tests =
    [[e UInt; e UReal]; [e UReal; e UInt]; [e (UArray UInt); e (UArray UReal)]]
  in
  print_s
    [%sexp (tests |> List.map ~f:promote_unsizedtype : UnsizedType.t list)] ;
  [%expect {| (UReal UReal (UArray UReal)) |}]

let to_var s = Expr.{Fixed.pattern= Var s; meta= Typed.Meta.empty}

let rec pp_unsizedtype_custom_scalar ppf (scalar, ut) =
  match ut with
  | UnsizedType.UInt | UReal -> string ppf scalar
  | UComplex -> pf ppf "std::complex<%s>" scalar
  | UArray t -> pf ppf "std::vector<%a>" pp_unsizedtype_custom_scalar (scalar, t)
  | UMatrix -> pf ppf "Eigen::Matrix<%s, -1, -1>" scalar
  | URowVector -> pf ppf "Eigen::Matrix<%s, 1, -1>" scalar
  | UVector -> pf ppf "Eigen::Matrix<%s, -1, 1>" scalar
  | UMathLibraryFunction | UFun _ ->
      Common.FatalError.fatal_error_msg
        [%message "Function types not implemented"]

let pp_unsizedtype_custom_scalar_eigen_exprs ppf (scalar, ut) =
  match ut with
  | UnsizedType.UInt | UReal | UMatrix | URowVector | UVector ->
      string ppf scalar
  | UComplex -> pf ppf "std::complex<%s>" scalar
  | UArray t ->
      (* Expressions are not accepted for arrays of Eigen::Matrix *)
      pf ppf "std::vector<%a>" pp_unsizedtype_custom_scalar (scalar, t)
  | UMathLibraryFunction | UFun _ ->
      Common.FatalError.fatal_error_msg
        [%message "Function types not implemented"]

let pp_unsizedtype_local ppf (adtype, ut) =
  let s = local_scalar ut adtype in
  pp_unsizedtype_custom_scalar ppf (s, ut)

let pp_expr_type ppf e =
  pp_unsizedtype_local ppf Expr.Typed.(adlevel_of e, type_of e)

let rec pp_possibly_var_decl ppf (adtype, ut, mem_pattern) =
  let scalar = local_scalar ut adtype in
  let pp_var_decl ppf p_ut =
    if mem_pattern = Common.Helpers.SoA && adtype = UnsizedType.AutoDiffable
    then
      pf ppf "@[<hov 2>stan::conditional_var_value_t<%s,@ @,%a>@]" scalar
        pp_unsizedtype_local (adtype, p_ut)
    else pf ppf "%a" pp_unsizedtype_local (adtype, p_ut) in
  match ut with
  | UArray t ->
      pf ppf "@[<hov 2>std::vector<@,%a>@]" pp_possibly_var_decl
        (adtype, t, mem_pattern)
  | UMatrix | UVector | URowVector -> pf ppf "%a" pp_var_decl ut
  | UReal | UInt | UComplex -> pf ppf "%a" pp_unsizedtype_local (adtype, ut)
  | x -> raise_s [%message (x : UnsizedType.t) "not implemented yet"]

let suffix_args = function
  | Fun_kind.FnRng -> ["base_rng__"]
  | FnTarget -> ["lp__"; "lp_accum__"]
  | FnPlain | FnLpdf _ -> []

let demangle_unnormalized_name udf suffix f =
  match suffix with
  | Fun_kind.FnLpdf true -> f ^ "<propto__>"
  | FnLpdf false -> f ^ "<false>"
  | FnTarget when udf -> f ^ "<propto__>"
  | _ -> f

let fn_renames =
  List.map
    ~f:(fun (k, v) -> (Internal_fun.to_string k, v))
    [ (Internal_fun.FnLength, "stan::math::size")
    ; (FnNegInf, "stan::math::negative_infinity")
    ; (FnResizeToMatch, "stan::math::resize_to_match")
    ; (FnNaN, "std::numeric_limits<double>::quiet_NaN") ]
  |> String.Map.of_alist_exn

let map_rect_calls = Int.Table.create ()
let functor_suffix = "_functor__"
let reduce_sum_functor_suffix = "_rsfunctor__"
let variadic_ode_functor_suffix = "_odefunctor__"
let variadic_dae_functor_suffix = "_daefunctor__"

let functor_suffix_select hof =
  match hof with
  | x when Stan_math_signatures.is_reduce_sum_fn x -> reduce_sum_functor_suffix
  | x when Stan_math_signatures.is_variadic_ode_fn x ->
      variadic_ode_functor_suffix
  | x when Stan_math_signatures.is_variadic_dae_fn x ->
      variadic_dae_functor_suffix
  | _ -> functor_suffix

let constraint_to_string = function
  | Transformation.Ordered -> Some "ordered"
  | PositiveOrdered -> Some "positive_ordered"
  | Simplex -> Some "simplex"
  | UnitVector -> Some "unit_vector"
  | CholeskyCorr -> Some "cholesky_factor_corr"
  | CholeskyCov -> Some "cholesky_factor_cov"
  | Correlation -> Some "corr_matrix"
  | Covariance -> Some "cov_matrix"
  | Lower _ -> Some "lb"
  | Upper _ -> Some "ub"
  | LowerUpper _ -> Some "lub"
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> Some "offset_multiplier"
  | Identity -> None

let check_to_string = function
  | Transformation.Lower _ -> Some "greater_or_equal"
  | Upper _ -> Some "less_or_equal"
  | CholeskyCov -> Some "cholesky_factor"
  | LowerUpper _ ->
      Common.FatalError.fatal_error_msg
        [%message "LowerUpper is really two other checks tied together"]
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> None
  | t -> constraint_to_string t

let default_multiplier = 1
let default_offset = 0

let transform_args = function
  | Transformation.Offset offset -> [offset; Expr.Helpers.int default_multiplier]
  | Multiplier multiplier -> [Expr.Helpers.int default_offset; multiplier]
  | transform -> Transformation.fold (fun args arg -> args @ [arg]) [] transform

let rec pp_index ppf = function
  | Index.All -> pf ppf "stan::model::index_omni()"
  | Single e -> pf ppf "stan::model::index_uni(%a)" pp_expr e
  | Upfrom e -> pf ppf "stan::model::index_min(%a)" pp_expr e
  | Between (e_low, e_high) ->
      pf ppf "stan::model::index_min_max(%a, %a)" pp_expr e_low pp_expr e_high
  | MultiIndex e -> pf ppf "stan::model::index_multi(%a)" pp_expr e

and pp_indexes ppf = function
  | [] -> pf ppf ""
  | idxs -> pf ppf "@[<hov 2>%a@]" (list ~sep:comma pp_index) idxs

and pp_logical_op ppf op lhs rhs =
  pf ppf
    "(stan::math::primitive_value(@,%a)@ %s@ stan::math::primitive_value(@,%a))"
    pp_expr lhs op pp_expr rhs

and pp_unary ppf fm es = pf ppf fm pp_expr (List.hd_exn es)

and pp_binary_op ppf op es =
  pf ppf "(%a@ %s@ %a)" pp_expr (first es) op pp_expr (second es)

and pp_binary_f ppf f es =
  pf ppf "%s(@,%a,@ %a)" f pp_expr (first es) pp_expr (second es)

and first es = List.nth_exn es 0
and second es = List.nth_exn es 1

and pp_scalar_binary ppf op fn es =
  if is_scalar (first es) && is_scalar (second es) then pp_binary_op ppf op es
  else pp_binary_f ppf fn es

and gen_operator_app op ppf es =
  match op with
  | Operator.Plus -> pp_scalar_binary ppf "+" "stan::math::add" es
  | PMinus ->
      pp_unary ppf
        (if is_scalar (List.hd_exn es) then "-%a" else "stan::math::minus(@,%a)")
        es
  | PPlus -> pp_unary ppf "%a" es
  | Transpose ->
      pp_unary ppf
        ( if is_scalar (List.hd_exn es) then "%a"
        else "stan::math::transpose(@,%a)" )
        es
  | PNot -> pp_unary ppf "stan::math::logical_negation(@,%a)" es
  | Minus -> pp_scalar_binary ppf "-" "stan::math::subtract" es
  | Times -> pp_scalar_binary ppf "*" "stan::math::multiply" es
  | Divide | IntDivide ->
      if
        is_matrix (second es)
        && (is_matrix (first es) || is_row_vector (first es))
      then pp_binary_f ppf "stan::math::mdivide_right" es
      else pp_scalar_binary ppf "/" "stan::math::divide" es
  | Modulo -> pp_binary_f ppf "stan::math::modulus" es
  | LDivide -> pp_binary_f ppf "stan::math::mdivide_left" es
  | And | Or ->
      Common.FatalError.fatal_error_msg
        [%message "And/Or should have been converted to an expression"]
  | EltTimes -> pp_scalar_binary ppf "*" "stan::math::elt_multiply" es
  | EltDivide -> pp_scalar_binary ppf "/" "stan::math::elt_divide" es
  | Pow -> pp_binary_f ppf "stan::math::pow" es
  | EltPow -> pp_binary_f ppf "stan::math::pow" es
  | Equals -> pp_binary_f ppf "stan::math::logical_eq" es
  | NEquals -> pp_binary_f ppf "stan::math::logical_neq" es
  | Less -> pp_binary_f ppf "stan::math::logical_lt" es
  | Leq -> pp_binary_f ppf "stan::math::logical_lte" es
  | Greater -> pp_binary_f ppf "stan::math::logical_gt" es
  | Geq -> pp_binary_f ppf "stan::math::logical_gte" es

and gen_misc_special_math_app (f : string)
    (mem_pattern : Common.Helpers.mem_pattern)
    (ret_type : UnsizedType.returntype option) =
  match f with
  | "lmultiply" ->
      Some (fun ppf es -> pp_binary_f ppf "stan::math::multiply_log" es)
  | "lchoose" ->
      Some
        (fun ppf es -> pp_binary_f ppf "stan::math::binomial_coefficient_log" es)
  | "target" -> Some (fun ppf _ -> pf ppf "stan::math::get_lp(lp__, lp_accum__)")
  | "get_lp" -> Some (fun ppf _ -> pf ppf "stan::math::get_lp(lp__, lp_accum__)")
  | f when Map.mem fn_renames f ->
      Some (fun ppf es -> pp_call ppf (Map.find_exn fn_renames f, pp_expr, es))
  | "rep_matrix" | "rep_vector" | "rep_row_vector" | "append_row" | "append_col"
    when mem_pattern = Common.Helpers.SoA -> (
      let is_autodiffable Expr.Fixed.{meta= Expr.Typed.Meta.{adlevel; _}; _} =
        adlevel = UnsizedType.AutoDiffable in
      match ret_type with
      | Some (UnsizedType.ReturnType t) ->
          Some
            (fun ppf es ->
              match List.exists ~f:is_autodiffable es with
              | true ->
                  pf ppf "%s<%a>(@,%a)" f pp_possibly_var_decl
                    (UnsizedType.AutoDiffable, t, mem_pattern)
                    (list ~sep:comma pp_expr) es
              | false -> pf ppf "%s(@,%a)" f (list ~sep:comma pp_expr) es )
      | Some Void -> None
      | None -> None )
  | _ -> None

and gen_functionals fname suffix es mem_pattern =
  let contains_hof_vars = function
    | {Expr.Fixed.pattern= Var _; meta= {Expr.Typed.Meta.type_= UFun _; _}} ->
        true
    | _ -> false in
  let is_hof_call = List.exists ~f:contains_hof_vars es in
  if not is_hof_call then None
  else
    let pp_hof_variables ppf es =
      let convert_hof_vars = function
        | { Expr.Fixed.pattern= Var name
          ; meta= {Expr.Typed.Meta.type_= UFun _; _} } as e ->
            { e with
              pattern=
                FunApp
                  ( StanLib
                      (name ^ functor_suffix_select fname, FnPlain, mem_pattern)
                  , [] ) }
        | e -> e in
      let converted_es = List.map ~f:convert_hof_vars es in
      let msgs = "pstream__" |> to_var in
      (* Here, because these signatures are written in C++ such that they
         wanted to have optional arguments and piggyback on C++ default
         arguments and not write the necessary overloads, we have to
         reorder the arguments as pstream__ does not always come last
         in a way that is specific to the function name. If you are a C++
         developer please don't add more of these - just add the
         overloads.
      *)
      let fname, args =
        match (fname, converted_es) with
        | "algebra_solver", f :: x :: y :: dat :: datint :: tl
         |"algebra_solver_newton", f :: x :: y :: dat :: datint :: tl ->
            (fname, f :: x :: y :: dat :: datint :: msgs :: tl)
        | "integrate_1d", f :: a :: b :: theta :: x_r :: x_i :: tl ->
            (fname, f :: a :: b :: theta :: x_r :: x_i :: msgs :: tl)
        | "integrate_ode_bdf", f :: y0 :: t0 :: ts :: theta :: x :: x_int :: tl
         |( "integrate_ode_adams"
          , f :: y0 :: t0 :: ts :: theta :: x :: x_int :: tl )
         |"integrate_ode_rk45", f :: y0 :: t0 :: ts :: theta :: x :: x_int :: tl
          ->
            (fname, f :: y0 :: t0 :: ts :: theta :: x :: x_int :: msgs :: tl)
        | ( x
          , {pattern= FunApp ((UserDefined (f, _) | StanLib (f, _, _)), _); _}
            :: grainsize :: container :: tl )
          when Stan_math_signatures.is_reduce_sum_fn x ->
            let chop_functor_suffix =
              String.chop_suffix_exn ~suffix:reduce_sum_functor_suffix in
            let propto_template =
              if Utils.is_distribution_name (chop_functor_suffix f) then
                if Utils.is_unnormalized_distribution (chop_functor_suffix f)
                then "<propto__>"
                else "<false>"
              else "" in
            let normalized_dist_functor =
              Utils.stdlib_distribution_name (chop_functor_suffix f)
              ^ reduce_sum_functor_suffix in
            ( strf "%s<%s%s>" fname normalized_dist_functor propto_template
            , grainsize :: container :: msgs :: tl )
        | x, f :: y0 :: t0 :: ts :: rel_tol :: abs_tol :: max_steps :: tl
          when Stan_math_signatures.is_variadic_ode_fn x
               && String.is_suffix fname
                    ~suffix:Stan_math_signatures.ode_tolerances_suffix
               && not (Stan_math_signatures.variadic_ode_adjoint_fn = x) ->
            ( fname
            , f :: y0 :: t0 :: ts :: rel_tol :: abs_tol :: max_steps :: msgs
              :: tl )
        | x, f :: y0 :: t0 :: ts :: tl
          when Stan_math_signatures.is_variadic_ode_fn x
               && not (Stan_math_signatures.variadic_ode_adjoint_fn = x) ->
            (fname, f :: y0 :: t0 :: ts :: msgs :: tl)
        | ( x
          , f
            :: y0
               :: t0
                  :: ts
                     :: rel_tol
                        :: abs_tol
                           :: rel_tol_b
                              :: abs_tol_b
                                 :: rel_tol_q
                                    :: abs_tol_q
                                       :: max_num_steps
                                          :: num_checkpoints
                                             :: interpolation_polynomial
                                                :: solver_f :: solver_b :: tl )
          when Stan_math_signatures.variadic_ode_adjoint_fn = x ->
            ( fname
            , f :: y0 :: t0 :: ts :: rel_tol :: abs_tol :: rel_tol_b
              :: abs_tol_b :: rel_tol_q :: abs_tol_q :: max_num_steps
              :: num_checkpoints :: interpolation_polynomial :: solver_f
              :: solver_b :: msgs :: tl )
        | ( x
          , f :: yy0 :: yp0 :: t0 :: ts :: rel_tol :: abs_tol :: max_steps :: tl
          )
          when Stan_math_signatures.is_variadic_dae_fn x
               && String.is_suffix fname
                    ~suffix:Stan_math_signatures.dae_tolerances_suffix ->
            ( fname
            , f :: yy0 :: yp0 :: t0 :: ts :: rel_tol :: abs_tol :: max_steps
              :: msgs :: tl )
        | x, f :: yy0 :: yp0 :: t0 :: ts :: tl
          when Stan_math_signatures.is_variadic_dae_fn x ->
            (fname, f :: yy0 :: yp0 :: t0 :: ts :: msgs :: tl)
        | ( "map_rect"
          , {pattern= FunApp ((UserDefined (f, _) | StanLib (f, _, _)), _); _}
            :: tl ) ->
            let next_map_rect_id = Hashtbl.length map_rect_calls + 1 in
            Hashtbl.add_exn map_rect_calls ~key:next_map_rect_id ~data:f ;
            (strf "%s<%d, %s>" fname next_map_rect_id f, tl @ [msgs])
        | _, args -> (fname, args @ [msgs]) in
      let fname =
        stan_namespace_qualify fname |> demangle_unnormalized_name false suffix
      in
      pp_call ppf (fname, pp_expr, args) in
    Some pp_hof_variables

and read_data ut ppf es =
  let i_or_r_or_c =
    match ut with
    | UnsizedType.UArray UInt -> "i"
    | UArray UReal -> "r"
    | UArray UComplex -> "c"
    | UInt | UReal | UComplex | UVector | URowVector | UMatrix | UArray _
     |UFun _ | UMathLibraryFunction ->
        Common.FatalError.fatal_error_msg
          [%message "Can't ReadData of " (ut : UnsizedType.t)] in
  pf ppf "context__.vals_%s(%a)" i_or_r_or_c pp_expr (List.hd_exn es)

(* assumes everything well formed from parser checks *)
and gen_fun_app suffix ppf fname es mem_pattern
    (ret_type : UnsizedType.returntype option) =
  let pp =
    let maybe_rewritten =
      [ Option.map ~f:gen_operator_app (Operator.of_string_opt fname)
      ; gen_misc_special_math_app fname mem_pattern ret_type
      ; gen_functionals fname suffix es mem_pattern ]
      |> List.filter_opt |> List.hd in
    match maybe_rewritten with
    | Some s -> s
    | None ->
        fun ppf es ->
          let fname =
            stan_namespace_qualify fname
            |> demangle_unnormalized_name false suffix in
          let extra = suffix_args suffix |> List.map ~f:to_var in
          pp_call ppf (fname, pp_expr, es @ extra) in
  pf ppf "@[<hov 2>%a@]" pp es

and pp_user_defined_fun ppf (f, suffix, es) =
  let extra_args = suffix_args suffix @ ["pstream__"] in
  let sep = if List.is_empty es then "" else ", " in
  pf ppf "@[<hov 2>%s(@,%a%s)@]"
    (demangle_unnormalized_name true suffix f)
    (list ~sep:comma pp_expr) es
    (sep ^ String.concat ~sep:", " extra_args)

and pp_compiler_internal_fn ad ut f ppf es =
  let pp_array_literal ut ppf es =
    pf ppf "std::vector<%a>{@,%a}" pp_unsizedtype_local (ad, ut)
      (list ~sep:comma pp_expr) es in
  match f with
  | Internal_fun.FnMakeArray ->
      let ut =
        match ut with
        | UnsizedType.UArray ut -> ut
        | _ ->
            Common.FatalError.fatal_error_msg
              [%message
                "Array literal must have array type but found "
                  (ut : UnsizedType.t)] in
      pp_array_literal ut ppf es
  | FnMakeRowVec -> (
    match ut with
    | UnsizedType.URowVector ->
        let st = local_scalar ut (promote_adtype es) in
        if List.is_empty es then pf ppf "Eigen::Matrix<%s,1,-1>(0)" st
        else
          pf ppf "(Eigen::Matrix<%s,1,-1>(%d) <<@ %a).finished()" st
            (List.length es) (list ~sep:comma pp_expr) es
    | UMatrix ->
        pf ppf "stan::math::to_matrix(@,%a)" (pp_array_literal URowVector) es
    | _ ->
        Common.FatalError.fatal_error_msg
          [%message
            "Unexpected type for row vector literal" (ut : UnsizedType.t)] )
  | FnReadData -> read_data ut ppf es
  | FnReadDataSerializer ->
      pf ppf "@[<hov 2>in__.read<@,%a>()@]" pp_unsizedtype_local
        (UnsizedType.AutoDiffable, UnsizedType.UReal)
  | FnReadParam {constrain; dims; mem_pattern} -> (
      let constrain_opt = constraint_to_string constrain in
      match constrain_opt with
      | None ->
          pf ppf "@[<hov 2>in__.template read<@,%a>(@,%a)@]"
            pp_possibly_var_decl
            (UnsizedType.AutoDiffable, ut, mem_pattern)
            (list ~sep:comma pp_expr) dims
      | Some constraint_string ->
          let constraint_args = transform_args constrain in
          let lp =
            Expr.Fixed.{pattern= Var "lp__"; meta= Expr.Typed.Meta.empty} in
          let args = constraint_args @ [lp] @ dims in
          pf ppf
            "@[<hov 2>in__.template read_constrain_%s<@,\
             %a,@ @,\
             jacobian__>(@,\
             %a)@]"
            constraint_string pp_possibly_var_decl
            (UnsizedType.AutoDiffable, ut, mem_pattern)
            (list ~sep:comma pp_expr) args )
  | FnDeepCopy ->
      gen_fun_app FnPlain ppf "stan::model::deep_copy" es Common.Helpers.AoS
        (Some UnsizedType.Void)
  | _ ->
      gen_fun_app FnPlain ppf (Internal_fun.to_string f) es Common.Helpers.AoS
        (Some UnsizedType.Void)

and pp_indexed ppf (vident, indices, pretty) =
  pf ppf "@[<hov 2>stan::model::rvalue(@,%s,@ %S,@ %a)@]" vident pretty
    pp_indexes indices

and pp_indexed_simple ppf (obj, idcs) =
  let idx_minus_one = function
    | Index.Single e -> minus_one e
    | MultiIndex e | Between (e, _) | Upfrom e ->
        Common.FatalError.fatal_error_msg
          [%message
            "No non-Single indices allowed" ~obj
              (idcs : Expr.Typed.t Index.t list)
              (Expr.Typed.loc_of e : Location_span.t)]
    | All ->
        Common.FatalError.fatal_error_msg
          [%message
            "No non-Single indices allowed" ~obj
              (idcs : Expr.Typed.t Index.t list)] in
  pf ppf "%s%a" obj
    (fun ppf idcs ->
      match idcs with
      | [] -> ()
      | idcs -> pf ppf "[%a]" (list ~sep:(const string "][") pp_expr) idcs )
    (List.map ~f:idx_minus_one idcs)

and pp_expr ppf Expr.Fixed.{pattern; meta} =
  match pattern with
  | Var s -> pf ppf "%s" s
  | Lit (Str, s) -> pf ppf "\"%s\"" (Cpp_str.escaped s)
  | Lit (Imaginary, s) -> pf ppf "stan::math::to_complex(0, %s)" s
  | Lit ((Real | Int), s) -> pf ppf "%s" s
  | Promotion (expr, UReal, _) when is_scalar expr -> pp_expr ppf expr
  | Promotion (expr, UComplex, DataOnly) when is_scalar expr ->
      (* this is in principle a little better than promote_scalar since it is constexpr *)
      pf ppf "stan::math::to_complex(%a, 0)" pp_expr expr
  | Promotion (expr, ut, ad) ->
      pf ppf "stan::math::promote_scalar<%a>(%a)" pp_unsizedtype_local (ad, ut)
        pp_expr expr
  | FunApp
      ( StanLib (op, _, _)
      , [ { meta= {type_= URowVector; _}
          ; pattern= FunApp (CompilerInternal FnMakeRowVec, es) } ] )
    when Operator.(Some Transpose = of_string_opt op) ->
      let st = local_scalar UVector (promote_adtype es) in
      if List.is_empty es then pf ppf "Eigen::Matrix<%s,-1,1>(0)" st
      else
        pf ppf "(Eigen::Matrix<%s,-1,1>(%d) <<@ %a).finished()" st
          (List.length es) (list ~sep:comma pp_expr) es
  | FunApp (StanLib (f, suffix, mem_pattern), es) ->
      let fun_args = List.map ~f:Expr.Typed.fun_arg es in
      let ret_type = Stan_math_signatures.stan_math_returntype f fun_args in
      gen_fun_app suffix ppf f es mem_pattern ret_type
  | FunApp (CompilerInternal f, es) ->
      pp_compiler_internal_fn meta.adlevel meta.type_ f ppf es
  | FunApp (UserDefined (f, suffix), es) ->
      pp_user_defined_fun ppf (f, suffix, es)
  | EAnd (e1, e2) -> pp_logical_op ppf "&&" e1 e2
  | EOr (e1, e2) -> pp_logical_op ppf "||" e1 e2
  | TernaryIf (ec, et, ef) ->
      let tform ppf = pf ppf "(@[<hov 2>@,%a@ ?@ %a@ :@ %a@])" in
      let eval_pp ppf a =
        if UnsizedType.is_eigen_type meta.type_ then
          pf ppf "stan::math::eval(%a)" pp_expr a
        else pf ppf "%a" pp_expr a in
      tform ppf pp_expr ec eval_pp et eval_pp ef
  | Indexed (e, []) -> pp_expr ppf e
  | Indexed (e, idx) -> (
    match e.pattern with
    | FunApp (CompilerInternal (FnReadParam _), _) -> pp_expr ppf e
    | FunApp (CompilerInternal FnReadData, _) ->
        pp_indexed_simple ppf (str "%a" pp_expr e, idx)
    | _
      when List.for_all ~f:dont_need_range_check idx
           && not (UnsizedType.is_indexing_matrix (Expr.Typed.type_of e, idx))
      ->
        pp_indexed_simple ppf (str "%a" pp_expr e, idx)
    | _ -> pp_indexed ppf (str "%a" pp_expr e, idx, pretty_print e) )

(* these functions are just for testing *)
let dummy_locate pattern =
  Expr.(
    Fixed.
      { pattern
      ; meta=
          Typed.Meta.{type_= UInt; adlevel= DataOnly; loc= Location_span.empty}
      })

let pp_unlocated e = str "%a" pp_expr (dummy_locate e)

let%expect_test "pp_expr1" =
  printf "%s" (pp_unlocated (Var "a")) ;
  [%expect {| a |}]

let%expect_test "pp_expr2" =
  printf "%s" (pp_unlocated (Lit (Str, "b"))) ;
  [%expect {| "b" |}]

let%expect_test "pp_expr3" =
  printf "%s" (pp_unlocated (Lit (Int, "112"))) ;
  [%expect {| 112 |}]

let%expect_test "pp_expr4" =
  printf "%s" (pp_unlocated (Lit (Int, "112"))) ;
  [%expect {| 112 |}]

let%expect_test "pp_expr5" =
  printf "%s" (pp_unlocated (FunApp (StanLib ("pi", FnPlain, AoS), []))) ;
  [%expect {| stan::math::pi() |}]

let%expect_test "pp_expr6" =
  printf "%s"
    (pp_unlocated
       (FunApp
          (StanLib ("sqrt", FnPlain, AoS), [dummy_locate (Lit (Int, "123"))]) ) ) ;
  [%expect {| stan::math::sqrt(123) |}]

let%expect_test "pp_expr7" =
  printf "%s"
    (pp_unlocated
       (FunApp
          ( StanLib ("atan", FnPlain, AoS)
          , [dummy_locate (Lit (Int, "123")); dummy_locate (Lit (Real, "1.2"))]
          ) ) ) ;
  [%expect {| stan::math::atan(123, 1.2) |}]

let%expect_test "pp_expr9" =
  printf "%s"
    (pp_unlocated
       (TernaryIf
          ( dummy_locate (Lit (Int, "1"))
          , dummy_locate (Lit (Real, "1.2"))
          , dummy_locate (Lit (Real, "2.3")) ) ) ) ;
  [%expect {| (1 ? 1.2 : 2.3) |}]

let%expect_test "pp_expr10" =
  printf "%s" (pp_unlocated (Indexed (dummy_locate (Var "a"), [All]))) ;
  [%expect {| stan::model::rvalue(a, "a", stan::model::index_omni()) |}]

let%expect_test "pp_expr11" =
  printf "%s"
    (pp_unlocated
       (FunApp
          (UserDefined ("poisson_rng", FnRng), [dummy_locate (Lit (Int, "123"))])
       ) ) ;
  [%expect {| poisson_rng(123, base_rng__, pstream__) |}]
