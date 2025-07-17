(** Lowering of Stan expressions to C++ *)

open Core
open Core.Poly
open Middle
open Cpp

let stan_namespace_qualify f =
  if String.is_suffix ~suffix:"functor__" f || String.contains f ':' then f
  else "stan::math::" ^ f

let fn_renames =
  List.map
    ~f:(fun (k, v) -> (Internal_fun.to_string k, v))
    [ (Internal_fun.FnLength, "stan::math::size")
    ; (FnNegInf, "stan::math::negative_infinity")
    ; (FnResizeToMatch, "stan::math::resize_to_match")
    ; (FnNaN, "std::numeric_limits<double>::quiet_NaN") ]
  @ [ ("lmultiply", "stan::math::multiply_log")
    ; ("lchoose", "stan::math::binomial_coefficient_log")
    ; ("std_normal_qf", "stan::math::inv_Phi")
    ; ("integrate_ode", "stan::math::integrate_ode_rk45")
      (* constraints -- originally internal functions, may be worth renaming now *)
    ; ("cholesky_factor_corr_jacobian", "stan::math::cholesky_corr_constrain")
    ; ("cholesky_factor_corr_constrain", "stan::math::cholesky_corr_constrain")
    ; ("cholesky_factor_corr_unconstrain", "stan::math::cholesky_corr_free")
    ; ("cholesky_factor_cov_jacobian", "stan::math::cholesky_factor_constrain")
    ; ("cholesky_factor_cov_constrain", "stan::math::cholesky_factor_constrain")
    ; ("cholesky_factor_cov_unconstrain", "stan::math::cholesky_factor_free")
    ; ("lower_bound_jacobian", "stan::math::lb_constrain")
    ; ("lower_bound_constrain", "stan::math::lb_constrain")
    ; ("lower_bound_unconstrain", "stan::math::lb_free")
    ; ("upper_bound_jacobian", "stan::math::ub_constrain")
    ; ("upper_bound_constrain", "stan::math::ub_constrain")
    ; ("upper_bound_unconstrain", "stan::math::ub_free")
    ; ("lower_upper_bound_jacobian", "stan::math::lub_constrain")
    ; ("lower_upper_bound_constrain", "stan::math::lub_constrain")
    ; ("lower_upper_bound_unconstrain", "stan::math::lub_free") ]
  |> String.Map.of_alist_exn

let constraint_to_string = function
  | Transformation.Ordered -> Some "ordered"
  | PositiveOrdered -> Some "positive_ordered"
  | Simplex -> Some "simplex"
  | UnitVector -> Some "unit_vector"
  | SumToZero -> Some "sum_to_zero"
  | CholeskyCorr -> Some "cholesky_factor_corr"
  | CholeskyCov -> Some "cholesky_factor_cov"
  | Correlation -> Some "corr_matrix"
  | Covariance -> Some "cov_matrix"
  | Lower _ -> Some "lb"
  | Upper _ -> Some "ub"
  | LowerUpper _ -> Some "lub"
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> Some "offset_multiplier"
  | StochasticRow -> Some "stochastic_row"
  | StochasticColumn -> Some "stochastic_column"
  | Identity -> None
  | TupleTransformation _ ->
      Common.ICE.internal_compiler_error
        [%message
          "Cannot generate tuple transformation directly; should not be called"]

let functor_suffix = "_functor__"
let reduce_sum_functor_suffix = "_rsfunctor__"
let variadic_functor_suffix x = sprintf "_variadic%d_functor__" x

type variadic = FixedArgs | ReduceSum | VariadicHOF of int
[@@deriving compare, hash]

let functor_type hof =
  match Stan_math_signatures.lookup_stan_math_variadic_function hof with
  | Some {required_fn_args; _} -> VariadicHOF (List.length required_fn_args)
  | None when Stan_math_signatures.is_reduce_sum_fn hof -> ReduceSum
  | None -> FixedArgs

let functor_suffix_select = function
  | VariadicHOF n -> variadic_functor_suffix n
  | ReduceSum -> reduce_sum_functor_suffix
  | FixedArgs -> functor_suffix

(* return true if the type of the expression
   is integer, real, or complex (e.g. not a container) *)
let is_scalar e =
  match Expr.Typed.type_of e with UInt | UReal | UComplex -> true | _ -> false

(** Used to determine if [operator/] should be
      mdivide_right() or divide() *)
let is_matrix e =
  match Expr.Typed.type_of e with
  | UMatrix | UComplexMatrix -> true
  | _ -> false

let is_row_vector e =
  match Expr.Typed.type_of e with
  | URowVector | UComplexRowVector -> true
  | _ -> false

let first es = List.nth_exn es 0
let second es = List.nth_exn es 1
let default_multiplier = 1
let default_offset = 0

let transform_args = function
  | Transformation.Offset offset -> [offset; Expr.Helpers.int default_multiplier]
  | Multiplier multiplier -> [Expr.Helpers.int default_offset; multiplier]
  | transform -> Transformation.fold (fun args arg -> args @ [arg]) [] transform

let is_single_index = function Index.Single _ -> true | _ -> false

let dont_need_range_check = function
  | Index.Single Expr.Fixed.{pattern= Var id; _} -> not (Utils.is_user_ident id)
  | _ -> false

let promote_adtype =
  List.fold
    ~f:(fun accum expr ->
      match Expr.Typed.adlevel_of expr with
      | AutoDiffable -> AutoDiffable
      | _ -> accum)
    ~init:UnsizedType.DataOnly

let suffix_args udf = function
  | Fun_kind.FnRng -> ["base_rng__"]
  | FnTarget -> ["lp__"; "lp_accum__"]
  | FnJacobian when udf -> ["lp__"; "lp_accum__"]
  | FnJacobian -> ["lp__"]
  | FnPlain | FnLpdf _ | FnLpmf _ -> []

let rec stantype_prim = function
  | UnsizedType.UInt -> Int
  | UArray t -> stantype_prim t
  | _ -> Double

let templates udf suffix =
  match suffix with
  | Fun_kind.FnLpdf true | FnLpmf true -> [TemplateType "propto__"]
  | FnLpdf false | FnLpmf false -> [TemplateType "false"]
  | FnTarget when udf -> [TemplateType "propto__"]
  | FnJacobian -> [TemplateType "jacobian__"]
  | _ -> []

let deserializer = Var "in__"

let rec local_scalar ut ad =
  match (ut, ad) with
  | UnsizedType.UArray t, _ -> local_scalar t ad
  | _, UnsizedType.DataOnly | UInt, AutoDiffable -> stantype_prim ut
  | _, AutoDiffable -> Types.local_scalar
  | _, TupleAD _ ->
      Common.ICE.internal_compiler_error
        [%message
          "Attempting to make a local scalar tuple"
            (ut : UnsizedType.t)
            (ad : UnsizedType.autodifftype)]

let minus_one e =
  let open Cpp.DSL in
  Parens (e - Literal "1")

let plus_one e =
  let open Cpp.DSL in
  Parens (e + Literal "1")

let rec lower_type ?(mem_pattern = Mem_pattern.AoS) (t : UnsizedType.t)
    (scalar : type_) : type_ =
  match t with
  | UInt -> Int
  | UReal -> scalar
  | UComplex -> Types.complex scalar
  | UArray t -> StdVector (lower_type t scalar)
  | UTuple ts -> Tuple (List.map ~f:(fun t -> lower_type t scalar) ts)
  | UVector -> Types.vector ~mem_pattern scalar
  | URowVector -> Types.row_vector ~mem_pattern scalar
  | UMatrix -> Types.matrix ~mem_pattern scalar
  | UComplexVector -> Types.vector (Types.complex scalar)
  | UComplexRowVector -> Types.row_vector (Types.complex scalar)
  | UComplexMatrix -> Types.matrix (Types.complex scalar)
  | UMathLibraryFunction | UFun _ ->
      Common.ICE.internal_compiler_error
        [%message "Function types not implemented"]

let rec lower_unsizedtype_local ?(mem_pattern = Mem_pattern.AoS) adtype ut =
  match (adtype, ut) with
  | UnsizedType.TupleAD ads, UnsizedType.UTuple ts ->
      Tuple (List.map2_exn ~f:(lower_unsizedtype_local ~mem_pattern) ads ts)
  | UnsizedType.TupleAD _, UnsizedType.UArray t ->
      Types.std_vector (lower_unsizedtype_local ~mem_pattern adtype t)
  | _, UnsizedType.UTuple _ | TupleAD _, _ ->
      Common.ICE.internal_compiler_error
        [%message
          "Tuple and Tuple AD type not matching!"
            (ut : UnsizedType.t)
            (adtype : UnsizedType.autodifftype)]
  | _, _ ->
      let s = local_scalar ut adtype in
      lower_type ~mem_pattern ut s

let rec lower_possibly_var_decl adtype ut mem_pattern =
  let var_decl p_ut = lower_unsizedtype_local ~mem_pattern adtype p_ut in
  match (ut, adtype) with
  | UnsizedType.UArray t, _ ->
      Types.std_vector (lower_possibly_var_decl adtype t mem_pattern)
  | ( ( UMatrix | UVector | URowVector | UComplexRowVector | UComplexVector
      | UComplexMatrix )
    , _ ) ->
      var_decl ut
  | (UReal | UInt | UComplex), _ -> lower_unsizedtype_local adtype ut
  | UTuple t_lst, TupleAD ads ->
      Tuple
        (List.map2_exn
           ~f:(fun ad t -> lower_possibly_var_decl ad t mem_pattern)
           ads t_lst)
  | x, ad ->
      Common.ICE.internal_compiler_error
        [%message
          "Cannot lower" (x : UnsizedType.t) (ad : UnsizedType.autodifftype)]

let rec lower_logical_op op e1 e2 =
  let prim e = Exprs.fun_call "stan::math::primitive_value" [lower_expr e] in
  Parens (BinOp (prim e1, op, prim e2))

and lower_binary_fun f es = Exprs.fun_call f (lower_exprs es)

and vector_literal ?(column = false) scalar es =
  let open Cpp.DSL in
  let vec = if column then Types.vector scalar else Types.row_vector scalar in
  let make_vector size = vec.:{Literal (string_of_int size)} in
  if List.is_empty es then make_vector 0
  else
    let vector = make_vector (List.length es) in
    let values = lower_exprs es in
    (vector << values).@!("finished")

and read_data ut es =
  let val_method =
    match ut with
    | UnsizedType.UArray UInt -> "vals_i"
    | UArray UReal -> "vals_r"
    | UArray UComplex -> "vals_c"
    | UInt | UReal | UComplex | UVector | URowVector | UMatrix | UTuple _
     |UComplexMatrix | UComplexRowVector | UComplexVector | UArray _ | UFun _
     |UMathLibraryFunction ->
        Common.ICE.internal_compiler_error
          [%message "Can't ReadData of " (ut : UnsizedType.t)] in
  let open Cpp.DSL in
  let data_context = Var "context__" in
  data_context.@?(val_method, [lower_expr (List.hd_exn es)])

and lower_binary_op op fn es =
  if is_scalar (first es) && is_scalar (second es) then
    Parens (BinOp (lower_expr (first es), op, lower_expr (second es)))
  else lower_binary_fun fn es

and lower_operator_app op es_in =
  let remove_basic_promotion (e : 'a Expr.Fixed.t) =
    match e.pattern with Promotion (e, _, _) when is_scalar e -> e | _ -> e
  in
  let es =
    if List.for_all es_in ~f:is_scalar then
      List.map ~f:remove_basic_promotion es_in
    else es_in in
  match op with
  | Operator.Plus -> lower_binary_op Add "stan::math::add" es
  | PMinus ->
      if is_scalar (first es) then PMinus (lower_expr (first es))
      else Exprs.fun_call "stan::math::minus" [lower_expr (first es)]
  | PPlus -> lower_expr (first es)
  | Transpose ->
      if is_scalar (first es) then lower_expr (first es)
      else Exprs.fun_call "stan::math::transpose" [lower_expr (first es)]
  | PNot -> Exprs.fun_call "stan::math::logical_negation" [lower_expr (first es)]
  | Minus -> lower_binary_op Subtract "stan::math::subtract" es
  | Times -> lower_binary_op Multiply "stan::math::multiply" es
  | Divide | IntDivide ->
      (* XXX: This conditional is probably a sign that we need to rethink how we store Operators
         in the MIR *)
      if
        is_matrix (second es)
        && (is_matrix (first es) || is_row_vector (first es))
      then lower_binary_fun "stan::math::mdivide_right" es
      else
        let f e = Expr.Typed.type_of e = UInt in
        (* NB: Not stripping promotions due to semantics of int / int *)
        let es' =
          if List.for_all ~f es && not (List.for_all ~f es_in) then es_in
          else es in
        lower_binary_op Divide "stan::math::divide" es'
  | Modulo -> lower_binary_fun "stan::math::modulus" es
  | LDivide -> lower_binary_fun "stan::math::mdivide_left" es
  | And | Or ->
      Common.ICE.internal_compiler_error
        [%message "And/Or should have been converted to an expression"]
  | EltTimes -> lower_binary_op Multiply "stan::math::elt_multiply" es
  | EltDivide -> lower_binary_op Divide "stan::math::elt_divide" es
  | Pow -> lower_binary_fun "stan::math::pow" es
  | EltPow -> lower_binary_fun "stan::math::pow" es
  | Equals -> lower_binary_fun "stan::math::logical_eq" es
  | NEquals -> lower_binary_fun "stan::math::logical_neq" es
  | Less -> lower_binary_fun "stan::math::logical_lt" es
  | Leq -> lower_binary_fun "stan::math::logical_lte" es
  | Greater -> lower_binary_fun "stan::math::logical_gt" es
  | Geq -> lower_binary_fun "stan::math::logical_gte" es

and lower_misc_special_math_app (f : string) (mem_pattern : Mem_pattern.t)
    (ret_type : UnsizedType.returntype option) =
  match f with
  | "target" ->
      Some
        (fun _ ->
          Exprs.fun_call "stan::math::get_lp" [Var "lp__"; Var "lp_accum__"])
  | "rep_matrix" | "rep_vector" | "rep_row_vector" | "append_row" | "append_col"
    when mem_pattern = Mem_pattern.SoA -> (
      let is_autodiffable Expr.Fixed.{meta= Expr.Typed.Meta.{adlevel; _}; _} =
        adlevel = UnsizedType.AutoDiffable in
      match ret_type with
      | Some (UnsizedType.ReturnType t) ->
          Some
            (fun es ->
              match List.exists ~f:is_autodiffable es with
              | true ->
                  Exprs.templated_fun_call (stan_namespace_qualify f)
                    [ lower_possibly_var_decl
                        (UnsizedType.fill_adtype_for_type AutoDiffable t)
                        t mem_pattern ]
                    (lower_exprs es)
              | false ->
                  Exprs.fun_call (stan_namespace_qualify f) (lower_exprs es))
      | Some Void -> None
      | None -> None)
  | _ -> None

and lower_functionals fname suffix es mem_pattern =
  let contains_hof_vars = function
    | {Expr.Fixed.pattern= Var _; meta= {Expr.Typed.Meta.type_= UFun _; _}} ->
        true
    | _ -> false in
  let is_hof_call = List.exists ~f:contains_hof_vars es in
  if not is_hof_call then None
  else
    let lower_hov es =
      let convert_hof_vars = function
        | { Expr.Fixed.pattern= Var name
          ; meta= {Expr.Typed.Meta.type_= UFun _; _} } as e ->
            { e with
              pattern=
                FunApp
                  ( StanLib
                      ( name ^ functor_suffix_select (functor_type fname)
                      , FnPlain
                      , mem_pattern )
                  , [] ) }
        | e -> e in
      let converted_es = List.map ~f:convert_hof_vars es in
      let msgs = "pstream__" |> Middle.Expr.Helpers.variable in
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
            ( Fmt.str "%s<%s%s>" fname normalized_dist_functor propto_template
            , grainsize :: container :: msgs :: tl )
        | _, _
          when Stan_math_signatures.is_stan_math_variadic_function_name fname ->
            let UnsizedType.{control_args; _} =
              Stan_math_signatures.lookup_stan_math_variadic_function fname
              |> Option.value_exn in
            let hd, tl =
              List.split_n converted_es (List.length control_args + 1) in
            (fname, hd @ (msgs :: tl))
        | ( "map_rect"
          , {pattern= Lit (Int, id); _}
            :: {pattern= FunApp ((UserDefined (f, _) | StanLib (f, _, _)), _); _}
            :: tl ) ->
            (Fmt.str "%s<%s, %s>" fname id f, tl @ [msgs])
        | _, args ->
            ( fname
            , args
              @ (suffix_args false suffix
                |> List.map ~f:Middle.Expr.Helpers.variable)
              @ [msgs] ) in
      let fname = stan_namespace_qualify fname in
      let templates = templates false suffix in
      Exprs.templated_fun_call fname templates
        (lower_exprs ~promote_reals:true args) in
    Some lower_hov

and lower_fun_app suffix fname es mem_pattern
    (ret_type : UnsizedType.returntype option) =
  let fname = Option.value (Map.find fn_renames fname) ~default:fname in
  let fname =
    (* Handle systematic renaming of math's constrain and free functions *)
    match String.rsplit2 fname ~on:'_' with
    | Some (f, "jacobian") -> f ^ "_constrain"
    | Some (f, "unconstrain") -> f ^ "_free"
    | _ -> fname in
  let special_options =
    [ Option.map ~f:lower_operator_app (Operator.of_string_opt fname)
    ; lower_misc_special_math_app fname mem_pattern ret_type
    ; lower_functionals fname suffix es mem_pattern ]
    |> List.filter_opt |> List.hd in
  match special_options with
  | Some s -> s es
  | None ->
      let fname = stan_namespace_qualify fname in
      let templates = templates false suffix in
      let extras = suffix_args false suffix |> List.map ~f:Exprs.to_var in
      Exprs.templated_fun_call fname templates (lower_exprs es @ extras)

and lower_user_defined_fun f suffix es =
  let extra_args =
    suffix_args true suffix @ ["pstream__"] |> List.map ~f:Exprs.to_var in
  Exprs.templated_fun_call f (templates true suffix)
    ((lower_exprs ~promote_reals:true) es @ extra_args)

and lower_compiler_internal ad ut f es =
  let open Cpp.DSL in
  let gen_tuple_literal (es : Expr.Typed.t list) : expr =
    (* we make full copies of tuples
       due to a lack of templating sophistication
       in function generation *)
    let is_variable ({pattern; _} : Expr.Typed.t) =
      match pattern with Var _ -> true | _ -> false in
    let types =
      List.map es ~f:(fun ({meta= {adlevel; type_; _}; _} as e) ->
          let base_type = lower_unsizedtype_local adlevel type_ in
          if
            (* avoid trying to reference temporaries like the
               result of adding two matrices *)
            is_variable e
            (* avoid nested tuples as references or passing a
               reference to a pointer-sized type like double *)
            && (not
                  (UnsizedType.is_scalar_type type_
                  || UnsizedType.contains_tuple type_))
            (* Eigen types in the data block are stored as maps
               (but normal Eigen matrices in GQ) *)
            && not
                 (UnsizedType.is_dataonlytype adlevel
                 && UnsizedType.is_eigen_type type_)
          then Types.const_ref base_type
          else base_type) in
    Constructor (Tuple types, lower_exprs es) in
  match f with
  | Internal_fun.FnMakeArray ->
      let ut =
        match ut with
        | UnsizedType.UArray ut -> ut
        | _ ->
            Common.ICE.internal_compiler_error
              [%message
                "Array literal must have array type but found "
                  (ut : UnsizedType.t)] in
      Exprs.std_vector_init_expr
        (lower_unsizedtype_local ad ut)
        (lower_exprs es)
  | FnMakeRowVec -> (
      match ut with
      | UnsizedType.URowVector ->
          let st = local_scalar ut (promote_adtype es) in
          vector_literal st es
      | UMatrix ->
          fun_call "stan::math::to_matrix"
            [ std_vector_init_expr
                (lower_unsizedtype_local ad URowVector)
                (lower_exprs es) ]
      | UComplexRowVector ->
          let st = Types.complex (local_scalar ut (promote_adtype es)) in
          vector_literal st es
      | UComplexMatrix ->
          fun_call "stan::math::to_matrix"
            [ std_vector_init_expr
                (lower_unsizedtype_local ad UComplexRowVector)
                (lower_exprs es) ]
      | _ ->
          Common.ICE.internal_compiler_error
            [%message
              "Unexpected type for row vector literal" (ut : UnsizedType.t)])
  | FnReadData -> read_data ut es
  | FnReadDeserializer ->
      deserializer.@<>(( "read"
                       , [lower_unsizedtype_local AutoDiffable ut]
                       , lower_exprs es ))
  | FnReadParam {constrain; dims; mem_pattern} -> (
      let constrain_opt = constraint_to_string constrain in
      match constrain_opt with
      | None ->
          deserializer.@<>(( "template read"
                           , [ lower_possibly_var_decl AutoDiffable ut
                                 mem_pattern ]
                           , lower_exprs dims ))
      | Some constraint_string ->
          let constraint_args = transform_args constrain in
          let lp =
            Expr.Fixed.{pattern= Var "lp__"; meta= Expr.Typed.Meta.empty} in
          let args = constraint_args @ [lp] @ dims in
          deserializer.@<>(( "template read_constrain_" ^ constraint_string
                           , [ lower_possibly_var_decl AutoDiffable ut
                                 mem_pattern; TemplateType "jacobian__" ]
                           , lower_exprs args )))
  | FnDeepCopy ->
      lower_fun_app Fun_kind.FnPlain "stan::model::deep_copy" es Mem_pattern.AoS
        (Some UnsizedType.Void)
  | FnMakeTuple -> gen_tuple_literal es
  | _ ->
      lower_fun_app FnPlain (Internal_fun.to_string f) es Mem_pattern.AoS
        (Some UnsizedType.Void)

and lower_index = function
  | Index.All -> Exprs.fun_call "stan::model::index_omni" []
  | Single e -> Exprs.fun_call "stan::model::index_uni" [lower_expr e]
  | Upfrom e -> Exprs.fun_call "stan::model::index_min" [lower_expr e]
  | Between (e_low, e_high) ->
      Exprs.fun_call "stan::model::index_min_max"
        [lower_expr e_low; lower_expr e_high]
  | MultiIndex e -> Exprs.fun_call "stan::model::index_multi" [lower_expr e]

and lower_indexed e indices pretty =
  Exprs.fun_call "stan::model::rvalue"
    ([lower_expr e; Exprs.literal_string pretty]
    @ List.map ~f:lower_index indices)

and lower_indexed_simple (e : expr) idcs =
  let idx_minus_one = function
    | Index.Single e -> minus_one e
    | MultiIndex e | Between (e, _) | Upfrom e ->
        Common.ICE.internal_compiler_error
          [%message
            "No non-Single indices allowed"
              (e : expr)
              (idcs : Expr.Typed.t Index.t list)]
    | All ->
        Common.ICE.internal_compiler_error
          [%message
            "No non-Single indices allowed"
              (e : expr)
              (idcs : Expr.Typed.t Index.t list)] in
  List.fold idcs ~init:e ~f:(fun e id ->
      Subscript (e, idx_minus_one (Index.map lower_expr id)))

and lower_expr ?(promote_reals = false)
    (Expr.Fixed.{pattern; meta} : Expr.Typed.t) : Cpp.expr =
  let open Exprs in
  match pattern with
  | Var s -> Var s
  | Lit (Str, s) -> literal_string s
  | Lit (Imaginary, s) ->
      fun_call "stan::math::to_complex" [Literal "0"; Literal s]
  | Lit ((Real | Int), s) -> Literal s
  | Promotion (expr, UReal, _) when is_scalar expr ->
      if promote_reals then
        (* this can be important for e.g. templated function calls
           where we might generate an incorrect specification for int *)
        static_cast Cpp.Double (lower_expr expr)
      else lower_expr expr
  | Promotion (expr, UComplex, DataOnly) when is_scalar expr ->
      (* this is in principle a little better than promote_scalar since it is constexpr *)
      fun_call "stan::math::to_complex" [lower_expr expr; Literal "0"]
  | Promotion (expr, ut, ad) ->
      templated_fun_call "stan::math::promote_scalar"
        [lower_unsizedtype_local ad ut]
        [lower_expr expr]
  | EAnd (e1, e2) -> lower_logical_op And e1 e2
  | EOr (e1, e2) -> lower_logical_op Or e1 e2
  | TernaryIf (ec, et, ef) ->
      let maybe_eval (e : Expr.Typed.t) =
        if UnsizedType.is_eigen_type e.meta.type_ then
          fun_call "stan::math::eval" [lower_expr e]
        else lower_expr e in
      Parens (TernaryIf (maybe_eval ec, maybe_eval et, maybe_eval ef))
  | FunApp
      ( StanLib (op, _, _)
      , [ { meta= {type_= URowVector; _}
          ; pattern= FunApp (CompilerInternal FnMakeRowVec, es) } ] )
    when Operator.(Some Transpose = of_string_opt op) ->
      let st = local_scalar UVector (promote_adtype es) in
      vector_literal ~column:true st es
  | FunApp
      ( StanLib (op, _, _)
      , [ { meta= {type_= UComplexRowVector; _}
          ; pattern= FunApp (CompilerInternal FnMakeRowVec, es) } ] )
    when Operator.(Some Transpose = of_string_opt op) ->
      let st = Types.complex (local_scalar UComplexVector (promote_adtype es)) in
      vector_literal ~column:true st es
  | FunApp (CompilerInternal f, es) ->
      lower_compiler_internal meta.adlevel meta.type_ f es
  | FunApp (StanLib (f, suffix, mem_pattern), es) ->
      let ret_type = Some (UnsizedType.ReturnType meta.type_) in
      lower_fun_app suffix f es mem_pattern ret_type
  | FunApp (UserDefined (f, suffix), es) -> lower_user_defined_fun f suffix es
  | Indexed (e, []) -> lower_expr e
  | Indexed (e, idx) -> (
      match e.pattern with
      | FunApp (CompilerInternal FnReadData, _) ->
          lower_indexed_simple (lower_expr e) idx
      | _
        when List.for_all ~f:dont_need_range_check idx
             && not (UnsizedType.is_indexing_matrix (Expr.Typed.type_of e, idx))
        ->
          lower_indexed_simple (lower_expr e) idx
      | _ -> lower_indexed e idx (Fmt.to_to_string Expr.Typed.pp e))
  | TupleProjection (t, ix) ->
      templated_fun_call "std::get"
        [TypeLiteral (string_of_int (ix - 1))]
        [lower_expr t]

and lower_exprs ?(promote_reals = false) =
  List.map ~f:(lower_expr ~promote_reals)

module Testing = struct
  (* these functions are just for testing *)
  let dummy_locate pattern =
    Expr.(
      Fixed.
        { pattern
        ; meta=
            Typed.Meta.{type_= UInt; adlevel= DataOnly; loc= Location_span.empty}
        })

  let pp_unlocated e =
    Fmt.str "%a" Cpp.Printing.pp_expr (lower_expr @@ dummy_locate e)

  let%expect_test "pp_expr1" =
    printf "%s" (pp_unlocated (Var "a"));
    [%expect {| a |}]

  let%expect_test "pp_expr2" =
    printf "%s" (pp_unlocated (Lit (Str, "b")));
    [%expect {| "b" |}]

  let%expect_test "pp_expr3" =
    printf "%s" (pp_unlocated (Lit (Int, "112")));
    [%expect {| 112 |}]

  let%expect_test "pp_expr4" =
    printf "%s" (pp_unlocated (Lit (Int, "112")));
    [%expect {| 112 |}]

  let%expect_test "pp_expr5" =
    printf "%s" (pp_unlocated (FunApp (StanLib ("pi", FnPlain, AoS), [])));
    [%expect {| stan::math::pi() |}]

  let%expect_test "pp_expr6" =
    printf "%s"
      (pp_unlocated
         (FunApp
            (StanLib ("sqrt", FnPlain, AoS), [dummy_locate (Lit (Int, "123"))])));
    [%expect {| stan::math::sqrt(123) |}]

  let%expect_test "pp_expr7" =
    printf "%s"
      (pp_unlocated
         (FunApp
            ( StanLib ("atan", FnPlain, AoS)
            , [dummy_locate (Lit (Int, "123")); dummy_locate (Lit (Real, "1.2"))]
            )));
    [%expect {| stan::math::atan(123, 1.2) |}]

  let%expect_test "pp_expr9" =
    printf "%s"
      (pp_unlocated
         (TernaryIf
            ( dummy_locate (Lit (Int, "1"))
            , dummy_locate (Lit (Real, "1.2"))
            , dummy_locate (Lit (Real, "2.3")) )));
    [%expect {| (1 ? 1.2 : 2.3) |}]

  let%expect_test "pp_expr10" =
    printf "%s" (pp_unlocated (Indexed (dummy_locate (Var "a"), [All])));
    [%expect {| stan::model::rvalue(a, "a", stan::model::index_omni()) |}]

  let%expect_test "pp_expr11" =
    printf "%s"
      (pp_unlocated
         (FunApp
            ( UserDefined ("poisson_rng", FnRng)
            , [dummy_locate (Lit (Int, "123"))] )));
    [%expect {| poisson_rng(123, base_rng__, pstream__) |}]

  let%expect_test "pp_expr12" =
    printf "%s\n"
      (Fmt.str "%a" Cpp.Printing.pp_expr (vector_literal Cpp.Double []));
    printf "%s"
      (Fmt.str "%a" Cpp.Printing.pp_expr
         (vector_literal ~column:true Cpp.Double []));
    [%expect
      {|
      Eigen::Matrix<double,1,-1>(0)
      Eigen::Matrix<double,-1,1>(0) |}]
end
