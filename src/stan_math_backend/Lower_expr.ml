(** Lowering of Stan expressions to C++ *)

open Core_kernel
open Core_kernel.Poly
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
    ; ("std_normal_qf", "stan::math::inv_Phi") ]
  |> String.Map.of_alist_exn

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

let map_rect_calls = Int.Table.create ()
let functor_suffix = "_functor__"
let reduce_sum_functor_suffix = "_rsfunctor__"
let variadic_functor_suffix x = sprintf "_variadic%d_functor__" x

let functor_suffix_select hof =
  match Hashtbl.find Stan_math_signatures.stan_math_variadic_signatures hof with
  | Some {required_fn_args; _} ->
      variadic_functor_suffix (List.length required_fn_args)
  | None when Stan_math_signatures.is_reduce_sum_fn hof ->
      reduce_sum_functor_suffix
  | None -> functor_suffix

(* retun true if the type of the expression
   is integer, real, or complex (e.g. not a container) *)
let is_scalar e =
  match Expr.Typed.type_of e with UInt | UReal | UComplex -> true | _ -> false

let is_matrix e = Expr.Typed.type_of e = UMatrix
let is_row_vector e = Expr.Typed.type_of e = URowVector
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
      | _ -> accum )
    ~init:UnsizedType.DataOnly

let suffix_args = function
  | Fun_kind.FnRng -> ["base_rng__"]
  | FnTarget -> ["lp__"; "lp_accum__"]
  | FnPlain | FnLpdf _ -> []

let rec stantype_prim = function
  | UnsizedType.UInt -> Int
  | UArray t -> stantype_prim t
  | _ -> Double

let templates udf suffix =
  match suffix with
  | Fun_kind.FnLpdf true -> [TemplateType "propto__"]
  | FnLpdf false -> [TemplateType "false"]
  | FnTarget when udf -> [TemplateType "propto__"]
  | _ -> []

let serializer_in = Var "in__"

let rec local_scalar ut ad =
  match (ut, ad) with
  | UnsizedType.UArray t, _ -> local_scalar t ad
  | _, UnsizedType.DataOnly | UInt, AutoDiffable -> stantype_prim ut
  | _, AutoDiffable -> Types.local_scalar

let minus_one e =
  let open Expression_syntax in
  Parens (e - Literal "1")

let plus_one e =
  let open Expression_syntax in
  Parens (e + Literal "1")

let to_mir_var s = Expr.{Fixed.pattern= Var s; meta= Typed.Meta.empty}

let rec lower_type (t : UnsizedType.t) (scalar : type_) : type_ =
  match t with
  | UInt -> Int
  | UReal -> scalar
  | UComplex -> Types.complex scalar
  | UArray t -> StdVector (lower_type t scalar)
  | UVector -> Types.vector scalar
  | URowVector -> Types.row_vector scalar
  | UMatrix -> Types.matrix scalar
  | UComplexVector -> Types.vector (Types.complex scalar)
  | UComplexRowVector -> Types.row_vector (Types.complex scalar)
  | UComplexMatrix -> Types.matrix (Types.complex scalar)
  | UMathLibraryFunction | UFun _ ->
      Common.FatalError.fatal_error_msg
        [%message "Function types not implemented"]

let lower_type_eigen_expr (t : UnsizedType.t) (scalar : type_) : type_ =
  match t with
  | UInt -> Int
  | UReal | UMatrix | URowVector | UVector | UComplexVector | UComplexMatrix
   |UComplexRowVector ->
      scalar
  | UComplex -> Types.complex scalar
  | UArray t ->
      (* Expressions are not accepted for arrays of Eigen::Matrix *)
      StdVector (lower_type t scalar)
  | UMathLibraryFunction | UFun _ ->
      Common.FatalError.fatal_error_msg
        [%message "Function types not implemented"]

let lower_unsizedtype_local adtype ut =
  let s = local_scalar ut adtype in
  lower_type ut s

let lower_expr_type e =
  Expr.Typed.(lower_unsizedtype_local (adlevel_of e) (type_of e))

let rec lower_possibly_var_decl adtype ut mem_pattern =
  let scalar = local_scalar ut adtype in
  let var_decl p_ut =
    if mem_pattern = Mem_pattern.SoA && adtype = UnsizedType.AutoDiffable then
      TypeTrait
        ( "stan::conditional_var_value_t"
        , [scalar; lower_unsizedtype_local adtype p_ut] )
    else lower_unsizedtype_local adtype p_ut in
  match ut with
  | UArray t -> Types.std_vector (lower_possibly_var_decl adtype t mem_pattern)
  | UMatrix | UVector | URowVector | UComplexRowVector | UComplexVector
   |UComplexMatrix ->
      var_decl ut
  | UReal | UInt | UComplex -> lower_unsizedtype_local adtype ut
  | x -> raise_s [%message (x : UnsizedType.t) "not implemented yet"]

let rec lower_logical_op op e1 e2 =
  let prim e = Exprs.fun_call "stan::math::primitive_value" [lower_expr e] in
  Parens (BinOp (prim e1, op, prim e2))

and lower_binop op es =
  Parens (BinOp (lower_expr (first es), op, lower_expr (second es)))

and lower_binary_fun f es = Exprs.fun_call f (lower_exprs (List.take es 2))

and vector_literal ?(column = false) scalar es =
  let open Expression_syntax in
  let fn = if column then Types.vector else Types.row_vector in
  let constructor size =
    Constructor (fn scalar, [Literal (string_of_int size)]) in
  if List.is_empty es then constructor 0
  else
    let vector = constructor (List.length es) in
    let values = lower_exprs es in
    (vector << values).@!("finished")

and read_data ut es =
  let val_method =
    match ut with
    | UnsizedType.UArray UInt -> "vals_i"
    | UArray UReal -> "vals_r"
    | UArray UComplex -> "vals_c"
    | UInt | UReal | UComplex | UVector | URowVector | UMatrix
     |UComplexMatrix | UComplexRowVector | UComplexVector | UArray _ | UFun _
     |UMathLibraryFunction ->
        Common.FatalError.fatal_error_msg
          [%message "Can't ReadData of " (ut : UnsizedType.t)] in
  let open Expression_syntax in
  let data_context = Var "context__" in
  data_context.@?((val_method, [lower_expr (List.hd_exn es)]))

and lower_scalar_binary op fn es =
  if is_scalar (first es) && is_scalar (second es) then lower_binop op es
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
  | Operator.Plus -> lower_scalar_binary Add "stan::math::add" es
  | PMinus ->
      if is_scalar (first es) then PMinus (lower_expr (first es))
      else Exprs.fun_call "stan::math::minus" [lower_expr (first es)]
  | PPlus -> lower_expr (first es)
  | Transpose ->
      if is_scalar (first es) then lower_expr (first es)
      else Exprs.fun_call "stan::math::transpose" [lower_expr (first es)]
  | PNot -> Exprs.fun_call "stan::math::logical_negation" [lower_expr (first es)]
  | Minus -> lower_scalar_binary Subtract "stan::math::subtract" es
  | Times -> lower_scalar_binary Multiply "stan::math::multiply" es
  | Divide | IntDivide ->
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
        lower_scalar_binary Divide "stan::math::divide" es'
  | Modulo -> lower_binary_fun "stan::math::modulus" es
  | LDivide -> lower_binary_fun "stan::math::mdivide_left" es
  | And | Or ->
      Common.FatalError.fatal_error_msg
        [%message "And/Or should have been converted to an expression"]
  | EltTimes -> lower_scalar_binary Multiply "stan::math::elt_multiply" es
  | EltDivide -> lower_scalar_binary Divide "stan::math::elt_divide" es
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
  | "target" | "get_lp" ->
      Some
        (fun _ ->
          Exprs.fun_call "stan::math::get_lp" [Var "lp__"; Var "lp_accum__"] )
  | f when Map.mem fn_renames f ->
      Some
        (fun es -> Exprs.fun_call (Map.find_exn fn_renames f) (lower_exprs es))
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
                    [lower_possibly_var_decl AutoDiffable t mem_pattern]
                    (lower_exprs es)
              | false ->
                  Exprs.fun_call (stan_namespace_qualify f) (lower_exprs es) )
      | Some Void -> None
      | None -> None )
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
                      (name ^ functor_suffix_select fname, FnPlain, mem_pattern)
                  , [] ) }
        | e -> e in
      let converted_es = List.map ~f:convert_hof_vars es in
      let msgs = "pstream__" |> to_mir_var in
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
            let Stan_math_signatures.{control_args; _} =
              Hashtbl.find_exn
                Stan_math_signatures.stan_math_variadic_signatures fname in
            let hd, tl =
              List.split_n converted_es (List.length control_args + 1) in
            (fname, hd @ (msgs :: tl))
        | ( "map_rect"
          , {pattern= FunApp ((UserDefined (f, _) | StanLib (f, _, _)), _); _}
            :: tl ) ->
            let next_map_rect_id = Hashtbl.length map_rect_calls + 1 in
            Hashtbl.add_exn map_rect_calls ~key:next_map_rect_id ~data:f ;
            (Fmt.str "%s<%d, %s>" fname next_map_rect_id f, tl @ [msgs])
        | _, args -> (fname, args @ [msgs]) in
      let fname = stan_namespace_qualify fname in
      let templates = templates false suffix in
      Exprs.templated_fun_call fname templates (lower_exprs args) in
    Some lower_hov

and lower_fun_app suffix fname es mem_pattern
    (ret_type : UnsizedType.returntype option) =
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
      let extras = suffix_args suffix |> List.map ~f:Exprs.to_var in
      Exprs.templated_fun_call fname templates (lower_exprs es @ extras)

and lower_user_defined_fun f suffix es =
  let extra_args =
    suffix_args suffix @ ["pstream__"] |> List.map ~f:Exprs.to_var in
  Exprs.templated_fun_call f (templates true suffix)
    (lower_exprs es @ extra_args)

and lower_compiler_internal ad ut f es =
  let open Expression_syntax in
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
        Common.FatalError.fatal_error_msg
          [%message
            "Unexpected type for row vector literal" (ut : UnsizedType.t)] )
  | FnReadData -> read_data ut es
  | FnReadDataSerializer ->
      serializer_in.@<>(( "read"
                        , [lower_unsizedtype_local AutoDiffable UReal]
                        , [] ))
  | FnReadParam {constrain; dims; mem_pattern} -> (
      let constrain_opt = constraint_to_string constrain in
      match constrain_opt with
      | None ->
          serializer_in.@<>(( "template read"
                            , [ lower_possibly_var_decl AutoDiffable ut
                                  mem_pattern ]
                            , lower_exprs dims ))
      | Some constraint_string ->
          let constraint_args = transform_args constrain in
          let lp =
            Expr.Fixed.{pattern= Var "lp__"; meta= Expr.Typed.Meta.empty} in
          let args = constraint_args @ [lp] @ dims in
          serializer_in.@<>(( "template read_constrain_" ^ constraint_string
                            , [ lower_possibly_var_decl AutoDiffable ut
                                  mem_pattern; TemplateType "jacobian__" ]
                            , lower_exprs args )) )
  | FnDeepCopy ->
      lower_fun_app Fun_kind.FnPlain "stan::model::deep_copy" es Mem_pattern.AoS
        (Some UnsizedType.Void)
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
    ( [lower_expr e; Exprs.literal_string pretty]
    @ List.map ~f:lower_index indices )

and lower_indexed_simple e idcs =
  let idx_minus_one = function
    | Index.Single e -> minus_one (lower_expr e)
    | MultiIndex e | Between (e, _) | Upfrom e ->
        Common.FatalError.fatal_error_msg
          [%message
            "No non-Single indices allowed"
              (e : Expr.Typed.t)
              (idcs : Expr.Typed.t Index.t list)
              (Expr.Typed.loc_of e : Location_span.t)]
    | All ->
        Common.FatalError.fatal_error_msg
          [%message
            "No non-Single indices allowed"
              (e : Expr.Typed.t)
              (idcs : Expr.Typed.t Index.t list)] in
  List.fold idcs ~init:(lower_expr e) ~f:(fun e id ->
      Index (e, idx_minus_one id) )

and lower_expr (Expr.Fixed.{pattern; meta} : Expr.Typed.t) : Cpp.expr =
  let open Exprs in
  match pattern with
  | Var s -> Var s
  | Lit (Str, s) -> literal_string s
  | Lit (Imaginary, s) ->
      fun_call "stan::math::to_complex" [Literal "0"; Literal s]
  | Lit ((Real | Int), s) -> Literal s
  | Promotion (expr, UReal, _) when is_scalar expr -> lower_expr expr
  | Promotion (expr, UComplex, DataOnly) when is_scalar expr ->
      (* this is in principle a little better than promote_scalar since it is constexpr *)
      fun_call "stan::math::to_complex" [lower_expr expr; Literal "0"]
  | Promotion (expr, ut, ad) ->
      templated_fun_call "stan::math::promote_scalar"
        [lower_unsizedtype_local ad ut]
        [lower_expr expr]
  | EAnd (e1, e2) -> lower_logical_op And e1 e2
  | EOr (e1, e2) -> lower_logical_op Or e1 e2
  | Indexed (e, []) -> lower_expr e
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
  | Indexed (e, idx) -> (
    match e.pattern with
    | FunApp (CompilerInternal (FnReadParam _), _) -> lower_expr e
    | FunApp (CompilerInternal FnReadData, _) -> lower_indexed_simple e idx
    | _
      when List.for_all ~f:dont_need_range_check idx
           && not (UnsizedType.is_indexing_matrix (Expr.Typed.type_of e, idx))
      ->
        lower_indexed_simple e idx
    | _ -> lower_indexed e idx (Fmt.to_to_string Expr.Typed.pp e) )

and lower_exprs = List.map ~f:lower_expr

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
            (StanLib ("sqrt", FnPlain, AoS), [dummy_locate (Lit (Int, "123"))])
         ) ) ;
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
            ( UserDefined ("poisson_rng", FnRng)
            , [dummy_locate (Lit (Int, "123"))] ) ) ) ;
    [%expect {| poisson_rng(123, base_rng__, pstream__) |}]
end
