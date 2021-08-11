(** The signatures of the Stan Math library, which are used for type checking *)
open Core_kernel

(* The "dimensionality" (bad name?) is supposed to help us represent the
    vectorized nature of many Stan functions. It allows us to represent when
    a function argument can be just a real or matrix, or some common forms of
    vectorization over reals. This captures the most commonly used forms in our
    previous signatures; there are a lot partially because we had a lot of
    inconsistencies.
*)
type dimensionality =
  | DReal
  | DVector
  | DMatrix
  (* Vectorizable int *)
  | DVInt
  (* Vectorizable real *)
  | DVReal
  (* DEPRECATED; vectorizable ints or reals *)
  | DIntAndReals
  (* Vectorizable vectors - for multivariate functions *)
  | DVectors
  | DDeepVectorized

(* all base types with up 8 levels of nested containers -
                       just used for element-wise vectorized unary functions now *)

let rec bare_array_type (t, i) =
  match i with 0 -> t | j -> UnsizedType.UArray (bare_array_type (t, j - 1))

let rec expand_arg = function
  | DReal -> [UnsizedType.UReal]
  | DVector -> [UVector]
  | DMatrix -> [UMatrix]
  | DVInt -> [UInt; UArray UInt]
  | DVReal -> [UReal; UArray UReal; UVector; URowVector]
  | DIntAndReals -> expand_arg DVReal @ expand_arg DVInt
  | DVectors -> [UVector; UArray UVector; URowVector; UArray URowVector]
  | DDeepVectorized ->
      let all_base = [UnsizedType.UInt; UReal; URowVector; UVector; UMatrix] in
      List.(
        concat_map all_base ~f:(fun a ->
            map (range 0 8) ~f:(fun i -> bare_array_type (a, i)) ))

type fkind = Lpmf | Lpdf | Rng | Cdf | Ccdf | UnaryVectorized

let is_primitive = function
  | UnsizedType.UReal -> true
  | UInt -> true
  | _ -> false

(** The signatures hash table *)
let stan_math_signatures = String.Table.create ()

(** All of the signatures that are added by hand, rather than the ones
    added "declaratively" *)
let manual_stan_math_signatures = String.Table.create ()

(* XXX The correct word here isn't combination - what is it? *)
let all_combinations xx =
  List.fold_right xx ~init:[[]] ~f:(fun x accum ->
      List.concat_map accum ~f:(fun acc ->
          List.map ~f:(fun arg -> arg :: acc) x ) )

let%expect_test "combinations " =
  let a = all_combinations [[1; 2]; [3; 4]; [5; 6]] in
  [%sexp (a : int list list)] |> Sexp.to_string_hum |> print_endline ;
  [%expect
    {| ((1 3 5) (2 3 5) (1 4 5) (2 4 5) (1 3 6) (2 3 6) (1 4 6) (2 4 6)) |}]

let missing_math_functions = String.Set.of_list ["beta_proportion_cdf"]

let rng_return_type t lt =
  if List.for_all ~f:is_primitive lt then t else UnsizedType.UArray t

let add_unqualified (name, rt, uqargts) =
  Hashtbl.add_multi manual_stan_math_signatures ~key:name
    ~data:(rt, List.map ~f:(fun x -> (UnsizedType.AutoDiffable, x)) uqargts)

let rec ints_to_real = function
  | UnsizedType.UInt -> UnsizedType.UReal
  | UArray t -> UArray (ints_to_real t)
  | x -> x

let reduce_sum_allowed_dimensionalities = [1; 2; 3; 4; 5; 6; 7]

let reduce_sum_slice_types =
  let base_slice_type i =
    [ bare_array_type (UnsizedType.UReal, i)
    ; bare_array_type (UnsizedType.UInt, i)
    ; bare_array_type (UnsizedType.UMatrix, i)
    ; bare_array_type (UnsizedType.UVector, i)
    ; bare_array_type (UnsizedType.URowVector, i) ]
  in
  List.concat (List.map ~f:base_slice_type reduce_sum_allowed_dimensionalities)

(* Variadic ODE *)
let variadic_ode_adjoint_ctl_tol_arg_types =
  [ (UnsizedType.DataOnly, UnsizedType.UReal)
    (* real relative_tolerance_forward *)
  ; (DataOnly, UVector) (* vector absolute_tolerance_forward *)
  ; (DataOnly, UReal) (* real relative_tolerance_backward *)
  ; (DataOnly, UVector) (* real absolute_tolerance_backward *)
  ; (DataOnly, UReal) (* real relative_tolerance_quadrature *)
  ; (DataOnly, UReal) (* real absolute_tolerance_quadrature *)
  ; (DataOnly, UInt) (* int max_num_steps *)
  ; (DataOnly, UInt) (* int num_steps_between_checkpoints *)
  ; (DataOnly, UInt) (* int interpolation_polynomial *)
  ; (DataOnly, UInt) (* int solver_forward *)
  ; (DataOnly, UInt)
  (* int solver_backward *)
   ]

let variadic_ode_tol_arg_types =
  [ (UnsizedType.DataOnly, UnsizedType.UReal)
  ; (DataOnly, UReal); (DataOnly, UInt) ]

let variadic_ode_mandatory_arg_types =
  [ (UnsizedType.AutoDiffable, UnsizedType.UVector)
  ; (AutoDiffable, UReal)
  ; (AutoDiffable, UArray UReal) ]

let variadic_ode_mandatory_fun_args =
  [ (UnsizedType.AutoDiffable, UnsizedType.UReal)
  ; (UnsizedType.AutoDiffable, UnsizedType.UVector) ]

let variadic_ode_fun_return_type = UnsizedType.UVector
let variadic_ode_return_type = UnsizedType.UArray UnsizedType.UVector

let mk_declarative_sig (fnkinds, name, args) =
  let sfxes = function
    | Lpmf -> ["_lpmf"; "_log"]
    | Lpdf -> ["_lpdf"; "_log"]
    | Rng -> ["_rng"]
    | Cdf -> ["_cdf"; "_cdf_log"; "_lcdf"]
    | Ccdf -> ["_ccdf_log"; "_lccdf"]
    | UnaryVectorized -> [""]
  in
  let add_ints = function DVReal -> DIntAndReals | x -> x in
  let all_expanded args = all_combinations (List.map ~f:expand_arg args) in
  let promoted_dim = function
    | DVInt -> UnsizedType.UInt
    (* XXX fix this up to work with more RNGs *)
    | _ -> UReal
  in
  let find_rt rt args = function
    | Rng -> UnsizedType.ReturnType (rng_return_type rt args)
    | UnaryVectorized -> ReturnType (ints_to_real (List.hd_exn args))
    | _ -> ReturnType UReal
  in
  let create_from_fk_args fk arglists =
    List.concat_map arglists ~f:(fun args ->
        List.map (sfxes fk) ~f:(fun sfx ->
            (name ^ sfx, find_rt UReal args fk, args) ) )
  in
  let add_fnkind = function
    | Rng ->
        let rt, args = (List.hd_exn args, List.tl_exn args) in
        let args = List.map ~f:add_ints args in
        let rt = promoted_dim rt in
        let name = name ^ "_rng" in
        List.map (all_expanded args) ~f:(fun args ->
            (name, find_rt rt args Rng, args) )
    | UnaryVectorized ->
        create_from_fk_args UnaryVectorized (all_expanded args)
    | fk -> create_from_fk_args fk (all_expanded args)
  in
  List.concat_map fnkinds ~f:add_fnkind
  |> List.filter ~f:(fun (n, _, _) -> not (Set.mem missing_math_functions n))
  |> List.map ~f:(fun (n, rt, args) ->
         (n, rt, List.map ~f:(fun x -> (UnsizedType.AutoDiffable, x)) args) )

let full_lpdf = [Lpdf; Rng; Ccdf; Cdf]
let full_lpmf = [Lpmf; Rng; Ccdf; Cdf]

let reduce_sum_functions =
  String.Set.of_list ["reduce_sum"; "reduce_sum_static"]

let variadic_ode_adjoint_fn = "ode_adjoint_tol_ctl"

let variadic_ode_nonadjoint_fns =
  String.Set.of_list
    [ "ode_bdf_tol"; "ode_rk45_tol"; "ode_adams_tol"; "ode_bdf"; "ode_rk45"
    ; "ode_adams"; "ode_ckrk"; "ode_ckrk_tol" ]

let ode_tolerances_suffix = "_tol"
let is_reduce_sum_fn f = Set.mem reduce_sum_functions f
let is_variadic_ode_nonadjoint_fn f = Set.mem variadic_ode_nonadjoint_fns f

let is_variadic_ode_fn f =
  Set.mem variadic_ode_nonadjoint_fns f || f = variadic_ode_adjoint_fn

let is_variadic_ode_nonadjoint_tol_fn f =
  is_variadic_ode_nonadjoint_fn f
  && String.is_suffix f ~suffix:ode_tolerances_suffix

let distributions =
  [ (full_lpmf, "beta_binomial", [DVInt; DVInt; DVReal; DVReal])
  ; (full_lpdf, "beta", [DVReal; DVReal; DVReal])
  ; ([Lpdf; Ccdf; Cdf], "beta_proportion", [DVReal; DVReal; DIntAndReals])
  ; (full_lpmf, "bernoulli", [DVInt; DVReal])
  ; ([Lpmf; Rng], "bernoulli_logit", [DVInt; DVReal])
  ; (full_lpmf, "binomial", [DVInt; DVInt; DVReal])
  ; ([Lpmf], "binomial_logit", [DVInt; DVInt; DVReal])
  ; ([Lpmf], "categorical", [DVInt; DVector])
  ; ([Lpmf], "categorical_logit", [DVInt; DVector])
  ; (full_lpdf, "cauchy", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "chi_square", [DVReal; DVReal])
  ; ([Lpdf], "dirichlet", [DVectors; DVectors])
  ; (full_lpmf, "discrete_range", [DVInt; DVInt; DVInt])
  ; (full_lpdf, "double_exponential", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "exp_mod_normal", [DVReal; DVReal; DVReal; DVReal])
  ; (full_lpdf, "exponential", [DVReal; DVReal])
  ; (full_lpdf, "frechet", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "gamma", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "gumbel", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "inv_chi_square", [DVReal; DVReal])
  ; (full_lpdf, "inv_gamma", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "logistic", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "lognormal", [DVReal; DVReal; DVReal])
  ; ([Lpdf], "multi_gp", [DMatrix; DMatrix; DVector])
  ; ([Lpdf], "multi_gp_cholesky", [DMatrix; DMatrix; DVector])
  ; ([Lpdf], "multi_normal", [DVectors; DVectors; DMatrix])
  ; ([Lpdf], "multi_normal_cholesky", [DVectors; DVectors; DMatrix])
  ; ([Lpdf], "multi_normal_prec", [DVectors; DVectors; DMatrix])
  ; ([Lpdf], "multi_student_t", [DVectors; DReal; DVectors; DMatrix])
  ; (full_lpmf, "neg_binomial", [DVInt; DVReal; DVReal])
  ; (full_lpmf, "neg_binomial_2", [DVInt; DVReal; DVReal])
  ; ([Lpmf; Rng], "neg_binomial_2_log", [DVInt; DVReal; DVReal])
  ; (full_lpdf, "normal", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "pareto", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "pareto_type_2", [DVReal; DVReal; DVReal; DVReal])
  ; (full_lpmf, "poisson", [DVInt; DVReal])
  ; ([Lpmf; Rng], "poisson_log", [DVInt; DVReal])
  ; (full_lpdf, "rayleigh", [DVReal; DVReal])
  ; (full_lpdf, "scaled_inv_chi_square", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "skew_normal", [DVReal; DVReal; DVReal; DVReal])
  ; (full_lpdf, "student_t", [DVReal; DVReal; DVReal; DVReal])
  ; (full_lpdf, "std_normal", [DVReal])
  ; (full_lpdf, "uniform", [DVReal; DVReal; DVReal])
  ; ([Lpdf; Rng], "von_mises", [DVReal; DVReal; DVReal])
  ; (full_lpdf, "weibull", [DVReal; DVReal; DVReal])
  ; ([Lpdf], "wiener", [DVReal; DVReal; DVReal; DVReal; DVReal])
  ; ([Lpdf], "wishart", [DMatrix; DReal; DMatrix]) ]

let math_sigs =
  [ ([UnaryVectorized], "acos", [DDeepVectorized])
  ; ([UnaryVectorized], "acosh", [DDeepVectorized])
  ; ([UnaryVectorized], "asin", [DDeepVectorized])
  ; ([UnaryVectorized], "asinh", [DDeepVectorized])
  ; ([UnaryVectorized], "atan", [DDeepVectorized])
  ; ([UnaryVectorized], "atanh", [DDeepVectorized])
  ; ([UnaryVectorized], "cbrt", [DDeepVectorized])
  ; ([UnaryVectorized], "ceil", [DDeepVectorized])
  ; ([UnaryVectorized], "cos", [DDeepVectorized])
  ; ([UnaryVectorized], "cosh", [DDeepVectorized])
  ; ([UnaryVectorized], "digamma", [DDeepVectorized])
  ; ([UnaryVectorized], "erf", [DDeepVectorized])
  ; ([UnaryVectorized], "erfc", [DDeepVectorized])
  ; ([UnaryVectorized], "exp", [DDeepVectorized])
  ; ([UnaryVectorized], "exp2", [DDeepVectorized])
  ; ([UnaryVectorized], "expm1", [DDeepVectorized])
  ; ([UnaryVectorized], "fabs", [DDeepVectorized])
  ; ([UnaryVectorized], "floor", [DDeepVectorized])
  ; ([UnaryVectorized], "inv", [DDeepVectorized])
  ; ([UnaryVectorized], "inv_cloglog", [DDeepVectorized])
  ; ([UnaryVectorized], "inv_logit", [DDeepVectorized])
  ; ([UnaryVectorized], "inv_Phi", [DDeepVectorized])
  ; ([UnaryVectorized], "inv_sqrt", [DDeepVectorized])
  ; ([UnaryVectorized], "inv_square", [DDeepVectorized])
  ; ([UnaryVectorized], "lambert_w0", [DDeepVectorized])
  ; ([UnaryVectorized], "lambert_wm1", [DDeepVectorized])
  ; ([UnaryVectorized], "lgamma", [DDeepVectorized])
  ; ([UnaryVectorized], "log", [DDeepVectorized])
  ; ([UnaryVectorized], "log10", [DDeepVectorized])
  ; ([UnaryVectorized], "log1m", [DDeepVectorized])
  ; ([UnaryVectorized], "log1m_exp", [DDeepVectorized])
  ; ([UnaryVectorized], "log1m_inv_logit", [DDeepVectorized])
  ; ([UnaryVectorized], "log1p", [DDeepVectorized])
  ; ([UnaryVectorized], "log1p_exp", [DDeepVectorized])
  ; ([UnaryVectorized], "log2", [DDeepVectorized])
  ; ([UnaryVectorized], "log_inv_logit", [DDeepVectorized])
  ; ([UnaryVectorized], "logit", [DDeepVectorized])
  ; ([UnaryVectorized], "Phi", [DDeepVectorized])
  ; ([UnaryVectorized], "Phi_approx", [DDeepVectorized])
  ; ([UnaryVectorized], "round", [DDeepVectorized])
  ; ([UnaryVectorized], "sin", [DDeepVectorized])
  ; ([UnaryVectorized], "sinh", [DDeepVectorized])
  ; ([UnaryVectorized], "sqrt", [DDeepVectorized])
  ; ([UnaryVectorized], "square", [DDeepVectorized])
  ; ([UnaryVectorized], "step", [DReal])
  ; ([UnaryVectorized], "tan", [DDeepVectorized])
  ; ([UnaryVectorized], "tanh", [DDeepVectorized])
    (* ; add_nullary ("target") *)
  ; ([UnaryVectorized], "tgamma", [DDeepVectorized])
  ; ([UnaryVectorized], "trunc", [DDeepVectorized])
  ; ([UnaryVectorized], "trigamma", [DDeepVectorized]) ]

let all_declarative_sigs = distributions @ math_sigs

let declarative_fnsigs =
  List.concat_map ~f:mk_declarative_sig all_declarative_sigs

(* -- Querying stan_math_signatures -- *)
let stan_math_returntype name args =
  let name = Utils.stdlib_distribution_name name in
  let namematches = Hashtbl.find_multi stan_math_signatures name in
  let filteredmatches =
    List.filter
      ~f:(fun x ->
        UnsizedType.check_compatible_arguments_mod_conv name (snd x) args )
      namematches
  in
  match name with
  | x when is_reduce_sum_fn x -> Some (UnsizedType.ReturnType UReal)
  | x when is_variadic_ode_fn x ->
      Some (UnsizedType.ReturnType (UArray UVector))
  | _ ->
      if List.length filteredmatches = 0 then None
        (* Return the least return type in case there are multiple options (due to implicit UInt-UReal conversion), where UInt<UReal *)
      else
        Some
          (List.hd_exn
             (List.sort ~compare:UnsizedType.compare_returntype
                (List.map ~f:fst filteredmatches)))

let is_stan_math_function_name name =
  let name = Utils.stdlib_distribution_name name in
  Hashtbl.mem stan_math_signatures name

let dist_name_suffix udf_names name =
  let is_udf_name s = List.exists ~f:(fun (n, _) -> n = s) udf_names in
  match
    Utils.distribution_suffices
    |> List.filter ~f:(fun sfx ->
           is_stan_math_function_name (name ^ sfx) || is_udf_name (name ^ sfx)
       )
    |> List.hd
  with
  | Some hd -> hd
  | None -> raise_s [%message "Couldn't find distribution " name]

let operator_to_stan_math_fns = function
  | Operator.Plus -> ["add"]
  | PPlus -> ["plus"]
  | Minus -> ["subtract"]
  | PMinus -> ["minus"]
  | Times -> ["multiply"]
  | Divide -> ["mdivide_right"; "divide"]
  | Modulo -> ["modulus"]
  | IntDivide -> []
  | LDivide -> ["mdivide_left"]
  | EltTimes -> ["elt_multiply"]
  | EltDivide -> ["elt_divide"]
  | Pow -> ["pow"]
  | EltPow -> ["pow"]
  | Or -> ["logical_or"]
  | And -> ["logical_and"]
  | Equals -> ["logical_eq"]
  | NEquals -> ["logical_neq"]
  | Less -> ["logical_lt"]
  | Leq -> ["logical_lte"]
  | Greater -> ["logical_gt"]
  | Geq -> ["logical_gte"]
  | PNot -> ["logical_negation"]
  | Transpose -> ["transpose"]

let int_divide_type =
  UnsizedType.(ReturnType UInt, [(AutoDiffable, UInt); (AutoDiffable, UInt)])

let operator_stan_math_return_type op arg_tys =
  match (op, arg_tys) with
  | Operator.IntDivide, [(_, UnsizedType.UInt); (_, UInt)] ->
      Some UnsizedType.(ReturnType UInt)
  | IntDivide, _ -> None
  | _ ->
      operator_to_stan_math_fns op
      |> List.filter_map ~f:(fun name -> stan_math_returntype name arg_tys)
      |> List.hd

let assignmentoperator_stan_math_return_type assop arg_tys =
  ( match assop with
  | Operator.Divide -> stan_math_returntype "divide" arg_tys
  | Plus | Minus | Times | EltTimes | EltDivide ->
      operator_stan_math_return_type assop arg_tys
  | _ -> None )
  |> Option.bind ~f:(function
       | ReturnType rtype
         when rtype = snd (List.hd_exn arg_tys)
              && not
                   ( (assop = Operator.EltTimes || assop = Operator.EltDivide)
                   && UnsizedType.is_scalar_type rtype ) ->
           Some UnsizedType.Void
       | _ -> None )

let get_sigs name =
  let name = Utils.stdlib_distribution_name name in
  Hashtbl.find_multi stan_math_signatures name |> List.sort ~compare

let make_assigmentoperator_stan_math_signatures assop =
  ( match assop with
  | Operator.Divide -> ["divide"]
  | assop -> operator_to_stan_math_fns assop )
  |> List.concat_map ~f:get_sigs
  |> List.concat_map ~f:(function
       | ReturnType rtype, [(ad1, lhs); (ad2, rhs)]
         when rtype = lhs
              && not
                   ( (assop = Operator.EltTimes || assop = Operator.EltDivide)
                   && UnsizedType.is_scalar_type rtype ) ->
           if rhs = UReal then
             [ (UnsizedType.Void, [(ad1, lhs); (ad2, UInt)])
             ; (Void, [(ad1, lhs); (ad2, UReal)]) ]
           else [(Void, [(ad1, lhs); (ad2, rhs)])]
       | _ -> [] )

let pp_math_sig ppf (rt, args) = UnsizedType.pp ppf (UFun (args, rt, FnPlain))

let pp_math_sigs ppf name =
  (Fmt.list ~sep:Fmt.cut pp_math_sig) ppf (get_sigs name)

let pretty_print_math_sigs = Fmt.strf "@[<v>@,%a@]" pp_math_sigs

let pretty_print_all_math_sigs ppf () =
  let open Fmt in
  let pp_sig ppf (name, (rt, args)) =
    pf ppf "%s(@[<hov 2>%a@]) => %a" name
      (list ~sep:comma UnsizedType.pp)
      (List.map ~f:snd args) UnsizedType.pp_returntype rt
  in
  let pp_sigs_for_name ppf name =
    (list ~sep:cut pp_sig) ppf
      (List.map ~f:(fun t -> (name, t)) (get_sigs name))
  in
  pf ppf "@[<v>%a@]"
    (list ~sep:cut pp_sigs_for_name)
    (List.sort ~compare (Hashtbl.keys stan_math_signatures))

let pretty_print_math_lib_operator_sigs op =
  if op = Operator.IntDivide then
    [Fmt.strf "@[<v>@,%a@]" pp_math_sig int_divide_type]
  else operator_to_stan_math_fns op |> List.map ~f:pretty_print_math_sigs

let pretty_print_math_lib_assignmentoperator_sigs op =
  match op with
  | Operator.Plus | Minus | Times | Divide | EltTimes | EltDivide ->
      Some
        (Fmt.strf "@[<v>@,%a@]"
           (Fmt.list ~sep:Fmt.cut pp_math_sig)
           (make_assigmentoperator_stan_math_signatures op))
  | _ -> None

(* -- Some helper definitions to populate stan_math_signatures -- *)
let bare_types = [UnsizedType.UInt; UReal; UVector; URowVector; UMatrix]
let bare_types_size = List.length bare_types
let vector_types = [UnsizedType.UReal; UArray UReal; UVector; URowVector]
let vector_types_size = List.length vector_types
let primitive_types = [UnsizedType.UInt; UReal]
let primitive_types_size = List.length primitive_types

let all_vector_types =
  [UnsizedType.UReal; UArray UReal; UVector; URowVector; UInt; UArray UInt]

let all_vector_types_size = List.length all_vector_types

let add_qualified (name, rt, argts) =
  Hashtbl.add_multi stan_math_signatures ~key:name ~data:(rt, argts)

let add_nullary name = add_unqualified (name, UnsizedType.ReturnType UReal, [])

let add_binary name =
  add_unqualified (name, ReturnType UReal, [UnsizedType.UReal; UReal])

let add_binary_vec name =
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified (name, ReturnType (ints_to_real i), [i; j]) )
        [UnsizedType.UInt; UReal] )
    [UnsizedType.UInt; UReal] ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (ints_to_real (bare_array_type (j, i)))
            , [bare_array_type (j, i); bare_array_type (j, i)] ) )
        [UnsizedType.UArray UInt; UArray UReal; UVector; URowVector; UMatrix]
      )
    (List.range 0 8) ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          List.iter
            ~f:(fun k ->
              add_unqualified
                ( name
                , ReturnType (ints_to_real (bare_array_type (k, j)))
                , [bare_array_type (k, j); i] ) )
            [ UnsizedType.UArray UInt; UArray UReal; UVector; URowVector
            ; UMatrix ] )
        (List.range 0 8) )
    [UnsizedType.UInt; UReal] ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          List.iter
            ~f:(fun k ->
              add_unqualified
                ( name
                , ReturnType (ints_to_real (bare_array_type (k, j)))
                , [i; bare_array_type (k, j)] ) )
            [ UnsizedType.UArray UInt; UArray UReal; UVector; URowVector
            ; UMatrix ] )
        (List.range 0 8) )
    [UnsizedType.UInt; UReal]

let add_binary_vec_real_real name =
  add_binary name ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (bare_array_type (j, i))
            , [bare_array_type (j, i); bare_array_type (j, i)] ) )
        [UnsizedType.UArray UReal; UVector; URowVector; UMatrix] )
    (List.range 0 8) ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          List.iter
            ~f:(fun k ->
              add_unqualified
                ( name
                , ReturnType (bare_array_type (k, j))
                , [bare_array_type (k, j); i] ) )
            [UnsizedType.UArray UReal; UVector; URowVector; UMatrix] )
        (List.range 0 8) )
    [UnsizedType.UReal] ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          List.iter
            ~f:(fun k ->
              add_unqualified
                ( name
                , ReturnType (bare_array_type (k, j))
                , [i; bare_array_type (k, j)] ) )
            [UnsizedType.UArray UReal; UVector; URowVector; UMatrix] )
        (List.range 0 8) )
    [UnsizedType.UReal]

let add_binary_vec_int_real name =
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (bare_array_type (i, j))
            , [UInt; bare_array_type (i, j)] ) )
        (List.range 0 8) )
    [UnsizedType.UArray UReal; UVector; URowVector; UMatrix] ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (bare_array_type (i, j))
            , [bare_array_type (UInt, j + 1); bare_array_type (i, j)] ) )
        (List.range 0 8) )
    [UnsizedType.UArray UReal; UVector; URowVector] ;
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UMatrix, i))
        , [bare_array_type (UInt, i + 2); bare_array_type (UMatrix, i)] ) )
    (List.range 0 8) ;
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UReal, i))
        , [bare_array_type (UInt, i); UReal] ) )
    (List.range 0 8)

let add_binary_vec_real_int name =
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (bare_array_type (i, j))
            , [bare_array_type (i, j); UInt] ) )
        (List.range 0 8) )
    [UnsizedType.UArray UReal; UVector; URowVector; UMatrix] ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (bare_array_type (i, j))
            , [bare_array_type (i, j); bare_array_type (UInt, j + 1)] ) )
        (List.range 0 8) )
    [UnsizedType.UArray UReal; UVector; URowVector] ;
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UMatrix, i))
        , [bare_array_type (UMatrix, i); bare_array_type (UInt, i + 2)] ) )
    (List.range 0 8) ;
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UReal, i))
        , [UReal; bare_array_type (UInt, i)] ) )
    (List.range 0 8)

let add_binary_vec_int_int name =
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UInt, i))
        , [bare_array_type (UInt, i); UInt] ) )
    (List.range 0 8) ;
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UInt, i))
        , [UInt; bare_array_type (UInt, i)] ) )
    (List.range 1 8) ;
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UInt, i))
        , [bare_array_type (UInt, i); bare_array_type (UInt, i)] ) )
    (List.range 1 8)

let add_ternary name =
  add_unqualified (name, ReturnType UReal, [UReal; UReal; UReal])

(*Adds functions that operate on matrix, double array and real types*)
let add_ternary_vec name =
  add_unqualified (name, ReturnType UReal, [UReal; UReal; UReal]) ;
  add_unqualified (name, ReturnType UVector, [UVector; UReal; UReal]) ;
  add_unqualified (name, ReturnType UVector, [UVector; UVector; UReal]) ;
  add_unqualified (name, ReturnType UVector, [UVector; UReal; UVector]) ;
  add_unqualified (name, ReturnType UVector, [UVector; UVector; UVector]) ;
  add_unqualified (name, ReturnType UVector, [UReal; UVector; UReal]) ;
  add_unqualified (name, ReturnType UVector, [UReal; UVector; UVector]) ;
  add_unqualified (name, ReturnType UVector, [UReal; UReal; UVector]) ;
  add_unqualified (name, ReturnType URowVector, [URowVector; UReal; UReal]) ;
  add_unqualified (name, ReturnType URowVector, [URowVector; URowVector; UReal]) ;
  add_unqualified (name, ReturnType URowVector, [URowVector; UReal; URowVector]) ;
  add_unqualified
    (name, ReturnType URowVector, [URowVector; URowVector; URowVector]) ;
  add_unqualified (name, ReturnType URowVector, [UReal; URowVector; UReal]) ;
  add_unqualified (name, ReturnType URowVector, [UReal; URowVector; URowVector]) ;
  add_unqualified (name, ReturnType URowVector, [UReal; UReal; URowVector]) ;
  add_unqualified (name, ReturnType UMatrix, [UMatrix; UReal; UReal]) ;
  add_unqualified (name, ReturnType UMatrix, [UMatrix; UMatrix; UReal]) ;
  add_unqualified (name, ReturnType UMatrix, [UMatrix; UReal; UMatrix]) ;
  add_unqualified (name, ReturnType UMatrix, [UMatrix; UMatrix; UMatrix]) ;
  add_unqualified (name, ReturnType UMatrix, [UReal; UMatrix; UReal]) ;
  add_unqualified (name, ReturnType UMatrix, [UReal; UMatrix; UMatrix]) ;
  add_unqualified (name, ReturnType UMatrix, [UReal; UReal; UMatrix])

let for_all_vector_types s = List.iter ~f:s all_vector_types
let for_vector_types s = List.iter ~f:s vector_types

(* -- Start populating stan_math_signaturess -- *)
let () =
  List.iter declarative_fnsigs ~f:(fun (key, rt, args) ->
      Hashtbl.add_multi stan_math_signatures ~key ~data:(rt, args) ) ;
  add_unqualified ("abs", ReturnType UInt, [UInt]) ;
  add_unqualified ("abs", ReturnType UReal, [UReal]) ;
  List.iter
    ~f:(fun x -> add_unqualified ("add", ReturnType x, [x; x]))
    bare_types ;
  add_unqualified ("add", ReturnType UVector, [UVector; UReal]) ;
  add_unqualified ("add", ReturnType URowVector, [URowVector; UReal]) ;
  add_unqualified ("add", ReturnType UMatrix, [UMatrix; UReal]) ;
  add_unqualified ("add", ReturnType UVector, [UReal; UVector]) ;
  add_unqualified ("add", ReturnType URowVector, [UReal; URowVector]) ;
  add_unqualified ("add", ReturnType UMatrix, [UReal; UMatrix]) ;
  add_unqualified ("add_diag", ReturnType UMatrix, [UMatrix; UReal]) ;
  add_unqualified ("add_diag", ReturnType UMatrix, [UMatrix; UVector]) ;
  add_unqualified ("add_diag", ReturnType UMatrix, [UMatrix; URowVector]) ;
  add_qualified
    ( "algebra_solver"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain ) )
      ; (AutoDiffable, UVector); (AutoDiffable, UVector)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ] ) ;
  add_qualified
    ( "algebra_solver"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain ) )
      ; (AutoDiffable, UVector); (AutoDiffable, UVector)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt); (DataOnly, UReal)
      ; (DataOnly, UReal); (DataOnly, UReal) ] ) ;
  add_qualified
    ( "algebra_solver_newton"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain ) )
      ; (AutoDiffable, UVector); (AutoDiffable, UVector)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ] ) ;
  add_qualified
    ( "algebra_solver_newton"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain ) )
      ; (AutoDiffable, UVector); (AutoDiffable, UVector)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt); (DataOnly, UReal)
      ; (DataOnly, UReal); (DataOnly, UReal) ] ) ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified
            ( "append_array"
            , ReturnType (bare_array_type (t, i))
            , [bare_array_type (t, i); bare_array_type (t, i)] ) )
        bare_types )
    (List.range 1 8) ;
  add_binary "atan2" ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UReal; UVector] ) ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UVector; UVector] ) ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UReal; UVector] ) ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UVector] ) ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UReal; UVector] ) ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UVector] ) ;
  add_binary_vec_int_real "bessel_first_kind" ;
  add_binary_vec_int_real "bessel_second_kind" ;
  add_binary_vec "beta" ;
  (* XXX For some reason beta_proportion_rng doesn't take ints as first arg *)
  for_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "beta_proportion_rng"
            , ReturnType (rng_return_type UReal [t; u])
            , [t; u] ) ) ) ;
  add_binary_vec_int_real "binary_log_loss" ;
  add_binary_vec "binomial_coefficient_log" ;
  add_unqualified
    ("block", ReturnType UMatrix, [UMatrix; UInt; UInt; UInt; UInt]) ;
  add_unqualified ("categorical_rng", ReturnType UInt, [UVector]) ;
  add_unqualified ("categorical_logit_rng", ReturnType UInt, [UVector]) ;
  add_unqualified
    ( "categorical_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UVector; UMatrix] ) ;
  add_unqualified
    ( "categorical_logit_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UMatrix] ) ;
  add_unqualified
    ( "categorical_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UMatrix] ) ;
  add_unqualified
    ( "categorical_logit_glm_lpmf"
    , ReturnType UReal
    , [UInt; URowVector; UVector; UMatrix] ) ;
  add_unqualified ("append_col", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("append_col", ReturnType UMatrix, [UVector; UMatrix]) ;
  add_unqualified ("append_col", ReturnType UMatrix, [UMatrix; UVector]) ;
  add_unqualified ("append_col", ReturnType UMatrix, [UVector; UVector]) ;
  add_unqualified
    ("append_col", ReturnType URowVector, [URowVector; URowVector]) ;
  add_unqualified ("append_col", ReturnType URowVector, [UReal; URowVector]) ;
  add_unqualified ("append_col", ReturnType URowVector, [URowVector; UReal]) ;
  add_unqualified ("chol2inv", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("cholesky_decompose", ReturnType UMatrix, [UMatrix]) ;
  add_binary_vec_int_int "choose" ;
  add_unqualified ("col", ReturnType UVector, [UMatrix; UInt]) ;
  add_unqualified ("cols", ReturnType UInt, [UVector]) ;
  add_unqualified ("cols", ReturnType UInt, [URowVector]) ;
  add_unqualified ("cols", ReturnType UInt, [UMatrix]) ;
  add_unqualified
    ("columns_dot_product", ReturnType URowVector, [UVector; UVector]) ;
  add_unqualified
    ("columns_dot_product", ReturnType URowVector, [URowVector; URowVector]) ;
  add_unqualified
    ("columns_dot_product", ReturnType URowVector, [UMatrix; UMatrix]) ;
  add_unqualified ("columns_dot_self", ReturnType URowVector, [UVector]) ;
  add_unqualified ("columns_dot_self", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("columns_dot_self", ReturnType URowVector, [UMatrix]) ;
  add_unqualified
    ("cov_exp_quad", ReturnType UMatrix, [UArray UReal; UReal; UReal]) ;
  add_unqualified
    ("cov_exp_quad", ReturnType UMatrix, [UArray UVector; UReal; UReal]) ;
  add_unqualified
    ("cov_exp_quad", ReturnType UMatrix, [UArray URowVector; UReal; UReal]) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType UMatrix
    , [UArray URowVector; UArray URowVector; UReal; UReal] ) ;
  add_unqualified ("crossprod", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ( "csr_matrix_times_vector"
    , ReturnType UVector
    , [UInt; UInt; UVector; UArray UInt; UArray UInt; UVector] ) ;
  add_unqualified
    ( "csr_to_dense_matrix"
    , ReturnType UMatrix
    , [UInt; UInt; UVector; UArray UInt; UArray UInt] ) ;
  add_unqualified ("csr_extract_w", ReturnType UVector, [UMatrix]) ;
  add_unqualified ("csr_extract_v", ReturnType (UArray UInt), [UMatrix]) ;
  add_unqualified ("csr_extract_u", ReturnType (UArray UInt), [UMatrix]) ;
  add_unqualified ("cumulative_sum", ReturnType (UArray UReal), [UArray UReal]) ;
  add_unqualified ("cumulative_sum", ReturnType UVector, [UVector]) ;
  add_unqualified ("cumulative_sum", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("determinant", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("diag_matrix", ReturnType UMatrix, [UVector]) ;
  add_unqualified ("diag_post_multiply", ReturnType UMatrix, [UMatrix; UVector]) ;
  add_unqualified
    ("diag_post_multiply", ReturnType UMatrix, [UMatrix; URowVector]) ;
  add_unqualified ("diag_pre_multiply", ReturnType UMatrix, [UVector; UMatrix]) ;
  add_unqualified
    ("diag_pre_multiply", ReturnType UMatrix, [URowVector; UMatrix]) ;
  add_unqualified ("diagonal", ReturnType UVector, [UMatrix]) ;
  add_unqualified ("dims", ReturnType (UArray UInt), [UInt]) ;
  add_unqualified ("dims", ReturnType (UArray UInt), [UReal]) ;
  add_unqualified ("dims", ReturnType (UArray UInt), [UVector]) ;
  add_unqualified ("dims", ReturnType (UArray UInt), [URowVector]) ;
  add_unqualified ("dims", ReturnType (UArray UInt), [UMatrix]) ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified
            ("dims", ReturnType (UArray UInt), [bare_array_type (t, i + 1)]) )
        bare_types )
    (List.range 0 8) ;
  add_unqualified ("dirichlet_rng", ReturnType UVector, [UVector]) ;
  add_unqualified ("distance", ReturnType UReal, [UVector; UVector]) ;
  add_unqualified ("distance", ReturnType UReal, [URowVector; URowVector]) ;
  add_unqualified ("distance", ReturnType UReal, [UVector; URowVector]) ;
  add_unqualified ("distance", ReturnType UReal, [URowVector; UVector]) ;
  add_unqualified ("divide", ReturnType UInt, [UInt; UInt]) ;
  add_unqualified ("divide", ReturnType UReal, [UReal; UReal]) ;
  add_unqualified ("divide", ReturnType UVector, [UVector; UReal]) ;
  add_unqualified ("divide", ReturnType URowVector, [URowVector; UReal]) ;
  add_unqualified ("divide", ReturnType UMatrix, [UMatrix; UReal]) ;
  add_unqualified ("dot_product", ReturnType UReal, [UVector; UVector]) ;
  add_unqualified ("dot_product", ReturnType UReal, [URowVector; URowVector]) ;
  add_unqualified ("dot_product", ReturnType UReal, [UVector; URowVector]) ;
  add_unqualified ("dot_product", ReturnType UReal, [URowVector; UVector]) ;
  add_unqualified
    ("dot_product", ReturnType UReal, [UArray UReal; UArray UReal]) ;
  add_unqualified ("dot_self", ReturnType UReal, [UVector]) ;
  add_unqualified ("dot_self", ReturnType UReal, [URowVector]) ;
  add_nullary "e" ;
  add_unqualified ("eigenvalues_sym", ReturnType UVector, [UMatrix]) ;
  add_unqualified ("eigenvectors_sym", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("generalized_inverse", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("qr_Q", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("qr_R", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("qr_thin_Q", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("qr_thin_R", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("elt_divide", ReturnType UInt, [UInt; UInt]) ;
  add_unqualified ("elt_divide", ReturnType UReal, [UReal; UReal]) ;
  add_unqualified ("elt_divide", ReturnType UVector, [UVector; UVector]) ;
  add_unqualified
    ("elt_divide", ReturnType URowVector, [URowVector; URowVector]) ;
  add_unqualified ("elt_divide", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("elt_divide", ReturnType UVector, [UVector; UReal]) ;
  add_unqualified ("elt_divide", ReturnType URowVector, [URowVector; UReal]) ;
  add_unqualified ("elt_divide", ReturnType UMatrix, [UMatrix; UReal]) ;
  add_unqualified ("elt_divide", ReturnType UVector, [UReal; UVector]) ;
  add_unqualified ("elt_divide", ReturnType URowVector, [UReal; URowVector]) ;
  add_unqualified ("elt_divide", ReturnType UMatrix, [UReal; UMatrix]) ;
  add_unqualified ("elt_multiply", ReturnType UInt, [UInt; UInt]) ;
  add_unqualified ("elt_multiply", ReturnType UReal, [UReal; UReal]) ;
  add_unqualified ("elt_multiply", ReturnType UVector, [UVector; UVector]) ;
  add_unqualified
    ("elt_multiply", ReturnType URowVector, [URowVector; URowVector]) ;
  add_unqualified ("elt_multiply", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_binary_vec_int_int "falling_factorial" ;
  add_binary_vec_real_int "falling_factorial" ;
  add_binary_vec "fdim" ;
  add_ternary_vec "fma" ;
  add_binary_vec "fmax" ;
  add_binary_vec "fmin" ;
  add_binary_vec "fmod" ;
  add_binary_vec_real_real "gamma_p" ;
  add_binary_vec_real_real "gamma_q" ;
  add_unqualified
    ( "gaussian_dlm_obs_log"
    , ReturnType UReal
    , [UMatrix; UMatrix; UMatrix; UMatrix; UMatrix; UVector; UMatrix] ) ;
  add_unqualified
    ( "gaussian_dlm_obs_log"
    , ReturnType UReal
    , [UMatrix; UMatrix; UMatrix; UVector; UMatrix; UVector; UMatrix] ) ;
  add_unqualified
    ( "gaussian_dlm_obs_lpdf"
    , ReturnType UReal
    , [UMatrix; UMatrix; UMatrix; UMatrix; UMatrix; UVector; UMatrix] ) ;
  add_unqualified
    ( "gaussian_dlm_obs_lpdf"
    , ReturnType UReal
    , [UMatrix; UMatrix; UMatrix; UVector; UMatrix; UVector; UMatrix] ) ;
  add_unqualified ("gp_dot_prod_cov", ReturnType UMatrix, [UArray UReal; UReal]) ;
  add_unqualified
    ("gp_dot_prod_cov", ReturnType UMatrix, [UArray UReal; UArray UReal; UReal]) ;
  add_unqualified
    ("gp_dot_prod_cov", ReturnType UMatrix, [UArray UReal; UArray UReal; UReal]) ;
  add_unqualified
    ("gp_dot_prod_cov", ReturnType UMatrix, [UArray UVector; UReal]) ;
  add_unqualified
    ( "gp_dot_prod_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal] ) ;
  add_unqualified
    ("gp_exp_quad_cov", ReturnType UMatrix, [UArray UReal; UReal; UReal]) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal] ) ;
  add_unqualified
    ("gp_exp_quad_cov", ReturnType UMatrix, [UArray UVector; UReal; UReal]) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal] ) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UArray UReal] ) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UArray UReal] ) ;
  add_unqualified
    ("gp_matern32_cov", ReturnType UMatrix, [UArray UReal; UReal; UReal]) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal] ) ;
  add_unqualified
    ("gp_matern32_cov", ReturnType UMatrix, [UArray UVector; UReal; UReal]) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal] ) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UArray UReal] ) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UArray UReal] ) ;
  add_unqualified
    ("gp_matern52_cov", ReturnType UMatrix, [UArray UReal; UReal; UReal]) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal] ) ;
  add_unqualified
    ("gp_matern52_cov", ReturnType UMatrix, [UArray UVector; UReal; UReal]) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal] ) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UArray UReal] ) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UArray UReal] ) ;
  add_unqualified
    ("gp_exponential_cov", ReturnType UMatrix, [UArray UReal; UReal; UReal]) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal] ) ;
  add_unqualified
    ("gp_exponential_cov", ReturnType UMatrix, [UArray UVector; UReal; UReal]) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal] ) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UArray UReal] ) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UArray UReal] ) ;
  add_unqualified
    ("gp_periodic_cov", ReturnType UMatrix, [UArray UReal; UReal; UReal; UReal]) ;
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal; UReal] ) ;
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UReal; UReal] ) ;
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal; UReal] ) ;
  (* ; add_nullary ("get_lp")   *)
  add_unqualified ("head", ReturnType URowVector, [URowVector; UInt]) ;
  add_unqualified ("head", ReturnType UVector, [UVector; UInt]) ;
  List.iter
    ~f:(fun t ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( "head"
            , ReturnType (bare_array_type (t, j))
            , [bare_array_type (t, j); UInt] ) )
        (List.range 1 4) )
    bare_types ;
  add_unqualified
    ("hmm_marginal", ReturnType UReal, [UMatrix; UMatrix; UVector]) ;
  add_qualified
    ( "hmm_hidden_state_prob"
    , ReturnType UMatrix
    , [(DataOnly, UMatrix); (DataOnly, UMatrix); (DataOnly, UVector)] ) ;
  add_unqualified
    ("hmm_latent_rng", ReturnType (UArray UInt), [UMatrix; UMatrix; UVector]) ;
  add_unqualified
    ("hypergeometric_log", ReturnType UReal, [UInt; UInt; UInt; UInt]) ;
  add_unqualified
    ("hypergeometric_lpmf", ReturnType UReal, [UInt; UInt; UInt; UInt]) ;
  add_unqualified ("hypergeometric_rng", ReturnType UInt, [UInt; UInt; UInt]) ;
  add_binary_vec "hypot" ;
  add_unqualified ("identity_matrix", ReturnType UMatrix, [UInt]) ;
  add_unqualified ("if_else", ReturnType UInt, [UInt; UInt; UInt]) ;
  add_unqualified ("if_else", ReturnType UReal, [UInt; UReal; UReal]) ;
  add_unqualified ("inc_beta", ReturnType UReal, [UReal; UReal; UReal]) ;
  add_unqualified ("int_step", ReturnType UInt, [UReal]) ;
  add_unqualified ("int_step", ReturnType UInt, [UInt]) ;
  add_qualified
    ( "integrate_1d"
    , ReturnType UReal
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UReal
            , FnPlain ) )
      ; (AutoDiffable, UReal); (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ] ) ;
  add_qualified
    ( "integrate_1d"
    , ReturnType UReal
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UReal
            , FnPlain ) )
      ; (AutoDiffable, UReal); (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt); (DataOnly, UReal) ]
    ) ;
  add_qualified
    ( "integrate_ode"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain ) )
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ] ) ;
  add_qualified
    ( "integrate_ode_adams"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain ) )
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ] ) ;
  add_qualified
    ( "integrate_ode_adams"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain ) )
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt); (DataOnly, UReal)
      ; (DataOnly, UReal); (DataOnly, UReal) ] ) ;
  add_qualified
    ( "integrate_ode_bdf"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain ) )
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ] ) ;
  add_qualified
    ( "integrate_ode_bdf"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain ) )
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt); (DataOnly, UReal)
      ; (DataOnly, UReal); (DataOnly, UReal) ] ) ;
  add_qualified
    ( "integrate_ode_rk45"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain ) )
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ] ) ;
  add_qualified
    ( "integrate_ode_rk45"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain ) )
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt); (DataOnly, UReal)
      ; (DataOnly, UReal); (DataOnly, UReal) ] ) ;
  add_unqualified
    ("inv_wishart_log", ReturnType UReal, [UMatrix; UReal; UMatrix]) ;
  add_unqualified
    ("inv_wishart_lpdf", ReturnType UReal, [UMatrix; UReal; UMatrix]) ;
  add_unqualified ("inv_wishart_rng", ReturnType UMatrix, [UReal; UMatrix]) ;
  add_unqualified ("inverse", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("inverse_spd", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("is_inf", ReturnType UInt, [UReal]) ;
  add_unqualified ("is_nan", ReturnType UInt, [UReal]) ;
  add_binary_vec "lbeta" ;
  add_binary "lchoose" ;
  add_binary_vec_real_int "ldexp" ;
  add_qualified
    ( "linspaced_int_array"
    , ReturnType (UArray UInt)
    , [(DataOnly, UInt); (DataOnly, UInt); (DataOnly, UInt)] ) ;
  add_qualified
    ( "linspaced_array"
    , ReturnType (UArray UReal)
    , [(DataOnly, UInt); (DataOnly, UReal); (DataOnly, UReal)] ) ;
  add_qualified
    ( "linspaced_row_vector"
    , ReturnType URowVector
    , [(DataOnly, UInt); (DataOnly, UReal); (DataOnly, UReal)] ) ;
  add_qualified
    ( "linspaced_vector"
    , ReturnType UVector
    , [(DataOnly, UInt); (DataOnly, UReal); (DataOnly, UReal)] ) ;
  add_unqualified ("lkj_corr_cholesky_log", ReturnType UReal, [UMatrix; UReal]) ;
  add_unqualified ("lkj_corr_cholesky_lpdf", ReturnType UReal, [UMatrix; UReal]) ;
  add_unqualified ("lkj_corr_cholesky_rng", ReturnType UMatrix, [UInt; UReal]) ;
  add_unqualified ("lkj_corr_log", ReturnType UReal, [UMatrix; UReal]) ;
  add_unqualified ("lkj_corr_lpdf", ReturnType UReal, [UMatrix; UReal]) ;
  add_unqualified ("lkj_corr_rng", ReturnType UMatrix, [UInt; UReal]) ;
  add_unqualified
    ("lkj_cov_log", ReturnType UReal, [UMatrix; UVector; UVector; UReal]) ;
  add_binary_vec_int_real "lmgamma" ;
  add_binary_vec "lmultiply" ;
  add_nullary "log10" ;
  add_nullary "log2" ;
  add_unqualified ("log_determinant", ReturnType UReal, [UMatrix]) ;
  add_binary_vec "log_diff_exp" ;
  add_binary_vec "log_falling_factorial" ;
  add_binary_vec "log_inv_logit_diff" ;
  add_ternary "log_mix" ;
  List.iter
    ~f:(fun v1 ->
      List.iter
        ~f:(fun v2 -> add_unqualified ("log_mix", ReturnType UReal, [v1; v2]))
        (List.tl_exn vector_types) ;
      add_unqualified ("log_mix", ReturnType UReal, [v1; UArray UVector]) ;
      add_unqualified ("log_mix", ReturnType UReal, [v1; UArray URowVector]) )
    (List.tl_exn vector_types) ;
  add_binary_vec "log_modified_bessel_first_kind" ;
  add_binary_vec "log_rising_factorial" ;
  add_unqualified ("log_softmax", ReturnType UVector, [UVector]) ;
  add_unqualified ("log_sum_exp", ReturnType UReal, [UArray UReal]) ;
  add_unqualified ("log_sum_exp", ReturnType UReal, [UVector]) ;
  add_unqualified ("log_sum_exp", ReturnType UReal, [URowVector]) ;
  add_unqualified ("log_sum_exp", ReturnType UReal, [UMatrix]) ;
  add_binary "log_sum_exp" ;
  let logical_binops =
    [ "logical_or"; "logical_and"; "logical_eq"; "logical_neq"; "logical_lt"
    ; "logical_lte"; "logical_gt"; "logical_gte" ]
  in
  List.iter
    ~f:(fun t1 ->
      add_unqualified ("logical_negation", ReturnType UInt, [t1]) ;
      List.iter
        ~f:(fun t2 ->
          List.iter
            ~f:(fun o -> add_unqualified (o, ReturnType UInt, [t1; t2]))
            logical_binops )
        primitive_types )
    primitive_types ;
  add_nullary "machine_precision" ;
  add_qualified
    ( "map_rect"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain ) )
      ; (AutoDiffable, UVector)
      ; (AutoDiffable, UArray UVector)
      ; (DataOnly, UArray (UArray UReal))
      ; (DataOnly, UArray (UArray UInt)) ] ) ;
  add_unqualified ("matrix_exp", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ("matrix_exp_multiply", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("matrix_power", ReturnType UMatrix, [UMatrix; UInt]) ;
  add_unqualified ("max", ReturnType UInt, [UArray UInt]) ;
  add_unqualified ("max", ReturnType UReal, [UArray UReal]) ;
  add_unqualified ("max", ReturnType UReal, [UVector]) ;
  add_unqualified ("max", ReturnType UReal, [URowVector]) ;
  add_unqualified ("max", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("max", ReturnType UInt, [UInt; UInt]) ;
  add_unqualified ("mdivide_left", ReturnType UVector, [UMatrix; UVector]) ;
  add_unqualified ("mdivide_left", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("mdivide_left_spd", ReturnType UVector, [UMatrix; UVector]) ;
  add_unqualified ("mdivide_left_spd", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified
    ("mdivide_left_tri_low", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified
    ("mdivide_left_tri_low", ReturnType UVector, [UMatrix; UVector]) ;
  add_unqualified
    ("mdivide_right", ReturnType URowVector, [URowVector; UMatrix]) ;
  add_unqualified ("mdivide_right_spd", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified
    ("mdivide_right_spd", ReturnType URowVector, [URowVector; UMatrix]) ;
  add_unqualified ("mdivide_right", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified
    ("mdivide_right_tri_low", ReturnType URowVector, [URowVector; UMatrix]) ;
  add_unqualified
    ("mdivide_right_tri_low", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("mean", ReturnType UReal, [UArray UReal]) ;
  add_unqualified ("mean", ReturnType UReal, [UVector]) ;
  add_unqualified ("mean", ReturnType UReal, [URowVector]) ;
  add_unqualified ("mean", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("min", ReturnType UInt, [UArray UInt]) ;
  add_unqualified ("min", ReturnType UReal, [UArray UReal]) ;
  add_unqualified ("min", ReturnType UReal, [UVector]) ;
  add_unqualified ("min", ReturnType UReal, [URowVector]) ;
  add_unqualified ("min", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("min", ReturnType UInt, [UInt; UInt]) ;
  add_unqualified ("minus", ReturnType UInt, [UInt]) ;
  add_unqualified ("minus", ReturnType UReal, [UReal]) ;
  add_unqualified ("minus", ReturnType UVector, [UVector]) ;
  add_unqualified ("minus", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("minus", ReturnType UMatrix, [UMatrix]) ;
  add_binary_vec_int_real "modified_bessel_first_kind" ;
  add_binary_vec_int_real "modified_bessel_second_kind" ;
  add_unqualified ("modulus", ReturnType UInt, [UInt; UInt]) ;
  add_unqualified ("multi_normal_rng", ReturnType UVector, [UVector; UMatrix]) ;
  add_unqualified
    ("multi_normal_rng", ReturnType (UArray UVector), [UArray UVector; UMatrix]) ;
  add_unqualified
    ("multi_normal_rng", ReturnType UVector, [URowVector; UMatrix]) ;
  add_unqualified
    ( "multi_normal_rng"
    , ReturnType (UArray UVector)
    , [UArray URowVector; UMatrix] ) ;
  add_unqualified
    ("multi_normal_cholesky_rng", ReturnType UVector, [UVector; UMatrix]) ;
  add_unqualified
    ( "multi_normal_cholesky_rng"
    , ReturnType (UArray UVector)
    , [UArray UVector; UMatrix] ) ;
  add_unqualified
    ("multi_normal_cholesky_rng", ReturnType UVector, [URowVector; UMatrix]) ;
  add_unqualified
    ( "multi_normal_cholesky_rng"
    , ReturnType (UArray UVector)
    , [UArray URowVector; UMatrix] ) ;
  add_unqualified
    ("multi_student_t_rng", ReturnType UVector, [UReal; UVector; UMatrix]) ;
  add_unqualified
    ( "multi_student_t_rng"
    , ReturnType (UArray UVector)
    , [UReal; UArray UVector; UMatrix] ) ;
  add_unqualified
    ("multi_student_t_rng", ReturnType UVector, [UReal; URowVector; UMatrix]) ;
  add_unqualified
    ( "multi_student_t_rng"
    , ReturnType (UArray UVector)
    , [UReal; UArray URowVector; UMatrix] ) ;
  add_unqualified
    ("multinomial_log", ReturnType UReal, [bare_array_type (UInt, 1); UVector]) ;
  add_unqualified
    ("multinomial_lpmf", ReturnType UReal, [bare_array_type (UInt, 1); UVector]) ;
  add_unqualified
    ("multinomial_rng", ReturnType (bare_array_type (UInt, 1)), [UVector; UInt]) ;
  add_unqualified
    ("multinomial_logit_log", ReturnType UReal, [UArray UInt; UVector]) ;
  add_unqualified
    ("multinomial_logit_lpmf", ReturnType UReal, [UArray UInt; UVector]) ;
  add_unqualified
    ("multinomial_logit_rng", ReturnType (UArray UInt), [UVector; UInt]) ;
  add_unqualified ("multinomial_log", ReturnType UReal, [UArray UInt; UVector]) ;
  add_unqualified ("multinomial_lpmf", ReturnType UReal, [UArray UInt; UVector]) ;
  add_unqualified ("multinomial_rng", ReturnType (UArray UInt), [UVector; UInt]) ;
  add_unqualified ("multiply", ReturnType UInt, [UInt; UInt]) ;
  add_unqualified ("multiply", ReturnType UReal, [UReal; UReal]) ;
  add_unqualified ("multiply", ReturnType UVector, [UVector; UReal]) ;
  add_unqualified ("multiply", ReturnType URowVector, [URowVector; UReal]) ;
  add_unqualified ("multiply", ReturnType UMatrix, [UMatrix; UReal]) ;
  add_unqualified ("multiply", ReturnType UReal, [URowVector; UVector]) ;
  add_unqualified ("multiply", ReturnType UMatrix, [UVector; URowVector]) ;
  add_unqualified ("multiply", ReturnType UVector, [UMatrix; UVector]) ;
  add_unqualified ("multiply", ReturnType URowVector, [URowVector; UMatrix]) ;
  add_unqualified ("multiply", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("multiply", ReturnType UVector, [UReal; UVector]) ;
  add_unqualified ("multiply", ReturnType URowVector, [UReal; URowVector]) ;
  add_unqualified ("multiply", ReturnType UMatrix, [UReal; UMatrix]) ;
  add_binary_vec "multiply_log" ;
  add_unqualified
    ("multiply_lower_tri_self_transpose", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UReal; UVector; UReal] ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UVector; UVector; UReal] ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UReal; UVector; UReal] ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UVector; UReal] ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UReal; UVector; UReal] ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UVector; UReal] ) ;
  add_nullary "negative_infinity" ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; UMatrix; UReal; UVector; UReal] ) ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; UMatrix; UVector; UVector; UReal] ) ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UReal; UMatrix; UReal; UVector; UVector] ) ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UReal; UMatrix; UVector; UVector; UVector] ) ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; URowVector; UReal; UVector; UVector] ) ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; URowVector; UVector; UVector; UVector] ) ;
  add_nullary "not_a_number" ;
  add_unqualified ("num_elements", ReturnType UInt, [UMatrix]) ;
  add_unqualified ("num_elements", ReturnType UInt, [UVector]) ;
  add_unqualified ("num_elements", ReturnType UInt, [URowVector]) ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified
            ("num_elements", ReturnType UInt, [bare_array_type (t, i)]) )
        bare_types )
    (List.range 1 10) ;
  add_unqualified ("one_hot_int_array", ReturnType (UArray UInt), [UInt; UInt]) ;
  add_unqualified ("one_hot_array", ReturnType (UArray UReal), [UInt; UInt]) ;
  add_unqualified ("one_hot_row_vector", ReturnType URowVector, [UInt; UInt]) ;
  add_unqualified ("one_hot_vector", ReturnType UVector, [UInt; UInt]) ;
  add_unqualified ("ones_int_array", ReturnType (UArray UInt), [UInt]) ;
  add_unqualified ("ones_array", ReturnType (UArray UReal), [UInt]) ;
  add_unqualified ("ones_row_vector", ReturnType URowVector, [UInt]) ;
  add_unqualified ("ones_vector", ReturnType UVector, [UInt]) ;
  add_unqualified
    ( "ordered_logistic_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UVector; UVector] ) ;
  add_unqualified
    ( "ordered_logistic_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UVector] ) ;
  add_unqualified
    ( "ordered_logistic_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UVector] ) ;
  add_unqualified
    ( "ordered_logistic_glm_lpmf"
    , ReturnType UReal
    , [UInt; URowVector; UVector; UVector] ) ;
  add_unqualified
    ("ordered_logistic_log", ReturnType UReal, [UInt; UReal; UVector]) ;
  add_unqualified
    ("ordered_logistic_log", ReturnType UReal, [UArray UInt; UVector; UVector]) ;
  add_unqualified
    ( "ordered_logistic_log"
    , ReturnType UReal
    , [UArray UInt; UVector; UArray UVector] ) ;
  add_unqualified
    ("ordered_logistic_lpmf", ReturnType UReal, [UInt; UReal; UVector]) ;
  add_unqualified
    ("ordered_logistic_lpmf", ReturnType UReal, [UArray UInt; UVector; UVector]) ;
  add_unqualified
    ( "ordered_logistic_lpmf"
    , ReturnType UReal
    , [UArray UInt; UVector; UArray UVector] ) ;
  add_unqualified ("ordered_logistic_rng", ReturnType UInt, [UReal; UVector]) ;
  add_unqualified
    ("ordered_probit_log", ReturnType UReal, [UInt; UReal; UVector]) ;
  add_unqualified
    ("ordered_probit_log", ReturnType UReal, [UArray UInt; UVector; UVector]) ;
  add_unqualified
    ( "ordered_probit_log"
    , ReturnType UReal
    , [UArray UInt; UVector; UArray UVector] ) ;
  add_unqualified
    ("ordered_probit_lpmf", ReturnType UReal, [UInt; UReal; UVector]) ;
  add_unqualified
    ("ordered_probit_lpmf", ReturnType UReal, [UArray UInt; UReal; UVector]) ;
  add_unqualified
    ( "ordered_probit_lpmf"
    , ReturnType UReal
    , [UArray UInt; UReal; UArray UVector] ) ;
  add_unqualified ("ordered_probit_rng", ReturnType UInt, [UReal; UVector]) ;
  add_binary_vec_real_real "owens_t" ;
  add_nullary "pi" ;
  add_unqualified ("plus", ReturnType UInt, [UInt]) ;
  add_unqualified ("plus", ReturnType UReal, [UReal]) ;
  add_unqualified ("plus", ReturnType UVector, [UVector]) ;
  add_unqualified ("plus", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("plus", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UReal; UVector] ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UVector; UVector] ) ;
  add_unqualified
    ("poisson_log_glm_lpmf", ReturnType UReal, [UInt; UMatrix; UReal; UVector]) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UVector] ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UReal; UVector] ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UVector] ) ;
  add_nullary "positive_infinity" ;
  add_binary_vec "pow" ;
  add_unqualified ("prod", ReturnType UInt, [UArray UInt]) ;
  add_unqualified ("prod", ReturnType UReal, [UArray UReal]) ;
  add_unqualified ("prod", ReturnType UReal, [UVector]) ;
  add_unqualified ("prod", ReturnType UReal, [URowVector]) ;
  add_unqualified ("prod", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("quad_form", ReturnType UReal, [UMatrix; UVector]) ;
  add_unqualified ("quad_form", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("quad_form_sym", ReturnType UReal, [UMatrix; UVector]) ;
  add_unqualified ("quad_form_sym", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("quad_form_diag", ReturnType UMatrix, [UMatrix; UVector]) ;
  add_unqualified ("quad_form_diag", ReturnType UMatrix, [UMatrix; URowVector]) ;
  add_qualified
    ( "quantile"
    , ReturnType UReal
    , [(DataOnly, UArray UReal); (DataOnly, UReal)] ) ;
  add_qualified
    ( "quantile"
    , ReturnType (UArray UReal)
    , [(DataOnly, UArray UReal); (DataOnly, UArray UReal)] ) ;
  add_qualified
    ("quantile", ReturnType UReal, [(DataOnly, UVector); (DataOnly, UReal)]) ;
  add_qualified
    ( "quantile"
    , ReturnType (UArray UReal)
    , [(DataOnly, UVector); (DataOnly, UArray UReal)] ) ;
  add_qualified
    ("quantile", ReturnType UReal, [(DataOnly, URowVector); (DataOnly, UReal)]) ;
  add_qualified
    ( "quantile"
    , ReturnType (UArray UReal)
    , [(DataOnly, URowVector); (DataOnly, UArray UReal)] ) ;
  add_unqualified ("rank", ReturnType UInt, [UArray UInt; UInt]) ;
  add_unqualified ("rank", ReturnType UInt, [UArray UReal; UInt]) ;
  add_unqualified ("rank", ReturnType UInt, [UVector; UInt]) ;
  add_unqualified ("rank", ReturnType UInt, [URowVector; UInt]) ;
  add_unqualified ("append_row", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("append_row", ReturnType UMatrix, [URowVector; UMatrix]) ;
  add_unqualified ("append_row", ReturnType UMatrix, [UMatrix; URowVector]) ;
  add_unqualified ("append_row", ReturnType UMatrix, [URowVector; URowVector]) ;
  add_unqualified ("append_row", ReturnType UVector, [UVector; UVector]) ;
  add_unqualified ("append_row", ReturnType UVector, [UReal; UVector]) ;
  add_unqualified ("append_row", ReturnType UVector, [UVector; UReal]) ;
  List.iter
    ~f:(fun t ->
      add_unqualified
        ("rep_array", ReturnType (bare_array_type (t, 1)), [t; UInt]) ;
      add_unqualified
        ("rep_array", ReturnType (bare_array_type (t, 2)), [t; UInt; UInt]) ;
      add_unqualified
        ( "rep_array"
        , ReturnType (bare_array_type (t, 3))
        , [t; UInt; UInt; UInt] ) ;
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( "rep_array"
            , ReturnType (bare_array_type (t, j + 1))
            , [bare_array_type (t, j); UInt] ) ;
          add_unqualified
            ( "rep_array"
            , ReturnType (bare_array_type (t, j + 2))
            , [bare_array_type (t, j); UInt; UInt] ) ;
          add_unqualified
            ( "rep_array"
            , ReturnType (bare_array_type (t, j + 3))
            , [bare_array_type (t, j); UInt; UInt; UInt] ) )
        (List.range 1 3) )
    bare_types ;
  add_unqualified ("rep_matrix", ReturnType UMatrix, [UReal; UInt; UInt]) ;
  add_unqualified ("rep_matrix", ReturnType UMatrix, [UVector; UInt]) ;
  add_unqualified ("rep_matrix", ReturnType UMatrix, [URowVector; UInt]) ;
  add_unqualified ("rep_row_vector", ReturnType URowVector, [UReal; UInt]) ;
  add_unqualified ("rep_vector", ReturnType UVector, [UReal; UInt]) ;
  add_unqualified ("reverse", ReturnType UVector, [UVector]) ;
  add_unqualified ("reverse", ReturnType URowVector, [URowVector]) ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified
            ( "reverse"
            , ReturnType (bare_array_type (t, i))
            , [bare_array_type (t, i)] ) )
        bare_types )
    (List.range 1 8) ;
  add_binary_vec_int_int "rising_factorial" ;
  add_binary_vec_real_int "rising_factorial" ;
  add_unqualified ("row", ReturnType URowVector, [UMatrix; UInt]) ;
  add_unqualified ("rows", ReturnType UInt, [UVector]) ;
  add_unqualified ("rows", ReturnType UInt, [URowVector]) ;
  add_unqualified ("rows", ReturnType UInt, [UMatrix]) ;
  add_unqualified ("rows_dot_product", ReturnType UVector, [UVector; UVector]) ;
  add_unqualified
    ("rows_dot_product", ReturnType UVector, [URowVector; URowVector]) ;
  add_unqualified ("rows_dot_product", ReturnType UVector, [UMatrix; UMatrix]) ;
  add_unqualified ("rows_dot_self", ReturnType UVector, [UVector]) ;
  add_unqualified ("rows_dot_self", ReturnType UVector, [URowVector]) ;
  add_unqualified ("rows_dot_self", ReturnType UVector, [UMatrix]) ;
  add_unqualified
    ("scale_matrix_exp_multiply", ReturnType UMatrix, [UReal; UMatrix; UMatrix]) ;
  add_unqualified ("sd", ReturnType UReal, [UArray UReal]) ;
  add_unqualified ("sd", ReturnType UReal, [UVector]) ;
  add_unqualified ("sd", ReturnType UReal, [URowVector]) ;
  add_unqualified ("sd", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("segment", ReturnType URowVector, [URowVector; UInt; UInt]) ;
  add_unqualified ("segment", ReturnType UVector, [UVector; UInt; UInt]) ;
  List.iter
    ~f:(fun t ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( "segment"
            , ReturnType (bare_array_type (t, j))
            , [bare_array_type (t, j); UInt; UInt] ) )
        (List.range 1 4) )
    bare_types ;
  add_unqualified ("singular_values", ReturnType UVector, [UMatrix]) ;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified ("size", ReturnType UInt, [bare_array_type (t, i)])
          )
        bare_types )
    (List.range 1 8) ;
  List.iter
    ~f:(fun t -> add_unqualified ("size", ReturnType UInt, [t]))
    bare_types ;
  add_unqualified ("softmax", ReturnType UVector, [UVector]) ;
  add_unqualified ("sort_asc", ReturnType (UArray UInt), [UArray UInt]) ;
  add_unqualified ("sort_asc", ReturnType (UArray UReal), [UArray UReal]) ;
  add_unqualified ("sort_asc", ReturnType UVector, [UVector]) ;
  add_unqualified ("sort_asc", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("sort_desc", ReturnType (UArray UInt), [UArray UInt]) ;
  add_unqualified ("sort_desc", ReturnType (UArray UReal), [UArray UReal]) ;
  add_unqualified ("sort_desc", ReturnType UVector, [UVector]) ;
  add_unqualified ("sort_desc", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("sort_indices_asc", ReturnType (UArray UInt), [UArray UInt]) ;
  add_unqualified ("sort_indices_asc", ReturnType (UArray UInt), [UArray UReal]) ;
  add_unqualified ("sort_indices_asc", ReturnType (UArray UInt), [UVector]) ;
  add_unqualified ("sort_indices_asc", ReturnType (UArray UInt), [URowVector]) ;
  add_unqualified ("sort_indices_desc", ReturnType (UArray UInt), [UArray UInt]) ;
  add_unqualified
    ("sort_indices_desc", ReturnType (UArray UInt), [UArray UReal]) ;
  add_unqualified ("sort_indices_desc", ReturnType (UArray UInt), [UVector]) ;
  add_unqualified ("sort_indices_desc", ReturnType (UArray UInt), [URowVector]) ;
  add_unqualified ("squared_distance", ReturnType UReal, [UReal; UReal]) ;
  add_unqualified ("squared_distance", ReturnType UReal, [UVector; UVector]) ;
  add_unqualified
    ("squared_distance", ReturnType UReal, [URowVector; URowVector]) ;
  add_unqualified ("squared_distance", ReturnType UReal, [UVector; URowVector]) ;
  add_unqualified ("squared_distance", ReturnType UReal, [URowVector; UVector]) ;
  add_nullary "sqrt2" ;
  add_unqualified ("sub_col", ReturnType UVector, [UMatrix; UInt; UInt; UInt]) ;
  add_unqualified
    ("sub_row", ReturnType URowVector, [UMatrix; UInt; UInt; UInt]) ;
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( "subtract"
        , ReturnType (List.nth_exn bare_types i)
        , [List.nth_exn bare_types i; List.nth_exn bare_types i] ) )
    (List.range 0 bare_types_size) ;
  add_unqualified ("subtract", ReturnType UVector, [UVector; UReal]) ;
  add_unqualified ("subtract", ReturnType URowVector, [URowVector; UReal]) ;
  add_unqualified ("subtract", ReturnType UMatrix, [UMatrix; UReal]) ;
  add_unqualified ("subtract", ReturnType UVector, [UReal; UVector]) ;
  add_unqualified ("subtract", ReturnType URowVector, [UReal; URowVector]) ;
  add_unqualified ("subtract", ReturnType UMatrix, [UReal; UMatrix]) ;
  add_unqualified ("sum", ReturnType UInt, [UArray UInt]) ;
  add_unqualified ("sum", ReturnType UReal, [UArray UReal]) ;
  add_unqualified ("sum", ReturnType UReal, [UVector]) ;
  add_unqualified ("sum", ReturnType UReal, [URowVector]) ;
  add_unqualified ("sum", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("svd_U", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("svd_V", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("symmetrize_from_lower_tri", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("tail", ReturnType URowVector, [URowVector; UInt]) ;
  add_unqualified ("tail", ReturnType UVector, [UVector; UInt]) ;
  List.iter
    ~f:(fun t ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( "tail"
            , ReturnType (bare_array_type (t, j))
            , [bare_array_type (t, j); UInt] ) )
        (List.range 1 4) )
    bare_types ;
  add_unqualified ("tcrossprod", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("to_array_1d", ReturnType (UArray UReal), [UMatrix]) ;
  add_unqualified ("to_array_1d", ReturnType (UArray UReal), [UVector]) ;
  add_unqualified ("to_array_1d", ReturnType (UArray UReal), [URowVector]) ;
  List.iter
    ~f:(fun i ->
      add_unqualified
        ("to_array_1d", ReturnType (UArray UReal), [bare_array_type (UReal, i)]) ;
      add_unqualified
        ("to_array_1d", ReturnType (UArray UInt), [bare_array_type (UInt, i)])
      )
    (List.range 1 10) ;
  add_unqualified
    ("to_array_2d", ReturnType (bare_array_type (UReal, 2)), [UMatrix]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UMatrix; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UMatrix; UInt; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UVector]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UVector; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UVector; UInt; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [URowVector]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UArray URowVector]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [URowVector; UInt; UInt]) ;
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [URowVector; UInt; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UArray UReal; UInt; UInt]) ;
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [UArray UReal; UInt; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UArray UInt; UInt; UInt]) ;
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [UArray UInt; UInt; UInt; UInt]) ;
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [bare_array_type (UReal, 2)]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [bare_array_type (UInt, 2)]) ;
  add_unqualified ("to_row_vector", ReturnType URowVector, [UMatrix]) ;
  add_unqualified ("to_row_vector", ReturnType URowVector, [UVector]) ;
  add_unqualified ("to_row_vector", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("to_row_vector", ReturnType URowVector, [UArray UReal]) ;
  add_unqualified ("to_row_vector", ReturnType URowVector, [UArray UInt]) ;
  add_unqualified ("to_vector", ReturnType UVector, [UMatrix]) ;
  add_unqualified ("to_vector", ReturnType UVector, [UVector]) ;
  add_unqualified ("to_vector", ReturnType UVector, [URowVector]) ;
  add_unqualified ("to_vector", ReturnType UVector, [UArray UReal]) ;
  add_unqualified ("to_vector", ReturnType UVector, [UArray UInt]) ;
  add_unqualified ("trace", ReturnType UReal, [UMatrix]) ;
  add_unqualified
    ("trace_gen_quad_form", ReturnType UReal, [UMatrix; UMatrix; UMatrix]) ;
  add_unqualified ("trace_quad_form", ReturnType UReal, [UMatrix; UVector]) ;
  add_unqualified ("trace_quad_form", ReturnType UReal, [UMatrix; UMatrix]) ;
  add_unqualified ("transpose", ReturnType URowVector, [UVector]) ;
  add_unqualified ("transpose", ReturnType UVector, [URowVector]) ;
  add_unqualified ("transpose", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("uniform_simplex", ReturnType UVector, [UInt]) ;
  add_unqualified ("variance", ReturnType UReal, [UArray UReal]) ;
  add_unqualified ("variance", ReturnType UReal, [UVector]) ;
  add_unqualified ("variance", ReturnType UReal, [URowVector]) ;
  add_unqualified ("variance", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("wishart_rng", ReturnType UMatrix, [UReal; UMatrix]) ;
  add_unqualified ("zeros_int_array", ReturnType (UArray UInt), [UInt]) ;
  add_unqualified ("zeros_array", ReturnType (UArray UReal), [UInt]) ;
  add_unqualified ("zeros_row_vector", ReturnType URowVector, [UInt]) ;
  add_unqualified ("zeros_vector", ReturnType UVector, [UInt]) ;
  (* Now add all the manually added stuff to the main hashtable used
     for type-checking *)
  Hashtbl.iteri manual_stan_math_signatures ~f:(fun ~key ~data ->
      List.iter data ~f:(fun data ->
          Hashtbl.add_multi stan_math_signatures ~key ~data ) )

let%expect_test "dist name suffix" =
  dist_name_suffix [] "normal" |> print_endline ;
  [%expect {| _lpdf |}]
