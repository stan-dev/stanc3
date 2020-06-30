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
let reduce_sum_functions = ["reduce_sum"; "reduce_sum_static"]
let is_reduce_sum_fn f = List.mem ~equal:String.equal reduce_sum_functions f

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

let assignmentoperator_to_stan_math_fn = function
  | Operator.Plus -> Some "assign_add"
  | Minus -> Some "assign_subtract"
  | Times -> Some "assign_multiply"
  | Divide -> Some "assign_divide"
  | EltTimes -> Some "assign_elt_times"
  | EltDivide -> Some "assign_elt_divide"
  | _ -> None

let assignmentoperator_stan_math_return_type assop arg_tys =
  assignmentoperator_to_stan_math_fn assop
  |> Option.bind ~f:(fun name -> stan_math_returntype name arg_tys)

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

let get_sigs name =
  let name = Utils.stdlib_distribution_name name in
  Hashtbl.find_multi stan_math_signatures name |> List.sort ~compare

let pp_math_sig ppf (rt, args) = UnsizedType.pp ppf (UFun (args, rt))

let pp_math_sigs ppf name =
  (Fmt.list ~sep:Fmt.cut pp_math_sig) ppf (get_sigs name)

let pretty_print_math_sigs = Fmt.strf "@[<v>@,%a@]" pp_math_sigs

let pretty_print_all_math_sigs ppf () =
  let open Fmt in
  let pp_sig ppf (name, (rt, args)) =
    pf ppf "%a %s(@[<hov 2>%a@])" UnsizedType.pp_returntype rt name
      (list ~sep:comma UnsizedType.pp)
      (List.map ~f:snd args)
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
  assignmentoperator_to_stan_math_fn op |> Option.map ~f:pretty_print_math_sigs

(* -- Some helper definitions to populate stan_math_signatures -- *)
let bare_types = function
  | 0 -> UnsizedType.UInt
  | 1 -> UReal
  | 2 -> UVector
  | 3 -> URowVector
  | 4 -> UMatrix
  | i -> raise_s [%sexp (i : int)]

let bare_types_size = 5

let vector_types = function
  | 0 -> UnsizedType.UReal
  | 1 -> UArray UReal
  | 2 -> UVector
  | 3 -> URowVector
  | i -> raise_s [%sexp (i : int)]

let vector_types_size = 4

let primitive_types = function
  | 0 -> UnsizedType.UInt
  | 1 -> UReal
  | i -> raise_s [%sexp (i : int)]

let primitive_types_size = 2

let all_vector_types = function
  | 0 -> UnsizedType.UReal
  | 1 -> UArray UReal
  | 2 -> UVector
  | 3 -> URowVector
  | 4 -> UInt
  | 5 -> UArray UInt
  | i -> raise_s [%sexp (i : int)]

let all_vector_types_size = 6

let add_qualified (name, rt, argts) =
  Hashtbl.add_multi stan_math_signatures ~key:name ~data:(rt, argts)

let add_nullary name = add_unqualified (name, UnsizedType.ReturnType UReal, [])

let add_binary name =
  add_unqualified (name, ReturnType UReal, [UnsizedType.UReal; UReal])

let add_ternary name =
  add_unqualified (name, ReturnType UReal, [UReal; UReal; UReal])

let for_all_vector_types s =
  for i = 0 to all_vector_types_size - 1 do
    s (all_vector_types i)
  done

let for_vector_types s =
  for i = 0 to vector_types_size - 1 do
    s (vector_types i)
  done

(* -- Start populating stan_math_signaturess -- *)
let () =
  List.iter declarative_fnsigs ~f:(fun (key, rt, args) ->
      Hashtbl.add_multi stan_math_signatures ~key ~data:(rt, args) ) ;
  add_unqualified ("abs", ReturnType UInt, [UInt]) ;
  add_unqualified ("abs", ReturnType UReal, [UReal]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ("add", ReturnType (bare_types i), [bare_types i; bare_types i])
  done ;
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
            , ReturnType UVector ) )
      ; (AutoDiffable, UVector); (AutoDiffable, UVector)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ] ) ;
  add_qualified
    ( "algebra_solver"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector ) )
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
            , ReturnType UVector ) )
      ; (AutoDiffable, UVector); (AutoDiffable, UVector)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ] ) ;
  add_qualified
    ( "algebra_solver_newton"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector ) )
      ; (AutoDiffable, UVector); (AutoDiffable, UVector)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt); (DataOnly, UReal)
      ; (DataOnly, UReal); (DataOnly, UReal) ] ) ;
  for i = 1 to 8 - 1 do
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (UInt, i))
      , [bare_array_type (UInt, i); bare_array_type (UInt, i)] ) ;
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (UReal, i))
      , [bare_array_type (UReal, i); bare_array_type (UReal, i)] ) ;
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (UVector, i))
      , [bare_array_type (UVector, i); bare_array_type (UVector, i)] ) ;
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (URowVector, i))
      , [bare_array_type (URowVector, i); bare_array_type (URowVector, i)] ) ;
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (UMatrix, i))
      , [bare_array_type (UMatrix, i); bare_array_type (UMatrix, i)] )
  done ;
  add_unqualified ("assign_multiply", Void, [UInt; UInt]) ;
  add_unqualified ("assign_multiply", Void, [UMatrix; UMatrix]) ;
  add_unqualified ("assign_multiply", Void, [UMatrix; UReal]) ;
  add_unqualified ("assign_multiply", Void, [UReal; UReal]) ;
  add_unqualified ("assign_multiply", Void, [URowVector; UReal]) ;
  add_unqualified ("assign_multiply", Void, [UMatrix; UInt]) ;
  add_unqualified ("assign_multiply", Void, [UReal; UInt]) ;
  add_unqualified ("assign_multiply", Void, [URowVector; UInt]) ;
  add_unqualified ("assign_multiply", Void, [URowVector; UMatrix]) ;
  add_unqualified ("assign_multiply", Void, [UVector; UReal]) ;
  add_unqualified ("assign_multiply", Void, [UVector; UInt]) ;
  add_unqualified ("assign_add", Void, [UInt; UInt]) ;
  add_unqualified ("assign_add", Void, [UMatrix; UMatrix]) ;
  add_unqualified ("assign_add", Void, [UMatrix; UReal]) ;
  add_unqualified ("assign_add", Void, [UReal; UReal]) ;
  add_unqualified ("assign_add", Void, [URowVector; UReal]) ;
  add_unqualified ("assign_add", Void, [UMatrix; UInt]) ;
  add_unqualified ("assign_add", Void, [UReal; UInt]) ;
  add_unqualified ("assign_add", Void, [URowVector; UInt]) ;
  add_unqualified ("assign_add", Void, [URowVector; URowVector]) ;
  add_unqualified ("assign_add", Void, [UVector; UReal]) ;
  add_unqualified ("assign_add", Void, [UVector; UInt]) ;
  add_unqualified ("assign_add", Void, [UVector; UVector]) ;
  add_unqualified ("assign_subtract", Void, [UInt; UInt]) ;
  add_unqualified ("assign_subtract", Void, [UMatrix; UMatrix]) ;
  add_unqualified ("assign_subtract", Void, [UMatrix; UReal]) ;
  add_unqualified ("assign_subtract", Void, [UReal; UReal]) ;
  add_unqualified ("assign_subtract", Void, [URowVector; UReal]) ;
  add_unqualified ("assign_subtract", Void, [UMatrix; UInt]) ;
  add_unqualified ("assign_subtract", Void, [UReal; UInt]) ;
  add_unqualified ("assign_subtract", Void, [URowVector; UInt]) ;
  add_unqualified ("assign_subtract", Void, [URowVector; URowVector]) ;
  add_unqualified ("assign_subtract", Void, [UVector; UReal]) ;
  add_unqualified ("assign_subtract", Void, [UVector; UInt]) ;
  add_unqualified ("assign_subtract", Void, [UVector; UVector]) ;
  add_unqualified ("assign_elt_times", Void, [UMatrix; UMatrix]) ;
  add_unqualified ("assign_elt_times", Void, [URowVector; URowVector]) ;
  add_unqualified ("assign_elt_times", Void, [UVector; UVector]) ;
  add_unqualified ("assign_elt_divide", Void, [UMatrix; UMatrix]) ;
  add_unqualified ("assign_elt_divide", Void, [UMatrix; UReal]) ;
  add_unqualified ("assign_elt_divide", Void, [URowVector; UReal]) ;
  add_unqualified ("assign_elt_divide", Void, [UMatrix; UInt]) ;
  add_unqualified ("assign_elt_divide", Void, [URowVector; UInt]) ;
  add_unqualified ("assign_elt_divide", Void, [URowVector; URowVector]) ;
  add_unqualified ("assign_elt_divide", Void, [UVector; UReal]) ;
  add_unqualified ("assign_elt_divide", Void, [UVector; UInt]) ;
  add_unqualified ("assign_elt_divide", Void, [UVector; UVector]) ;
  add_unqualified ("assign_divide", Void, [UInt; UInt]) ;
  add_unqualified ("assign_divide", Void, [UMatrix; UReal]) ;
  add_unqualified ("assign_divide", Void, [UReal; UReal]) ;
  add_unqualified ("assign_divide", Void, [URowVector; UReal]) ;
  add_unqualified ("assign_divide", Void, [UVector; UReal]) ;
  add_unqualified ("assign_divide", Void, [UMatrix; UInt]) ;
  add_unqualified ("assign_divide", Void, [UReal; UInt]) ;
  add_unqualified ("assign_divide", Void, [URowVector; UInt]) ;
  add_unqualified ("assign_divide", Void, [UVector; UInt]) ;
  add_binary "atan2" ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UReal; UVector] ) ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UVector; UVector] ) ;
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
    , [bare_array_type (UInt, 1); URowVector; UReal; UVector] ) ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); URowVector; UVector; UVector] ) ;
  add_unqualified ("bessel_first_kind", ReturnType UReal, [UInt; UReal]) ;
  add_unqualified ("bessel_second_kind", ReturnType UReal, [UInt; UReal]) ;
  (* XXX For some reason beta_proportion_rng doesn't take ints as first arg *)
  for_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "beta_proportion_rng"
            , ReturnType (rng_return_type UReal [t; u])
            , [t; u] ) ) ) ;
  add_unqualified ("binary_log_loss", ReturnType UReal, [UInt; UReal]) ;
  add_binary "binomial_coefficient_log" ;
  add_unqualified
    ("block", ReturnType UMatrix, [UMatrix; UInt; UInt; UInt; UInt]) ;
  add_unqualified ("categorical_rng", ReturnType UInt, [UVector]) ;
  add_unqualified ("categorical_logit_rng", ReturnType UInt, [UVector]) ;
  add_unqualified
    ( "categorical_logit_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UVector; UMatrix] ) ;
  add_unqualified
    ( "categorical_logit_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UMatrix] ) ;
  add_unqualified
    ( "categorical_logit_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); URowVector; UVector; UMatrix] ) ;
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
  add_unqualified ("cholesky_decompose", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("choose", ReturnType UInt, [UInt; UInt]) ;
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
    ( "cov_exp_quad"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; UReal] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType UMatrix
    , [bare_array_type (URowVector, 1); UReal; UReal] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); bare_array_type (UVector, 1); UReal; UReal]
    ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType UMatrix
    , [ bare_array_type (URowVector, 1)
      ; bare_array_type (URowVector, 1)
      ; UReal; UReal ] ) ;
  add_unqualified ("crossprod", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ( "csr_matrix_times_vector"
    , ReturnType UVector
    , [ UInt; UInt; UVector
      ; bare_array_type (UInt, 1)
      ; bare_array_type (UInt, 1)
      ; UVector ] ) ;
  add_unqualified
    ( "csr_to_dense_matrix"
    , ReturnType UMatrix
    , [ UInt; UInt; UVector
      ; bare_array_type (UInt, 1)
      ; bare_array_type (UInt, 1) ] ) ;
  add_unqualified ("csr_extract_w", ReturnType UVector, [UMatrix]) ;
  add_unqualified
    ("csr_extract_v", ReturnType (bare_array_type (UInt, 1)), [UMatrix]) ;
  add_unqualified
    ("csr_extract_u", ReturnType (bare_array_type (UInt, 1)), [UMatrix]) ;
  add_unqualified
    ( "cumulative_sum"
    , ReturnType (bare_array_type (UReal, 1))
    , [bare_array_type (UReal, 1)] ) ;
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
  add_unqualified ("dims", ReturnType (bare_array_type (UInt, 1)), [UInt]) ;
  add_unqualified ("dims", ReturnType (bare_array_type (UInt, 1)), [UReal]) ;
  add_unqualified ("dims", ReturnType (bare_array_type (UInt, 1)), [UVector]) ;
  add_unqualified ("dims", ReturnType (bare_array_type (UInt, 1)), [URowVector]) ;
  add_unqualified ("dims", ReturnType (bare_array_type (UInt, 1)), [UMatrix]) ;
  for i = 0 to 8 - 1 do
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (UInt, 1))
      , [bare_array_type (UInt, i + 1)] ) ;
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (UInt, 1))
      , [bare_array_type (UReal, i + 1)] ) ;
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (UInt, 1))
      , [bare_array_type (UVector, i + 1)] ) ;
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (UInt, 1))
      , [bare_array_type (URowVector, i + 1)] ) ;
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (UInt, 1))
      , [bare_array_type (UMatrix, i + 1)] )
  done ;
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
    ( "dot_product"
    , ReturnType UReal
    , [bare_array_type (UReal, 1); bare_array_type (UReal, 1)] ) ;
  add_unqualified ("dot_self", ReturnType UReal, [UVector]) ;
  add_unqualified ("dot_self", ReturnType UReal, [URowVector]) ;
  add_nullary "e" ;
  add_unqualified ("eigenvalues_sym", ReturnType UVector, [UMatrix]) ;
  add_unqualified ("eigenvectors_sym", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("qr_Q", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("qr_R", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("qr_thin_Q", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("qr_thin_R", ReturnType UMatrix, [UMatrix]) ;
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
  add_unqualified ("elt_multiply", ReturnType UVector, [UVector; UVector]) ;
  add_unqualified
    ("elt_multiply", ReturnType URowVector, [URowVector; URowVector]) ;
  add_unqualified ("elt_multiply", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("falling_factorial", ReturnType UReal, [UReal; UInt]) ;
  add_unqualified ("falling_factorial", ReturnType UInt, [UInt; UInt]) ;
  add_binary "fdim" ;
  add_ternary "fma" ;
  add_binary "fmax" ;
  add_binary "fmin" ;
  add_binary "fmod" ;
  add_binary "gamma_p" ;
  add_binary "gamma_q" ;
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
  add_unqualified
    ("gp_dot_prod_cov", ReturnType UMatrix, [bare_array_type (UReal, 1); UReal]) ;
  add_unqualified
    ( "gp_dot_prod_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); bare_array_type (UReal, 1); UReal] ) ;
  add_unqualified
    ( "gp_dot_prod_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); bare_array_type (UReal, 1); UReal] ) ;
  add_unqualified
    ( "gp_dot_prod_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal] ) ;
  add_unqualified
    ( "gp_dot_prod_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); bare_array_type (UVector, 1); UReal] ) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); bare_array_type (UVector, 1); UReal; UReal]
    ) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; bare_array_type (UReal, 1)] ) ;
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [ bare_array_type (UVector, 1)
      ; bare_array_type (UVector, 1)
      ; UReal
      ; bare_array_type (UReal, 1) ] ) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); bare_array_type (UVector, 1); UReal; UReal]
    ) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; bare_array_type (UReal, 1)] ) ;
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [ bare_array_type (UVector, 1)
      ; bare_array_type (UVector, 1)
      ; UReal
      ; bare_array_type (UReal, 1) ] ) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); bare_array_type (UVector, 1); UReal; UReal]
    ) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; bare_array_type (UReal, 1)] ) ;
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [ bare_array_type (UVector, 1)
      ; bare_array_type (UVector, 1)
      ; UReal
      ; bare_array_type (UReal, 1) ] ) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); bare_array_type (UReal, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; UReal] ) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); bare_array_type (UVector, 1); UReal; UReal]
    ) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; bare_array_type (UReal, 1)] ) ;
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [ bare_array_type (UVector, 1)
      ; bare_array_type (UVector, 1)
      ; UReal
      ; bare_array_type (UReal, 1) ] ) ;
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); UReal; UReal; UReal] ) ;
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [ bare_array_type (UReal, 1)
      ; bare_array_type (UReal, 1)
      ; UReal; UReal; UReal ] ) ;
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [bare_array_type (UVector, 1); UReal; UReal; UReal] ) ;
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [ bare_array_type (UVector, 1)
      ; bare_array_type (UVector, 1)
      ; UReal; UReal; UReal ] ) ;
  (* ; add_nullary ("get_lp")   *)
  add_unqualified ("head", ReturnType URowVector, [URowVector; UInt]) ;
  add_unqualified ("head", ReturnType UVector, [UVector; UInt]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ( "head"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_array_type (bare_types i, 1); UInt] ) ;
    add_unqualified
      ( "head"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_array_type (bare_types i, 2); UInt] ) ;
    add_unqualified
      ( "head"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_array_type (bare_types i, 3); UInt] )
  done ;
  add_unqualified
    ("hmm_marginal", ReturnType UReal, [UMatrix; UMatrix; UVector]) ;
  add_qualified
    ( "hmm_hidden_state_prob"
    , ReturnType UMatrix
    , [(DataOnly, UMatrix); (DataOnly, UMatrix); (DataOnly, UVector)] ) ;
  add_unqualified
    ( "hmm_latent_rng"
    , ReturnType (bare_array_type (UInt, 1))
    , [UMatrix; UMatrix; UVector] ) ;
  add_unqualified
    ("hypergeometric_log", ReturnType UReal, [UInt; UInt; UInt; UInt]) ;
  add_unqualified
    ("hypergeometric_lpmf", ReturnType UReal, [UInt; UInt; UInt; UInt]) ;
  add_unqualified ("hypergeometric_rng", ReturnType UInt, [UInt; UInt; UInt]) ;
  add_binary "hypot" ;
  add_unqualified ("identity_matrix", ReturnType UMatrix, [UInt]) ;
  for j = 0 to 8 - 1 do
    add_unqualified
      ( "if_else"
      , ReturnType (bare_array_type (UReal, j))
      , [UInt; bare_array_type (UReal, j); bare_array_type (UReal, j)] ) ;
    add_unqualified
      ( "if_else"
      , ReturnType (bare_array_type (UInt, j))
      , [UInt; bare_array_type (UInt, j); bare_array_type (UInt, j)] ) ;
    add_unqualified
      ( "if_else"
      , ReturnType (bare_array_type (UVector, j))
      , [UInt; bare_array_type (UVector, j); bare_array_type (UVector, j)] ) ;
    add_unqualified
      ( "if_else"
      , ReturnType (bare_array_type (URowVector, j))
      , [UInt; bare_array_type (URowVector, j); bare_array_type (URowVector, j)]
      ) ;
    add_unqualified
      ( "if_else"
      , ReturnType (bare_array_type (UMatrix, j))
      , [UInt; bare_array_type (UMatrix, j); bare_array_type (UMatrix, j)] )
  done ;
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
            , ReturnType UReal ) )
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
            , ReturnType UReal ) )
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
            , ReturnType (UArray UReal) ) )
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
            , ReturnType (UArray UReal) ) )
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
            , ReturnType (UArray UReal) ) )
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
            , ReturnType (UArray UReal) ) )
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
            , ReturnType (UArray UReal) ) )
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
            , ReturnType (UArray UReal) ) )
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
            , ReturnType (UArray UReal) ) )
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
  add_binary "lbeta" ;
  add_binary "lchoose" ;
  add_unqualified
    ("linspaced_array", ReturnType (UArray UReal), [UInt; UReal; UReal]) ;
  add_unqualified
    ("linspaced_row_vector", ReturnType URowVector, [UInt; UReal; UReal]) ;
  add_unqualified ("linspaced_vector", ReturnType UVector, [UInt; UReal; UReal]) ;
  add_unqualified ("lkj_corr_cholesky_log", ReturnType UReal, [UMatrix; UReal]) ;
  add_unqualified ("lkj_corr_cholesky_lpdf", ReturnType UReal, [UMatrix; UReal]) ;
  add_unqualified ("lkj_corr_cholesky_rng", ReturnType UMatrix, [UInt; UReal]) ;
  add_unqualified ("lkj_corr_log", ReturnType UReal, [UMatrix; UReal]) ;
  add_unqualified ("lkj_corr_lpdf", ReturnType UReal, [UMatrix; UReal]) ;
  add_unqualified ("lkj_corr_rng", ReturnType UMatrix, [UInt; UReal]) ;
  add_unqualified
    ("lkj_cov_log", ReturnType UReal, [UMatrix; UVector; UVector; UReal]) ;
  add_unqualified ("lmgamma", ReturnType UReal, [UInt; UReal]) ;
  add_binary "lmultiply" ;
  add_nullary "log10" ;
  add_nullary "log2" ;
  add_unqualified ("log_determinant", ReturnType UReal, [UMatrix]) ;
  add_binary "log_diff_exp" ;
  add_binary "log_falling_factorial" ;
  add_ternary "log_mix" ;
  for i = 1 to vector_types_size - 1 do
    for j = 1 to vector_types_size - 1 do
      add_unqualified
        ("log_mix", ReturnType UReal, [vector_types i; vector_types j])
    done ;
    add_unqualified
      ( "log_mix"
      , ReturnType UReal
      , [vector_types i; bare_array_type (UVector, 1)] ) ;
    add_unqualified
      ( "log_mix"
      , ReturnType UReal
      , [vector_types i; bare_array_type (URowVector, 1)] )
  done ;
  add_binary "log_rising_factorial" ;
  add_unqualified ("log_softmax", ReturnType UVector, [UVector]) ;
  add_unqualified
    ("log_sum_exp", ReturnType UReal, [bare_array_type (UReal, 1)]) ;
  add_unqualified ("log_sum_exp", ReturnType UReal, [UVector]) ;
  add_unqualified ("log_sum_exp", ReturnType UReal, [URowVector]) ;
  add_unqualified ("log_sum_exp", ReturnType UReal, [UMatrix]) ;
  add_binary "log_sum_exp" ;
  for i = 0 to primitive_types_size - 1 do
    add_unqualified ("logical_negation", ReturnType UInt, [primitive_types i]) ;
    for j = 0 to primitive_types_size - 1 do
      add_unqualified
        ("logical_or", ReturnType UInt, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_and", ReturnType UInt, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_eq", ReturnType UInt, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_neq", ReturnType UInt, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_lt", ReturnType UInt, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_lte", ReturnType UInt, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_gt", ReturnType UInt, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_gte", ReturnType UInt, [primitive_types i; primitive_types j])
    done
  done ;
  add_nullary "machine_precision" ;
  add_qualified
    ( "map_rect"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector ) )
      ; (AutoDiffable, UVector)
      ; (AutoDiffable, UArray UVector)
      ; (DataOnly, UArray (UArray UReal))
      ; (DataOnly, UArray (UArray UInt)) ] ) ;
  add_unqualified ("matrix_exp", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ("matrix_exp_multiply", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("max", ReturnType UInt, [bare_array_type (UInt, 1)]) ;
  add_unqualified ("max", ReturnType UReal, [bare_array_type (UReal, 1)]) ;
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
  add_unqualified ("mean", ReturnType UReal, [bare_array_type (UReal, 1)]) ;
  add_unqualified ("mean", ReturnType UReal, [UVector]) ;
  add_unqualified ("mean", ReturnType UReal, [URowVector]) ;
  add_unqualified ("mean", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("min", ReturnType UInt, [bare_array_type (UInt, 1)]) ;
  add_unqualified ("min", ReturnType UReal, [bare_array_type (UReal, 1)]) ;
  add_unqualified ("min", ReturnType UReal, [UVector]) ;
  add_unqualified ("min", ReturnType UReal, [URowVector]) ;
  add_unqualified ("min", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("min", ReturnType UInt, [UInt; UInt]) ;
  add_unqualified ("minus", ReturnType UInt, [UInt]) ;
  add_unqualified ("minus", ReturnType UReal, [UReal]) ;
  add_unqualified ("minus", ReturnType UVector, [UVector]) ;
  add_unqualified ("minus", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("minus", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ("modified_bessel_first_kind", ReturnType UReal, [UInt; UReal]) ;
  add_unqualified
    ("modified_bessel_second_kind", ReturnType UReal, [UInt; UReal]) ;
  add_unqualified ("modulus", ReturnType UInt, [UInt; UInt]) ;
  add_unqualified ("multi_normal_rng", ReturnType UVector, [UVector; UMatrix]) ;
  add_unqualified
    ( "multi_normal_rng"
    , ReturnType (bare_array_type (UVector, 1))
    , [bare_array_type (UVector, 1); UMatrix] ) ;
  add_unqualified
    ("multi_normal_rng", ReturnType UVector, [URowVector; UMatrix]) ;
  add_unqualified
    ( "multi_normal_rng"
    , ReturnType (bare_array_type (UVector, 1))
    , [bare_array_type (URowVector, 1); UMatrix] ) ;
  add_unqualified
    ("multi_normal_cholesky_rng", ReturnType UVector, [UVector; UMatrix]) ;
  add_unqualified
    ( "multi_normal_cholesky_rng"
    , ReturnType (bare_array_type (UVector, 1))
    , [bare_array_type (UVector, 1); UMatrix] ) ;
  add_unqualified
    ("multi_normal_cholesky_rng", ReturnType UVector, [URowVector; UMatrix]) ;
  add_unqualified
    ( "multi_normal_cholesky_rng"
    , ReturnType (bare_array_type (UVector, 1))
    , [bare_array_type (URowVector, 1); UMatrix] ) ;
  add_unqualified
    ("multi_student_t_rng", ReturnType UVector, [UReal; UVector; UMatrix]) ;
  add_unqualified
    ( "multi_student_t_rng"
    , ReturnType (bare_array_type (UVector, 1))
    , [UReal; bare_array_type (UVector, 1); UMatrix] ) ;
  add_unqualified
    ("multi_student_t_rng", ReturnType UVector, [UReal; URowVector; UMatrix]) ;
  add_unqualified
    ( "multi_student_t_rng"
    , ReturnType (bare_array_type (UVector, 1))
    , [UReal; bare_array_type (URowVector, 1); UMatrix] ) ;
  add_unqualified
    ("multinomial_log", ReturnType UReal, [bare_array_type (UInt, 1); UVector]) ;
  add_unqualified
    ("multinomial_lpmf", ReturnType UReal, [bare_array_type (UInt, 1); UVector]) ;
  add_unqualified
    ("multinomial_rng", ReturnType (bare_array_type (UInt, 1)), [UVector; UInt]) ;
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
  add_binary "multiply_log" ;
  add_unqualified
    ("multiply_lower_tri_self_transpose", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UReal; UVector; UReal] ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UVector; UVector; UReal] ) ;
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
    , [bare_array_type (UInt, 1); URowVector; UReal; UVector; UReal] ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); URowVector; UVector; UVector; UReal] ) ;
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
  for i = 1 to 10 - 1 do
    add_unqualified
      ("num_elements", ReturnType UInt, [bare_array_type (UInt, i)]) ;
    add_unqualified
      ("num_elements", ReturnType UInt, [bare_array_type (UReal, i)]) ;
    add_unqualified
      ("num_elements", ReturnType UInt, [bare_array_type (UMatrix, i)]) ;
    add_unqualified
      ("num_elements", ReturnType UInt, [bare_array_type (URowVector, i)]) ;
    add_unqualified
      ("num_elements", ReturnType UInt, [bare_array_type (UVector, i)])
  done ;
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
    , [bare_array_type (UInt, 1); UMatrix; UVector; UVector] ) ;
  add_unqualified
    ( "ordered_logistic_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); URowVector; UVector; UVector] ) ;
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
    ( "ordered_logistic_log"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UVector; UVector] ) ;
  add_unqualified
    ( "ordered_logistic_log"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UVector; bare_array_type (UVector, 1)] ) ;
  add_unqualified
    ("ordered_logistic_lpmf", ReturnType UReal, [UInt; UReal; UVector]) ;
  add_unqualified
    ( "ordered_logistic_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UVector; UVector] ) ;
  add_unqualified
    ( "ordered_logistic_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UVector; bare_array_type (UVector, 1)] ) ;
  add_unqualified ("ordered_logistic_rng", ReturnType UInt, [UReal; UVector]) ;
  add_unqualified
    ("ordered_probit_log", ReturnType UReal, [UInt; UReal; UVector]) ;
  add_unqualified
    ( "ordered_probit_log"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UVector; UVector] ) ;
  add_unqualified
    ( "ordered_probit_log"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UVector; bare_array_type (UVector, 1)] ) ;
  add_unqualified
    ("ordered_probit_lpmf", ReturnType UReal, [UInt; UReal; UVector]) ;
  add_unqualified
    ( "ordered_probit_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UReal; UVector] ) ;
  add_unqualified
    ( "ordered_probit_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UReal; bare_array_type (UVector, 1)] ) ;
  add_unqualified ("ordered_probit_rng", ReturnType UInt, [UReal; UVector]) ;
  add_binary "owens_t" ;
  add_nullary "pi" ;
  add_unqualified ("plus", ReturnType UInt, [UInt]) ;
  add_unqualified ("plus", ReturnType UReal, [UReal]) ;
  add_unqualified ("plus", ReturnType UVector, [UVector]) ;
  add_unqualified ("plus", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("plus", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UReal; UVector] ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UVector; UVector] ) ;
  add_unqualified
    ("poisson_log_glm_lpmf", ReturnType UReal, [UInt; UMatrix; UReal; UVector]) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UVector] ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); URowVector; UReal; UVector] ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); URowVector; UVector; UVector] ) ;
  add_nullary "positive_infinity" ;
  add_binary "pow" ;
  add_unqualified ("prod", ReturnType UInt, [bare_array_type (UInt, 1)]) ;
  add_unqualified ("prod", ReturnType UReal, [bare_array_type (UReal, 1)]) ;
  add_unqualified ("prod", ReturnType UReal, [UVector]) ;
  add_unqualified ("prod", ReturnType UReal, [URowVector]) ;
  add_unqualified ("prod", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("quad_form", ReturnType UReal, [UMatrix; UVector]) ;
  add_unqualified ("quad_form", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("quad_form_sym", ReturnType UReal, [UMatrix; UVector]) ;
  add_unqualified ("quad_form_sym", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("quad_form_diag", ReturnType UMatrix, [UMatrix; UVector]) ;
  add_unqualified ("quad_form_diag", ReturnType UMatrix, [UMatrix; URowVector]) ;
  add_unqualified ("rank", ReturnType UInt, [bare_array_type (UInt, 1); UInt]) ;
  add_unqualified ("rank", ReturnType UInt, [bare_array_type (UReal, 1); UInt]) ;
  add_unqualified ("rank", ReturnType UInt, [UVector; UInt]) ;
  add_unqualified ("rank", ReturnType UInt, [URowVector; UInt]) ;
  add_unqualified ("append_row", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("append_row", ReturnType UMatrix, [URowVector; UMatrix]) ;
  add_unqualified ("append_row", ReturnType UMatrix, [UMatrix; URowVector]) ;
  add_unqualified ("append_row", ReturnType UMatrix, [URowVector; URowVector]) ;
  add_unqualified ("append_row", ReturnType UVector, [UVector; UVector]) ;
  add_unqualified ("append_row", ReturnType UVector, [UReal; UVector]) ;
  add_unqualified ("append_row", ReturnType UVector, [UVector; UReal]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ( "rep_array"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_types i; UInt] ) ;
    add_unqualified
      ( "rep_array"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_types i; UInt; UInt] ) ;
    add_unqualified
      ( "rep_array"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_types i; UInt; UInt; UInt] ) ;
    for j = 1 to 3 - 1 do
      add_unqualified
        ( "rep_array"
        , ReturnType (bare_array_type (bare_types i, j + 1))
        , [bare_array_type (bare_types i, j); UInt] ) ;
      add_unqualified
        ( "rep_array"
        , ReturnType (bare_array_type (bare_types i, j + 2))
        , [bare_array_type (bare_types i, j); UInt; UInt] ) ;
      add_unqualified
        ( "rep_array"
        , ReturnType (bare_array_type (bare_types i, j + 3))
        , [bare_array_type (bare_types i, j); UInt; UInt; UInt] )
    done
  done ;
  add_unqualified ("rep_matrix", ReturnType UMatrix, [UReal; UInt; UInt]) ;
  add_unqualified ("rep_matrix", ReturnType UMatrix, [UVector; UInt]) ;
  add_unqualified ("rep_matrix", ReturnType UMatrix, [URowVector; UInt]) ;
  add_unqualified ("rep_row_vector", ReturnType URowVector, [UReal; UInt]) ;
  add_unqualified ("rep_vector", ReturnType UVector, [UReal; UInt]) ;
  for i = 0 to 7 do
    add_unqualified
      ( "reverse"
      , ReturnType (bare_array_type (UVector, i))
      , [bare_array_type (UVector, i)] ) ;
    add_unqualified
      ( "reverse"
      , ReturnType (bare_array_type (URowVector, i))
      , [bare_array_type (URowVector, i)] )
  done ;
  for i = 1 to 7 do
    add_unqualified
      ( "reverse"
      , ReturnType (bare_array_type (UInt, i))
      , [bare_array_type (UInt, i)] ) ;
    add_unqualified
      ( "reverse"
      , ReturnType (bare_array_type (UReal, i))
      , [bare_array_type (UReal, i)] ) ;
    add_unqualified
      ( "reverse"
      , ReturnType (bare_array_type (UMatrix, i))
      , [bare_array_type (UMatrix, i)] )
  done ;
  add_unqualified ("rising_factorial", ReturnType UReal, [UReal; UInt]) ;
  add_unqualified ("rising_factorial", ReturnType UInt, [UInt; UInt]) ;
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
  add_unqualified ("sd", ReturnType UReal, [bare_array_type (UReal, 1)]) ;
  add_unqualified ("sd", ReturnType UReal, [UVector]) ;
  add_unqualified ("sd", ReturnType UReal, [URowVector]) ;
  add_unqualified ("sd", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("segment", ReturnType URowVector, [URowVector; UInt; UInt]) ;
  add_unqualified ("segment", ReturnType UVector, [UVector; UInt; UInt]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ( "segment"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_array_type (bare_types i, 1); UInt; UInt] ) ;
    add_unqualified
      ( "segment"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_array_type (bare_types i, 2); UInt; UInt] ) ;
    add_unqualified
      ( "segment"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_array_type (bare_types i, 3); UInt; UInt] )
  done ;
  add_unqualified ("singular_values", ReturnType UVector, [UMatrix]) ;
  for i = 1 to 8 - 1 do
    add_unqualified ("size", ReturnType UInt, [bare_array_type (UInt, i)]) ;
    add_unqualified ("size", ReturnType UInt, [bare_array_type (UReal, i)]) ;
    add_unqualified ("size", ReturnType UInt, [bare_array_type (UVector, i)]) ;
    add_unqualified ("size", ReturnType UInt, [bare_array_type (URowVector, i)]) ;
    add_unqualified ("size", ReturnType UInt, [bare_array_type (UMatrix, i)])
  done ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified ("size", ReturnType UInt, [bare_types i])
  done ;
  add_unqualified ("softmax", ReturnType UVector, [UVector]) ;
  add_unqualified
    ( "sort_asc"
    , ReturnType (bare_array_type (UInt, 1))
    , [bare_array_type (UInt, 1)] ) ;
  add_unqualified
    ( "sort_asc"
    , ReturnType (bare_array_type (UReal, 1))
    , [bare_array_type (UReal, 1)] ) ;
  add_unqualified ("sort_asc", ReturnType UVector, [UVector]) ;
  add_unqualified ("sort_asc", ReturnType URowVector, [URowVector]) ;
  add_unqualified
    ( "sort_desc"
    , ReturnType (bare_array_type (UInt, 1))
    , [bare_array_type (UInt, 1)] ) ;
  add_unqualified
    ( "sort_desc"
    , ReturnType (bare_array_type (UReal, 1))
    , [bare_array_type (UReal, 1)] ) ;
  add_unqualified ("sort_desc", ReturnType UVector, [UVector]) ;
  add_unqualified ("sort_desc", ReturnType URowVector, [URowVector]) ;
  add_unqualified
    ( "sort_indices_asc"
    , ReturnType (bare_array_type (UInt, 1))
    , [bare_array_type (UInt, 1)] ) ;
  add_unqualified
    ( "sort_indices_asc"
    , ReturnType (bare_array_type (UInt, 1))
    , [bare_array_type (UReal, 1)] ) ;
  add_unqualified
    ("sort_indices_asc", ReturnType (bare_array_type (UInt, 1)), [UVector]) ;
  add_unqualified
    ("sort_indices_asc", ReturnType (bare_array_type (UInt, 1)), [URowVector]) ;
  add_unqualified
    ( "sort_indices_desc"
    , ReturnType (bare_array_type (UInt, 1))
    , [bare_array_type (UInt, 1)] ) ;
  add_unqualified
    ( "sort_indices_desc"
    , ReturnType (bare_array_type (UInt, 1))
    , [bare_array_type (UReal, 1)] ) ;
  add_unqualified
    ("sort_indices_desc", ReturnType (bare_array_type (UInt, 1)), [UVector]) ;
  add_unqualified
    ("sort_indices_desc", ReturnType (bare_array_type (UInt, 1)), [URowVector]) ;
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
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ("subtract", ReturnType (bare_types i), [bare_types i; bare_types i])
  done ;
  add_unqualified ("subtract", ReturnType UVector, [UVector; UReal]) ;
  add_unqualified ("subtract", ReturnType URowVector, [URowVector; UReal]) ;
  add_unqualified ("subtract", ReturnType UMatrix, [UMatrix; UReal]) ;
  add_unqualified ("subtract", ReturnType UVector, [UReal; UVector]) ;
  add_unqualified ("subtract", ReturnType URowVector, [UReal; URowVector]) ;
  add_unqualified ("subtract", ReturnType UMatrix, [UReal; UMatrix]) ;
  add_unqualified ("sum", ReturnType UInt, [bare_array_type (UInt, 1)]) ;
  add_unqualified ("sum", ReturnType UReal, [bare_array_type (UReal, 1)]) ;
  add_unqualified ("sum", ReturnType UReal, [UVector]) ;
  add_unqualified ("sum", ReturnType UReal, [URowVector]) ;
  add_unqualified ("sum", ReturnType UReal, [UMatrix]) ;
  add_unqualified ("tail", ReturnType URowVector, [URowVector; UInt]) ;
  add_unqualified ("tail", ReturnType UVector, [UVector; UInt]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ( "tail"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_array_type (bare_types i, 1); UInt] ) ;
    add_unqualified
      ( "tail"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_array_type (bare_types i, 2); UInt] ) ;
    add_unqualified
      ( "tail"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_array_type (bare_types i, 3); UInt] )
  done ;
  add_unqualified ("tcrossprod", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified
    ("to_array_1d", ReturnType (bare_array_type (UReal, 1)), [UMatrix]) ;
  add_unqualified
    ("to_array_1d", ReturnType (bare_array_type (UReal, 1)), [UVector]) ;
  add_unqualified
    ("to_array_1d", ReturnType (bare_array_type (UReal, 1)), [URowVector]) ;
  for i = 1 to 10 - 1 do
    add_unqualified
      ( "to_array_1d"
      , ReturnType (bare_array_type (UReal, 1))
      , [bare_array_type (UReal, i)] ) ;
    add_unqualified
      ( "to_array_1d"
      , ReturnType (bare_array_type (UInt, 1))
      , [bare_array_type (UInt, i)] )
  done ;
  add_unqualified
    ("to_array_2d", ReturnType (bare_array_type (UReal, 2)), [UMatrix]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UMatrix; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UMatrix; UInt; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UVector]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UVector; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [UVector; UInt; UInt; UInt]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [URowVector]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [URowVector; UInt; UInt]) ;
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [URowVector; UInt; UInt; UInt]) ;
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [bare_array_type (UReal, 1); UInt; UInt]) ;
  add_unqualified
    ( "to_matrix"
    , ReturnType UMatrix
    , [bare_array_type (UReal, 1); UInt; UInt; UInt] ) ;
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [bare_array_type (UInt, 1); UInt; UInt]) ;
  add_unqualified
    ( "to_matrix"
    , ReturnType UMatrix
    , [bare_array_type (UInt, 1); UInt; UInt; UInt] ) ;
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [bare_array_type (UReal, 2)]) ;
  add_unqualified ("to_matrix", ReturnType UMatrix, [bare_array_type (UInt, 2)]) ;
  add_unqualified ("to_row_vector", ReturnType URowVector, [UMatrix]) ;
  add_unqualified ("to_row_vector", ReturnType URowVector, [UVector]) ;
  add_unqualified ("to_row_vector", ReturnType URowVector, [URowVector]) ;
  add_unqualified
    ("to_row_vector", ReturnType URowVector, [bare_array_type (UReal, 1)]) ;
  add_unqualified
    ("to_row_vector", ReturnType URowVector, [bare_array_type (UInt, 1)]) ;
  add_unqualified ("to_vector", ReturnType UVector, [UMatrix]) ;
  add_unqualified ("to_vector", ReturnType UVector, [UVector]) ;
  add_unqualified ("to_vector", ReturnType UVector, [URowVector]) ;
  add_unqualified
    ("to_vector", ReturnType UVector, [bare_array_type (UReal, 1)]) ;
  add_unqualified ("to_vector", ReturnType UVector, [bare_array_type (UInt, 1)]) ;
  add_unqualified ("trace", ReturnType UReal, [UMatrix]) ;
  add_unqualified
    ("trace_gen_quad_form", ReturnType UReal, [UMatrix; UMatrix; UMatrix]) ;
  add_unqualified ("trace_quad_form", ReturnType UReal, [UMatrix; UVector]) ;
  add_unqualified ("trace_quad_form", ReturnType UReal, [UMatrix; UMatrix]) ;
  add_unqualified ("transpose", ReturnType URowVector, [UVector]) ;
  add_unqualified ("transpose", ReturnType UVector, [URowVector]) ;
  add_unqualified ("transpose", ReturnType UMatrix, [UMatrix]) ;
  add_unqualified ("uniform_simplex", ReturnType UVector, [UInt]) ;
  add_unqualified ("variance", ReturnType UReal, [bare_array_type (UReal, 1)]) ;
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
