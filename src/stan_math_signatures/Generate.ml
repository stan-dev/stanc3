(** This is an executable program that dumps out a list
  of all the functions defined in the Stan math library and their type.

  This used to just be done at runtime, but it was actually an appreciable percentage
  of the total time spent by the compiler just to generate all the combinations,
  so now it is done by the build system and just stored in the binary using
  the [Marshal] library.
*)

open Core
open Middle

(** The "dimensionality" (bad name?) is supposed to help us represent the
    vectorized nature of many Stan functions. It allows us to represent when
    a function argument can be just a real or matrix, or some common forms of
    vectorization over reals. This captures the most commonly used forms in our
    previous signatures; there are a lot partially because we had a lot of
    inconsistencies.
*)
type dimensionality =
  | DInt
  | DReal
  | DVector
  | DMatrix
  | DIntArray
  (* Vectorizable int *)
  | DVInt
  (* Vectorizable real *)
  | DVReal
  | DVComplex
  (* DEPRECATED; vectorizable ints or reals *)
  | DIntAndReals
  (* Vectorizable vectors - for multivariate functions *)
  | DVectors
  | DDeepVectorized
  | DComplexVectors
  | DDeepComplexVectorized
[@@warning "-37"]
(* don't warn that some constructors are not yet used *)

(* all base types with up 8 levels of nested containers -
                       just used for element-wise vectorized unary functions now *)

let rec bare_array_type (t, i) =
  match i with 0 -> t | j -> UnsizedType.UArray (bare_array_type (t, j - 1))

let bare_types =
  [ UnsizedType.UInt; UReal; UComplex; UVector; URowVector; UMatrix
  ; UComplexVector; UComplexRowVector; UComplexMatrix ]

let vector_types = [UnsizedType.UReal; UArray UReal; UVector; URowVector]
let primitive_types = [UnsizedType.UInt; UReal]

let complex_types =
  [UnsizedType.UComplex; UComplexVector; UComplexRowVector; UComplexMatrix]

let all_vector_types =
  [UnsizedType.UReal; UArray UReal; UVector; URowVector; UInt; UArray UInt]

let rec expand_arg = function
  | DInt -> [UnsizedType.UInt]
  | DReal -> [UReal]
  | DVector -> [UVector]
  | DMatrix -> [UMatrix]
  | DIntArray -> [UArray UInt]
  | DVInt -> [UInt; UArray UInt]
  | DVReal -> [UReal; UArray UReal; UVector; URowVector]
  | DVComplex -> [UComplex; UArray UComplex; UComplexVector; UComplexRowVector]
  | DIntAndReals -> expand_arg DVReal @ expand_arg DVInt
  | DVectors -> [UVector; UArray UVector; URowVector; UArray URowVector]
  | DComplexVectors ->
      [ UComplexVector; UArray UComplexVector; UComplexRowVector
      ; UArray UComplexRowVector ]
  | DDeepVectorized ->
      let all_base = [UnsizedType.UInt; UReal; URowVector; UVector; UMatrix] in
      List.(
        concat_map all_base ~f:(fun a ->
            map (range 0 8) ~f:(fun i -> bare_array_type (a, i))))
  | DDeepComplexVectorized ->
      List.(
        concat_map complex_types ~f:(fun a ->
            map (range 0 8) ~f:(fun i -> bare_array_type (a, i))))

type return_behavior = SameAsArg | IntsToReals | ComplexToReals
[@@deriving show {with_path= false}]

type fkind =
  | Lpmf
  | Lpdf
  | Rng
  | Cdf
  | Ccdf
  | UnaryVectorized of return_behavior
[@@deriving show {with_path= false}]

let is_primitive = function
  | UnsizedType.UReal -> true
  | UInt -> true
  | _ -> false

(** The signatures hash table *)
let (stan_math_signatures : (string, UnsizedType.signature list) Hashtbl.t) =
  String.Table.create ()

(** The variadic signatures hash table

    These functions cannot be overloaded.
*)
let (stan_math_variadic_signatures :
      (string, UnsizedType.variadic_signature) Hashtbl.t) =
  String.Table.create ()

(* XXX The correct word here isn't combination - what is it? *)
let all_combinations xx =
  List.fold_right xx ~init:[[]] ~f:(fun x accum ->
      List.concat_map accum ~f:(fun acc ->
          List.map ~f:(fun arg -> arg :: acc) x))

let missing_math_functions =
  String.Set.of_list ["beta_proportion_cdf"; "loglogistic_lcdf"]

let rng_return_type t lt =
  if List.for_all ~f:is_primitive lt then t else UnsizedType.UArray t

let add_unqualified (name, rt, uqargts, mem_pattern) =
  Hashtbl.add_multi stan_math_signatures ~key:name
    ~data:
      ( List.map ~f:(fun x -> (UnsizedType.AutoDiffable, x)) uqargts
      , rt
      , Fun_kind.suffix_from_name name
      , mem_pattern )

let rec ints_to_real unsized =
  match unsized with
  | UnsizedType.UInt -> UnsizedType.UReal
  | UArray t -> UArray (ints_to_real t)
  | x -> x

let rec complex_to_real = function
  | UnsizedType.UComplex -> UnsizedType.UReal
  | UComplexVector -> UVector
  | UComplexRowVector -> URowVector
  | UComplexMatrix -> UMatrix
  | UArray t -> UArray (complex_to_real t)
  | x -> x

let rec real_to_complex = function
  | UnsizedType.UReal -> UnsizedType.UComplex
  | UVector -> UComplexVector
  | URowVector -> UComplexRowVector
  | UMatrix -> UComplexMatrix
  | UArray t -> UArray (real_to_complex t)
  | x -> x

let mk_declarative_sig (fnkinds, name, args, mem_pattern) =
  let sfxes = function
    | Lpmf -> ["_lpmf"]
    | Lpdf -> ["_lpdf"]
    | Rng -> ["_rng"]
    | Cdf -> ["_cdf"; "_lcdf"]
    | Ccdf -> ["_lccdf"]
    | UnaryVectorized _ -> [""] in
  let add_ints = function DVReal -> DIntAndReals | x -> x in
  let all_expanded args = all_combinations (List.map ~f:expand_arg args) in
  let promoted_dim = function
    | DInt | DIntArray | DVInt -> UnsizedType.UInt
    (* XXX fix this up to work with more RNGs *)
    | _ -> UReal in
  let find_rt rt args = function
    | Rng -> UnsizedType.ReturnType (rng_return_type rt args)
    | UnaryVectorized SameAsArg -> ReturnType (List.hd_exn args)
    | UnaryVectorized IntsToReals ->
        ReturnType (ints_to_real (List.hd_exn args))
    | UnaryVectorized ComplexToReals ->
        ReturnType (complex_to_real (List.hd_exn args))
    | _ -> ReturnType UReal in
  let create_from_fk_args fk arglists =
    List.concat_map arglists ~f:(fun args ->
        List.map (sfxes fk) ~f:(fun sfx ->
            (name ^ sfx, find_rt UReal args fk, args, mem_pattern))) in
  let add_fnkind = function
    | Rng ->
        let rt, args = (List.hd_exn args, List.tl_exn args) in
        let args = List.map ~f:add_ints args in
        let rt = promoted_dim rt in
        let name = name ^ "_rng" in
        List.map (all_expanded args) ~f:(fun args ->
            (name, find_rt rt args Rng, args, mem_pattern))
    | fk -> create_from_fk_args fk (all_expanded args) in
  List.concat_map fnkinds ~f:add_fnkind
  |> List.filter ~f:(fun (n, _, _, _) -> not (Set.mem missing_math_functions n))
  |> List.map ~f:(fun (n, rt, args, support_soa) ->
         ( n
         , rt
         , List.map ~f:(fun x -> (UnsizedType.AutoDiffable, x)) args
         , support_soa ))

let full_lpdf = [Lpdf; Rng; Ccdf; Cdf]
let full_lpmf = [Lpmf; Rng; Ccdf; Cdf]

let distributions =
  [ (full_lpmf, "beta_binomial", [DVInt; DVInt; DVReal; DVReal], Mem_pattern.SoA)
  ; ( [Lpmf; Ccdf; Cdf; Rng]
    , "beta_neg_binomial"
    , [DVInt; DVReal; DVReal; DVReal]
    , SoA ); (full_lpdf, "beta", [DVReal; DVReal; DVReal], SoA)
  ; ([Lpdf; Ccdf; Cdf], "beta_proportion", [DVReal; DVReal; DIntAndReals], SoA)
  ; (full_lpmf, "bernoulli", [DVInt; DVReal], SoA)
  ; ([Lpmf; Rng], "bernoulli_logit", [DVInt; DVReal], SoA)
  ; ([Lpmf], "bernoulli_logit_glm", [DVInt; DMatrix; DReal; DVector], SoA)
  ; (full_lpmf, "binomial", [DVInt; DVInt; DVReal], SoA)
  ; ([Lpmf], "binomial_logit", [DVInt; DVInt; DVReal], SoA)
  ; ([Lpmf], "binomial_logit_glm", [DVInt; DVInt; DMatrix; DReal; DVector], SoA)
  ; ([Lpmf], "categorical", [DVInt; DVector], AoS)
  ; ([Lpmf], "categorical_logit", [DVInt; DVector], AoS)
  ; ([Lpmf], "categorical_logit_glm", [DVInt; DMatrix; DVector; DMatrix], SoA)
  ; (full_lpdf, "cauchy", [DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "chi_square", [DVReal; DVReal], SoA)
  ; ([Lpdf], "dirichlet", [DVectors; DVectors], SoA)
  ; ([Lpmf], "dirichlet_multinomial", [DIntArray; DVector], AoS)
  ; (full_lpmf, "discrete_range", [DVInt; DVInt; DVInt], SoA)
  ; (full_lpdf, "double_exponential", [DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "exp_mod_normal", [DVReal; DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "exponential", [DVReal; DVReal], SoA)
  ; (full_lpdf, "frechet", [DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "gamma", [DVReal; DVReal; DVReal], SoA)
  ; ( [Lpdf]
    , "gaussian_dlm_obs"
    , [DMatrix; DMatrix; DMatrix; DMatrix; DMatrix; DVector; DMatrix]
    , AoS ); (full_lpdf, "gumbel", [DVReal; DVReal; DVReal], SoA)
  ; ([Rng], "hmm_latent", [DIntArray; DMatrix; DMatrix; DVector], AoS)
  ; ([Lpmf; Rng], "hypergeometric", [DInt; DInt; DInt; DInt], SoA)
  ; (full_lpdf, "inv_chi_square", [DVReal; DVReal], SoA)
  ; (full_lpdf, "inv_gamma", [DVReal; DVReal; DVReal], SoA)
  ; ([Lpdf], "inv_wishart_cholesky", [DMatrix; DReal; DMatrix], SoA)
  ; ([Lpdf], "inv_wishart", [DMatrix; DReal; DMatrix], SoA)
  ; ([Lpdf], "lkj_corr", [DMatrix; DReal], AoS)
  ; ([Lpdf], "lkj_corr_cholesky", [DMatrix; DReal], AoS)
  ; ([Lpdf], "lkj_cov", [DMatrix; DVector; DVector; DReal], AoS)
  ; (full_lpdf, "logistic", [DVReal; DVReal; DVReal], SoA)
  ; ([Lpdf; Rng; Cdf], "loglogistic", [DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "lognormal", [DVReal; DVReal; DVReal], SoA)
  ; ([Lpdf], "multi_gp", [DMatrix; DMatrix; DVector], AoS)
  ; ([Lpdf], "multi_gp_cholesky", [DMatrix; DMatrix; DVector], AoS)
  ; ([Lpmf], "multinomial", [DIntArray; DVector], AoS)
  ; ([Lpmf], "multinomial_logit", [DIntArray; DVector], AoS)
  ; ([Lpdf], "multi_normal", [DVectors; DVectors; DMatrix], AoS)
  ; ([Lpdf], "multi_normal_cholesky", [DVectors; DVectors; DMatrix], AoS)
  ; ([Lpdf], "multi_normal_prec", [DVectors; DVectors; DMatrix], AoS)
  ; ([Lpdf], "multi_student_t", [DVectors; DReal; DVectors; DMatrix], AoS)
  ; ( [Lpdf]
    , "multi_student_t_cholesky"
    , [DVectors; DReal; DVectors; DMatrix]
    , SoA ); (full_lpmf, "neg_binomial", [DVInt; DVReal; DVReal], SoA)
  ; (full_lpmf, "neg_binomial_2", [DVInt; DVReal; DVReal], SoA)
  ; ([Lpmf; Rng], "neg_binomial_2_log", [DVInt; DVReal; DVReal], SoA)
  ; ( [Lpmf]
    , "neg_binomial_2_log_glm"
    , [DVInt; DMatrix; DReal; DVector; DReal]
    , SoA ); (full_lpdf, "normal", [DVReal; DVReal; DVReal], SoA)
  ; ([Lpdf], "normal_id_glm", [DVector; DMatrix; DReal; DVector; DReal], SoA)
  ; ([Lpmf], "ordered_logistic", [DInt; DReal; DVector], SoA)
  ; ([Lpmf], "ordered_logistic_glm", [DVInt; DMatrix; DVector; DVector], SoA)
  ; ([Lpmf], "ordered_probit", [DInt; DReal; DVector], SoA)
  ; (full_lpdf, "pareto", [DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "pareto_type_2", [DVReal; DVReal; DVReal; DVReal], SoA)
  ; (full_lpmf, "poisson", [DVInt; DVReal], SoA)
  ; ([Lpmf; Rng], "poisson_log", [DVInt; DVReal], SoA)
  ; ([Lpmf], "poisson_log_glm", [DVInt; DMatrix; DReal; DVector], SoA)
  ; (full_lpdf, "rayleigh", [DVReal; DVReal], SoA)
  ; (full_lpdf, "scaled_inv_chi_square", [DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "skew_normal", [DVReal; DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "skew_double_exponential", [DVReal; DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "student_t", [DVReal; DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "std_normal", [DVReal], SoA)
  ; (full_lpdf, "uniform", [DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "von_mises", [DVReal; DVReal; DVReal], SoA)
  ; (full_lpdf, "weibull", [DVReal; DVReal; DVReal], SoA)
  ; ([Lpdf], "wiener", [DVReal; DVReal; DVReal; DVReal; DVReal], SoA)
    (* new wiener_lpdfs -- c++ is fully vectorized, but this style of implementation
       in the typechecker is too expensive to enumerate, so we provide only the
       full scalar and full vector case *)
  ; ([Lpdf], "wiener", [DReal; DReal; DReal; DReal; DReal; DReal], AoS)
  ; ( [Lpdf]
    , "wiener"
    , [DReal; DReal; DReal; DReal; DReal; DReal; DReal; DReal]
    , AoS )
  ; ( [Lpdf]
    , "wiener"
    , [DVector; DVector; DVector; DVector; DVector; DVector]
    , AoS )
  ; ( [Lpdf]
    , "wiener"
    , [DVector; DVector; DVector; DVector; DVector; DVector; DVector; DVector]
    , AoS ); ([Lpdf], "wishart_cholesky", [DMatrix; DReal; DMatrix], SoA)
  ; ([Lpdf], "wishart", [DMatrix; DReal; DMatrix], SoA) ]

let basic_vectorized = UnaryVectorized IntsToReals

let math_sigs =
  [ ([UnaryVectorized SameAsArg], "abs", [DDeepVectorized], Mem_pattern.SoA)
  ; ([basic_vectorized], "acos", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "acosh", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "asin", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "asinh", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "atan", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "atanh", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "cbrt", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "ceil", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "cos", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "cosh", [DDeepVectorized], SoA)
  ; ([UnaryVectorized SameAsArg], "conj", [DDeepComplexVectorized], AoS)
  ; ([basic_vectorized], "digamma", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "erf", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "erfc", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "exp", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "exp2", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "expm1", [DDeepVectorized], SoA)
  ; ([UnaryVectorized ComplexToReals], "get_imag", [DDeepComplexVectorized], AoS)
  ; ([UnaryVectorized ComplexToReals], "get_real", [DDeepComplexVectorized], AoS)
  ; ([UnaryVectorized ComplexToReals], "abs", [DDeepComplexVectorized], AoS)
  ; ([basic_vectorized], "floor", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "inv", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "inv_cloglog", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "inv_erfc", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "inv_logit", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "inv_Phi", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "inv_sqrt", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "inv_square", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "lambert_w0", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "lambert_wm1", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "lgamma", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "log", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "log10", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "log1m", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "log1m_exp", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "log1m_inv_logit", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "log1p", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "log1p_exp", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "log2", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "log_inv_logit", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "logit", [DDeepVectorized], SoA)
  ; ([UnaryVectorized SameAsArg], "minus", [DDeepVectorized], SoA)
  ; ([UnaryVectorized SameAsArg], "minus", [DDeepComplexVectorized], SoA)
  ; ([basic_vectorized], "Phi", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "Phi_approx", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "round", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "sin", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "sinh", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "sqrt", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "square", [DDeepVectorized], SoA)
    (* TODO: Eventually will want to move _qf to be part of the distribution list above *)
  ; ([basic_vectorized], "std_normal_qf", [DDeepVectorized], SoA)
    (* std_normal_qf is an alias for inv_Phi *)
  ; ([basic_vectorized], "std_normal_log_qf", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "step", [DReal], SoA)
  ; ([basic_vectorized], "tan", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "tanh", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "tgamma", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "trunc", [DDeepVectorized], SoA)
  ; ([basic_vectorized], "trigamma", [DDeepVectorized], SoA) ]

let all_declarative_sigs = distributions @ math_sigs

let declarative_fnsigs =
  List.concat_map ~f:mk_declarative_sig all_declarative_sigs

(* -- Some helper definitions to populate stan_math_signatures -- *)

let add_qualified (name, rt, argts, supports_soa) =
  Hashtbl.add_multi stan_math_signatures ~key:name
    ~data:(argts, rt, Fun_kind.suffix_from_name name, supports_soa)

let add_nullary name =
  add_unqualified (name, UnsizedType.ReturnType UReal, [], AoS)

let add_binary_vec_general ~return_fn ~vectors ~scalars name supports_soa =
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified (name, ReturnType (return_fn i), [i; j], supports_soa))
        scalars)
    scalars;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (return_fn (bare_array_type (j, i)))
            , [bare_array_type (j, i); bare_array_type (j, i)]
            , supports_soa ))
        vectors)
    (List.range 0 8);
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          List.iter
            ~f:(fun k ->
              add_unqualified
                ( name
                , ReturnType (return_fn (bare_array_type (k, j)))
                , [bare_array_type (k, j); i]
                , supports_soa ))
            vectors)
        (List.range 0 8))
    scalars;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          List.iter
            ~f:(fun k ->
              add_unqualified
                ( name
                , ReturnType (return_fn (bare_array_type (k, j)))
                , [i; bare_array_type (k, j)]
                , supports_soa ))
            vectors)
        (List.range 0 8))
    scalars

let add_binary_vec name supports_soa =
  add_binary_vec_general ~return_fn:ints_to_real
    ~vectors:[UArray UInt; UArray UReal; UVector; URowVector; UMatrix]
    ~scalars:[UInt; UReal] name supports_soa

let add_binary_vec_real_real name supports_soa =
  add_binary_vec_general ~return_fn:Fun.id
    ~vectors:[UArray UReal; UVector; URowVector; UMatrix]
    ~scalars:[UReal] name supports_soa

let add_binary_vec_complex_complex name supports_soa =
  add_binary_vec_general ~return_fn:Fun.id
    ~vectors:[UArray UComplex; UComplexVector; UComplexRowVector; UComplexMatrix]
    ~scalars:[UComplex] name supports_soa

let add_binary_vec_reals_to_complex name supports_soa =
  add_binary_vec_general ~return_fn:real_to_complex
    ~vectors:[UArray UReal; UVector; URowVector; UMatrix]
    ~scalars:[UReal] name supports_soa

(* the following mix types in a way that doesn't
   work with the general method used above *)
let add_binary_vec_int_real name supports_soa =
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (bare_array_type (i, j))
            , [UInt; bare_array_type (i, j)]
            , supports_soa ))
        (List.range 0 8))
    [UnsizedType.UArray UReal; UVector; URowVector; UMatrix];
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (bare_array_type (i, j))
            , [bare_array_type (UInt, j + 1); bare_array_type (i, j)]
            , supports_soa ))
        (List.range 0 8))
    [UnsizedType.UArray UReal; UVector; URowVector];
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UMatrix, i))
        , [bare_array_type (UInt, i + 2); bare_array_type (UMatrix, i)]
        , supports_soa ))
    (List.range 0 8);
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UReal, i))
        , [bare_array_type (UInt, i); UReal]
        , supports_soa ))
    (List.range 0 8)

let add_binary_vec_real_int name supports_soa =
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (bare_array_type (i, j))
            , [bare_array_type (i, j); UInt]
            , supports_soa ))
        (List.range 0 8))
    [UnsizedType.UArray UReal; UVector; URowVector; UMatrix];
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( name
            , ReturnType (bare_array_type (i, j))
            , [bare_array_type (i, j); bare_array_type (UInt, j + 1)]
            , supports_soa ))
        (List.range 0 8))
    [UnsizedType.UArray UReal; UVector; URowVector];
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UMatrix, i))
        , [bare_array_type (UMatrix, i); bare_array_type (UInt, i + 2)]
        , supports_soa ))
    (List.range 0 8);
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UReal, i))
        , [UReal; bare_array_type (UInt, i)]
        , supports_soa ))
    (List.range 0 8)

let add_binary_vec_int_int name supports_soa =
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UInt, i))
        , [bare_array_type (UInt, i); UInt]
        , supports_soa ))
    (List.range 0 8);
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UInt, i))
        , [UInt; bare_array_type (UInt, i)]
        , supports_soa ))
    (List.range 1 8);
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( name
        , ReturnType (bare_array_type (UInt, i))
        , [bare_array_type (UInt, i); bare_array_type (UInt, i)]
        , supports_soa ))
    (List.range 1 8)

(* things like lower_bound require the second argument be a scalar or the same
    - this is basically the general case above with the last nested loop removed
*)
let add_first_arg_vector_binary name supports_soa =
  let vectors = [UnsizedType.UArray UReal; UVector; URowVector; UMatrix] in
  add_unqualified (name, ReturnType UReal, [UReal; UReal], supports_soa);
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          let ty = bare_array_type (j, i) in
          add_unqualified (name, ReturnType ty, [ty; ty], supports_soa);
          add_unqualified (name, ReturnType ty, [ty; UReal], supports_soa))
        vectors)
    (List.range 0 8)

let add_first_arg_vector_ternary name supports_soa =
  let vectors = [UnsizedType.UArray UReal; UVector; URowVector; UMatrix] in
  add_unqualified (name, ReturnType UReal, [UReal; UReal; UReal], supports_soa);
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun j ->
          let ty = bare_array_type (j, i) in
          add_unqualified (name, ReturnType ty, [ty; ty; ty], supports_soa);
          add_unqualified (name, ReturnType ty, [ty; ty; UReal], supports_soa);
          add_unqualified (name, ReturnType ty, [ty; UReal; ty], supports_soa))
        vectors)
    (List.range 0 8)

(** For functions that accept and return (arrays of...) vectors*)
let add_nested_unary base name supports_soa =
  List.iter
    ~f:(fun i ->
      let t = bare_array_type (base, i) in
      add_unqualified (name, ReturnType t, [t], supports_soa))
    (List.range 0 8)

let add_ternary name supports_soa =
  add_unqualified (name, ReturnType UReal, [UReal; UReal; UReal], supports_soa)

(*Adds functions that operate on matrix, double array and real types*)
let add_ternary_vec name supports_soa =
  add_unqualified (name, ReturnType UReal, [UReal; UReal; UReal], supports_soa);
  add_unqualified
    (name, ReturnType UVector, [UVector; UReal; UReal], supports_soa);
  add_unqualified
    (name, ReturnType UVector, [UVector; UVector; UReal], supports_soa);
  add_unqualified
    (name, ReturnType UVector, [UVector; UReal; UVector], supports_soa);
  add_unqualified
    (name, ReturnType UVector, [UVector; UVector; UVector], supports_soa);
  add_unqualified
    (name, ReturnType UVector, [UReal; UVector; UReal], supports_soa);
  add_unqualified
    (name, ReturnType UVector, [UReal; UVector; UVector], supports_soa);
  add_unqualified
    (name, ReturnType UVector, [UReal; UReal; UVector], supports_soa);
  add_unqualified
    (name, ReturnType URowVector, [URowVector; UReal; UReal], supports_soa);
  add_unqualified
    (name, ReturnType URowVector, [URowVector; URowVector; UReal], supports_soa);
  add_unqualified
    (name, ReturnType URowVector, [URowVector; UReal; URowVector], supports_soa);
  add_unqualified
    ( name
    , ReturnType URowVector
    , [URowVector; URowVector; URowVector]
    , supports_soa );
  add_unqualified
    (name, ReturnType URowVector, [UReal; URowVector; UReal], supports_soa);
  add_unqualified
    (name, ReturnType URowVector, [UReal; URowVector; URowVector], supports_soa);
  add_unqualified
    (name, ReturnType URowVector, [UReal; UReal; URowVector], supports_soa);
  add_unqualified
    (name, ReturnType UMatrix, [UMatrix; UReal; UReal], supports_soa);
  add_unqualified
    (name, ReturnType UMatrix, [UMatrix; UMatrix; UReal], supports_soa);
  add_unqualified
    (name, ReturnType UMatrix, [UMatrix; UReal; UMatrix], supports_soa);
  add_unqualified
    (name, ReturnType UMatrix, [UMatrix; UMatrix; UMatrix], supports_soa);
  add_unqualified
    (name, ReturnType UMatrix, [UReal; UMatrix; UReal], supports_soa);
  add_unqualified
    (name, ReturnType UMatrix, [UReal; UMatrix; UMatrix], supports_soa);
  add_unqualified
    (name, ReturnType UMatrix, [UReal; UReal; UMatrix], supports_soa)

let for_all_vector_types s = List.iter ~f:s all_vector_types
let for_vector_types s = List.iter ~f:s vector_types

(* -- Start populating stan_math_signaturess -- *)
let () =
  List.iter declarative_fnsigs ~f:(fun (key, rt, args, mem_pattern) ->
      Hashtbl.add_multi stan_math_signatures ~key
        ~data:(args, rt, Fun_kind.suffix_from_name key, mem_pattern));
  add_unqualified ("acos", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("acosh", ReturnType UComplex, [UComplex], AoS);
  List.iter
    ~f:(fun x -> add_unqualified ("add", ReturnType x, [x; x], SoA))
    bare_types;
  add_unqualified ("add", ReturnType UVector, [UVector; UReal], SoA);
  add_unqualified ("add", ReturnType URowVector, [URowVector; UReal], SoA);
  add_unqualified ("add", ReturnType UMatrix, [UMatrix; UReal], SoA);
  add_unqualified ("add", ReturnType UVector, [UReal; UVector], SoA);
  add_unqualified ("add", ReturnType URowVector, [UReal; URowVector], SoA);
  add_unqualified ("add", ReturnType UMatrix, [UReal; UMatrix], SoA);
  add_unqualified
    ("add", ReturnType UComplexVector, [UComplexVector; UComplex], SoA);
  add_unqualified
    ("add", ReturnType UComplexRowVector, [UComplexRowVector; UComplex], SoA);
  add_unqualified
    ("add", ReturnType UComplexMatrix, [UComplexMatrix; UComplex], SoA);
  add_unqualified
    ("add", ReturnType UComplexVector, [UComplex; UComplexVector], SoA);
  add_unqualified
    ("add", ReturnType UComplexRowVector, [UComplex; UComplexRowVector], SoA);
  add_unqualified
    ("add", ReturnType UComplexMatrix, [UComplex; UComplexMatrix], SoA);
  add_unqualified ("add_diag", ReturnType UMatrix, [UMatrix; UReal], AoS);
  add_unqualified ("add_diag", ReturnType UMatrix, [UMatrix; UVector], AoS);
  add_unqualified ("add_diag", ReturnType UMatrix, [UMatrix; URowVector], AoS);
  add_unqualified
    ("add_diag", ReturnType UComplexMatrix, [UComplexMatrix; UComplex], AoS);
  add_unqualified
    ( "add_diag"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexVector]
    , AoS );
  add_unqualified
    ( "add_diag"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexRowVector]
    , AoS );
  add_qualified
    ( "algebra_solver"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain
            , AoS ) ); (AutoDiffable, UVector); (AutoDiffable, UVector)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
    , AoS );
  add_qualified
    ( "algebra_solver"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UVector)
      ; (AutoDiffable, UVector); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt); (DataOnly, UReal); (DataOnly, UReal)
      ; (DataOnly, UReal) ]
    , AoS );
  add_qualified
    ( "algebra_solver_newton"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UVector)
      ; (AutoDiffable, UVector); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt) ]
    , AoS );
  add_qualified
    ( "algebra_solver_newton"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UVector)
      ; (AutoDiffable, UVector); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt); (DataOnly, UReal); (DataOnly, UReal)
      ; (DataOnly, UReal) ]
    , AoS );
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified
            ( "append_array"
            , ReturnType (bare_array_type (t, i))
            , [bare_array_type (t, i); bare_array_type (t, i)]
            , AoS ))
        bare_types)
    (List.range 1 8);
  add_unqualified ("arg", ReturnType UReal, [UComplex], AoS);
  add_unqualified ("asin", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("asinh", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("atan", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("atanh", ReturnType UComplex, [UComplex], AoS);
  add_binary_vec "atan2" AoS;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UVector; UVector]
    , SoA );
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UVector]
    , SoA );
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UReal; UVector]
    , SoA );
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UVector]
    , SoA );
  add_unqualified
    ( "bernoulli_logit_glm_rng"
    , ReturnType (UArray UInt)
    , [UMatrix; UVector; UVector]
    , AoS );
  add_unqualified
    ( "bernoulli_logit_glm_rng"
    , ReturnType (UArray UInt)
    , [URowVector; UVector; UVector]
    , AoS );
  add_unqualified
    ( "binomial_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UArray UInt; UMatrix; UVector; UVector]
    , SoA );
  add_unqualified
    ( "binomial_logit_glm_lpmf"
    , ReturnType UReal
    , [UInt; UInt; UMatrix; UVector; UVector]
    , SoA );
  add_unqualified
    ( "binomial_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UArray UInt; URowVector; UReal; UVector]
    , SoA );
  add_unqualified
    ( "binomial_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UArray UInt; URowVector; UVector; UVector]
    , SoA );
  add_binary_vec_int_real "bessel_first_kind" SoA;
  add_binary_vec_int_real "bessel_second_kind" SoA;
  add_binary_vec "beta" SoA;
  (* XXX For some reason beta_proportion_rng doesn't take ints as first arg *)
  for_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "beta_proportion_rng"
            , ReturnType (rng_return_type UReal [t; u])
            , [t; u]
            , AoS )));
  add_binary_vec_int_real "binary_log_loss" AoS;
  add_unqualified
    ("block", ReturnType UMatrix, [UMatrix; UInt; UInt; UInt; UInt], SoA);
  add_unqualified
    ( "block"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UInt; UInt; UInt; UInt]
    , AoS );
  add_unqualified ("categorical_rng", ReturnType UInt, [UVector], AoS);
  add_unqualified ("categorical_logit_rng", ReturnType UInt, [UVector], AoS);
  add_unqualified
    ( "categorical_logit_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UMatrix]
    , SoA );
  add_unqualified
    ( "categorical_logit_glm_lpmf"
    , ReturnType UReal
    , [UInt; URowVector; UVector; UMatrix]
    , SoA );
  add_unqualified ("append_col", ReturnType UMatrix, [UMatrix; UMatrix], AoS);
  add_unqualified ("append_col", ReturnType UMatrix, [UVector; UMatrix], AoS);
  add_unqualified ("append_col", ReturnType UMatrix, [UMatrix; UVector], AoS);
  add_unqualified ("append_col", ReturnType UMatrix, [UVector; UVector], AoS);
  add_unqualified
    ("append_col", ReturnType URowVector, [URowVector; URowVector], AoS);
  add_unqualified ("append_col", ReturnType URowVector, [UReal; URowVector], AoS);
  add_unqualified ("append_col", ReturnType URowVector, [URowVector; UReal], AoS);
  add_unqualified
    ( "append_col"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexMatrix]
    , AoS );
  add_unqualified
    ( "append_col"
    , ReturnType UComplexMatrix
    , [UComplexVector; UComplexMatrix]
    , AoS );
  add_unqualified
    ( "append_col"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexVector]
    , AoS );
  add_unqualified
    ( "append_col"
    , ReturnType UComplexMatrix
    , [UComplexVector; UComplexVector]
    , AoS );
  add_unqualified
    ( "append_col"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UComplexRowVector]
    , AoS );
  add_unqualified
    ( "append_col"
    , ReturnType UComplexRowVector
    , [UComplex; UComplexRowVector]
    , AoS );
  add_unqualified
    ( "append_col"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UComplex]
    , AoS );
  List.iter
    ~f:(fun i ->
      let m = bare_array_type (UMatrix, i) in
      let v = bare_array_type (UVector, i) in
      add_unqualified ("corr_matrix_constrain", ReturnType m, [v; UInt], SoA);
      add_unqualified ("corr_matrix_unconstrain", ReturnType v, [m], AoS);
      add_unqualified ("corr_matrix_jacobian", ReturnType m, [v; UInt], SoA);
      add_unqualified ("cov_matrix_constrain", ReturnType m, [v; UInt], SoA);
      add_unqualified ("cov_matrix_unconstrain", ReturnType v, [m], AoS);
      add_unqualified ("cov_matrix_jacobian", ReturnType m, [v; UInt], SoA))
    (List.range 0 8);
  add_unqualified ("chol2inv", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified ("cholesky_decompose", ReturnType UMatrix, [UMatrix], SoA);
  List.iter
    ~f:(fun i ->
      let m = bare_array_type (UMatrix, i) in
      let v = bare_array_type (UVector, i) in
      add_unqualified
        ("cholesky_factor_corr_constrain", ReturnType m, [v; UInt], SoA);
      add_unqualified
        ("cholesky_factor_corr_unconstrain", ReturnType v, [m], AoS);
      add_unqualified
        ("cholesky_factor_corr_jacobian", ReturnType m, [v; UInt], SoA);
      add_unqualified
        ("cholesky_factor_cov_constrain", ReturnType m, [v; UInt; UInt], SoA);
      add_unqualified ("cholesky_factor_cov_unconstrain", ReturnType v, [m], AoS);
      add_unqualified
        ("cholesky_factor_cov_jacobian", ReturnType m, [v; UInt; UInt], SoA))
    (List.range 0 8);
  add_binary_vec_int_int "choose" AoS;
  add_unqualified ("col", ReturnType UVector, [UMatrix; UInt], AoS);
  add_unqualified ("col", ReturnType UComplexVector, [UComplexMatrix; UInt], SoA);
  add_unqualified ("cols", ReturnType UInt, [UVector], SoA);
  add_unqualified ("cols", ReturnType UInt, [URowVector], SoA);
  add_unqualified ("cols", ReturnType UInt, [UMatrix], SoA);
  add_unqualified ("cols", ReturnType UInt, [UComplexVector], SoA);
  add_unqualified ("cols", ReturnType UInt, [UComplexRowVector], SoA);
  add_unqualified ("cols", ReturnType UInt, [UComplexMatrix], SoA);
  add_unqualified
    ("columns_dot_product", ReturnType URowVector, [UVector; UVector], AoS);
  add_unqualified
    ("columns_dot_product", ReturnType URowVector, [URowVector; URowVector], AoS);
  add_unqualified
    ("columns_dot_product", ReturnType URowVector, [UMatrix; UMatrix], SoA);
  add_unqualified
    ( "columns_dot_product"
    , ReturnType UComplexRowVector
    , [UComplexVector; UComplexVector]
    , AoS );
  add_unqualified
    ( "columns_dot_product"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UComplexRowVector]
    , AoS );
  add_unqualified
    ( "columns_dot_product"
    , ReturnType UComplexRowVector
    , [UComplexMatrix; UComplexMatrix]
    , AoS );
  add_unqualified ("columns_dot_self", ReturnType URowVector, [UVector], AoS);
  add_unqualified ("columns_dot_self", ReturnType URowVector, [URowVector], AoS);
  add_unqualified ("columns_dot_self", ReturnType URowVector, [UMatrix], AoS);
  add_unqualified
    ("columns_dot_self", ReturnType UComplexRowVector, [UComplexVector], AoS);
  add_unqualified
    ("columns_dot_self", ReturnType UComplexRowVector, [UComplexRowVector], AoS);
  add_unqualified
    ("columns_dot_self", ReturnType UComplexRowVector, [UComplexMatrix], AoS);
  add_unqualified
    ( "complex_schur_decompose"
    , ReturnType (UTuple [UComplexMatrix; UComplexMatrix])
    , [UComplexMatrix]
    , AoS );
  add_unqualified
    ( "complex_schur_decompose"
    , ReturnType (UTuple [UComplexMatrix; UComplexMatrix])
    , [UMatrix]
    , AoS );
  add_unqualified
    ( "complex_schur_decompose_t"
    , ReturnType UComplexMatrix
    , [UComplexMatrix]
    , AoS );
  add_unqualified
    ("complex_schur_decompose_t", ReturnType UComplexMatrix, [UMatrix], AoS);
  add_unqualified
    ( "complex_schur_decompose_u"
    , ReturnType UComplexMatrix
    , [UComplexMatrix]
    , AoS );
  add_unqualified
    ("complex_schur_decompose_u", ReturnType UComplexMatrix, [UMatrix], AoS);
  add_unqualified ("cos", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("cosh", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("crossprod", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified
    ( "csr_matrix_times_vector"
    , ReturnType UVector
    , [UInt; UInt; UVector; UArray UInt; UArray UInt; UVector]
    , SoA );
  add_unqualified
    ( "csr_to_dense_matrix"
    , ReturnType UMatrix
    , [UInt; UInt; UVector; UArray UInt; UArray UInt]
    , AoS );
  add_unqualified
    ( "csr_extract"
    , ReturnType (UTuple [UVector; UArray UInt; UArray UInt])
    , [UMatrix]
    , AoS );
  add_unqualified ("csr_extract_w", ReturnType UVector, [UMatrix], AoS);
  add_unqualified ("csr_extract_v", ReturnType (UArray UInt), [UMatrix], AoS);
  add_unqualified ("csr_extract_u", ReturnType (UArray UInt), [UMatrix], AoS);
  add_unqualified
    ("cumulative_sum", ReturnType (UArray UInt), [UArray UInt], AoS);
  add_unqualified
    ("cumulative_sum", ReturnType (UArray UReal), [UArray UReal], AoS);
  add_unqualified ("cumulative_sum", ReturnType UVector, [UVector], SoA);
  add_unqualified ("cumulative_sum", ReturnType URowVector, [URowVector], SoA);
  add_unqualified
    ("cumulative_sum", ReturnType (UArray UComplex), [UArray UComplex], AoS);
  add_unqualified
    ("cumulative_sum", ReturnType UComplexVector, [UComplexVector], AoS);
  add_unqualified
    ("cumulative_sum", ReturnType UComplexRowVector, [UComplexRowVector], AoS);
  add_unqualified ("determinant", ReturnType UReal, [UMatrix], SoA);
  add_unqualified ("diag_matrix", ReturnType UMatrix, [UVector], AoS);
  add_unqualified
    ("diag_matrix", ReturnType UComplexMatrix, [UComplexVector], AoS);
  add_unqualified
    ("diag_post_multiply", ReturnType UMatrix, [UMatrix; UVector], SoA);
  add_unqualified
    ("diag_post_multiply", ReturnType UMatrix, [UMatrix; URowVector], SoA);
  add_unqualified
    ( "diag_post_multiply"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexVector]
    , AoS );
  add_unqualified
    ( "diag_post_multiply"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexRowVector]
    , AoS );
  add_unqualified
    ("diag_pre_multiply", ReturnType UMatrix, [UVector; UMatrix], SoA);
  add_unqualified
    ("diag_pre_multiply", ReturnType UMatrix, [URowVector; UMatrix], SoA);
  add_unqualified
    ( "diag_pre_multiply"
    , ReturnType UComplexMatrix
    , [UComplexVector; UComplexMatrix]
    , AoS );
  add_unqualified
    ( "diag_pre_multiply"
    , ReturnType UComplexMatrix
    , [UComplexRowVector; UComplexMatrix]
    , AoS );
  add_unqualified ("diagonal", ReturnType UVector, [UMatrix], SoA);
  add_unqualified ("diagonal", ReturnType UComplexVector, [UComplexMatrix], SoA);
  add_unqualified ("dims", ReturnType (UArray UInt), [UComplex], AoS);
  add_unqualified ("dims", ReturnType (UArray UInt), [UInt], SoA);
  add_unqualified ("dims", ReturnType (UArray UInt), [UReal], SoA);
  add_unqualified ("dims", ReturnType (UArray UInt), [UVector], SoA);
  add_unqualified ("dims", ReturnType (UArray UInt), [URowVector], SoA);
  add_unqualified ("dims", ReturnType (UArray UInt), [UMatrix], SoA);
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified
            ("dims", ReturnType (UArray UInt), [bare_array_type (t, i + 1)], SoA))
        bare_types)
    (List.range 0 8);
  add_unqualified ("dirichlet_rng", ReturnType UVector, [UVector], AoS);
  add_unqualified
    ("dirichlet_multinomial_rng", ReturnType (UArray UInt), [UVector; UInt], AoS);
  add_unqualified ("distance", ReturnType UReal, [UVector; UVector], SoA);
  add_unqualified ("distance", ReturnType UReal, [URowVector; URowVector], SoA);
  add_unqualified ("distance", ReturnType UReal, [UVector; URowVector], SoA);
  add_unqualified ("distance", ReturnType UReal, [URowVector; UVector], SoA);
  add_unqualified ("divide", ReturnType UInt, [UInt; UInt], SoA);
  add_unqualified ("divide", ReturnType UReal, [UReal; UReal], SoA);
  add_unqualified ("divide", ReturnType UVector, [UVector; UReal], SoA);
  add_unqualified ("divide", ReturnType URowVector, [URowVector; UReal], SoA);
  add_unqualified ("divide", ReturnType UMatrix, [UMatrix; UReal], SoA);
  add_unqualified ("divide", ReturnType UVector, [UReal; UVector], SoA);
  add_unqualified ("divide", ReturnType URowVector, [UReal; URowVector], SoA);
  add_unqualified ("divide", ReturnType UMatrix, [UReal; UMatrix], SoA);
  add_unqualified ("divide", ReturnType UComplex, [UComplex; UComplex], AoS);
  add_unqualified
    ("divide", ReturnType UComplexVector, [UComplexVector; UComplex], AoS);
  add_unqualified
    ("divide", ReturnType UComplexRowVector, [UComplexRowVector; UComplex], AoS);
  add_unqualified
    ("divide", ReturnType UComplexMatrix, [UComplexMatrix; UComplex], AoS);
  add_unqualified
    ("divide", ReturnType UComplexVector, [UComplex; UComplexVector], AoS);
  add_unqualified
    ("divide", ReturnType UComplexRowVector, [UComplex; UComplexRowVector], AoS);
  add_unqualified
    ("divide", ReturnType UComplexMatrix, [UComplex; UComplexMatrix], AoS);
  add_unqualified ("dot_product", ReturnType UReal, [UVector; UVector], SoA);
  add_unqualified
    ("dot_product", ReturnType UReal, [URowVector; URowVector], SoA);
  add_unqualified ("dot_product", ReturnType UReal, [UVector; URowVector], SoA);
  add_unqualified ("dot_product", ReturnType UReal, [URowVector; UVector], SoA);
  add_unqualified
    ("dot_product", ReturnType UReal, [UArray UReal; UArray UReal], SoA);
  add_unqualified
    ("dot_product", ReturnType UComplex, [UComplexVector; UComplexVector], AoS);
  add_unqualified
    ( "dot_product"
    , ReturnType UComplex
    , [UComplexRowVector; UComplexRowVector]
    , AoS );
  add_unqualified
    ( "dot_product"
    , ReturnType UComplex
    , [UComplexVector; UComplexRowVector]
    , AoS );
  add_unqualified
    ( "dot_product"
    , ReturnType UComplex
    , [UComplexRowVector; UComplexVector]
    , AoS );
  add_unqualified
    ("dot_product", ReturnType UComplex, [UArray UComplex; UArray UComplex], AoS);
  add_unqualified ("dot_self", ReturnType UReal, [UVector], SoA);
  add_unqualified ("dot_self", ReturnType UReal, [URowVector], SoA);
  add_unqualified ("dot_self", ReturnType UComplex, [UComplexVector], AoS);
  add_unqualified ("dot_self", ReturnType UComplex, [UComplexRowVector], AoS);
  add_nullary "e";
  add_unqualified
    ( "eigendecompose"
    , ReturnType (UTuple [UComplexMatrix; UComplexVector])
    , [UMatrix]
    , AoS );
  add_unqualified
    ( "eigendecompose"
    , ReturnType (UTuple [UComplexMatrix; UComplexVector])
    , [UComplexMatrix]
    , AoS );
  add_unqualified ("eigenvalues", ReturnType UComplexVector, [UMatrix], AoS);
  add_unqualified ("eigenvectors", ReturnType UComplexMatrix, [UMatrix], AoS);
  add_unqualified
    ("eigenvalues", ReturnType UComplexVector, [UComplexMatrix], AoS);
  add_unqualified
    ("eigenvectors", ReturnType UComplexMatrix, [UComplexMatrix], AoS);
  add_unqualified
    ( "eigendecompose_sym"
    , ReturnType (UTuple [UMatrix; UVector])
    , [UMatrix]
    , AoS );
  add_unqualified
    ( "eigendecompose_sym"
    , ReturnType (UTuple [UComplexMatrix; UComplexVector])
    , [UComplexMatrix]
    , AoS );
  add_unqualified ("eigenvalues_sym", ReturnType UVector, [UMatrix], AoS);
  add_unqualified
    ("eigenvalues_sym", ReturnType UComplexVector, [UComplexMatrix], AoS);
  add_unqualified
    ("eigenvectors_sym", ReturnType UComplexMatrix, [UComplexMatrix], AoS);
  add_unqualified ("eigenvectors_sym", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified ("generalized_inverse", ReturnType UMatrix, [UMatrix], SoA);
  add_unqualified ("qr", ReturnType (UTuple [UMatrix; UMatrix]), [UMatrix], AoS);
  add_unqualified ("qr_Q", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified ("qr_R", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified
    ("qr_thin", ReturnType (UTuple [UMatrix; UMatrix]), [UMatrix], AoS);
  add_unqualified ("qr_thin_Q", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified ("qr_thin_R", ReturnType UMatrix, [UMatrix], AoS);
  List.iter
    ~f:(fun x -> add_unqualified ("elt_divide", ReturnType x, [x; x], SoA))
    bare_types;
  add_unqualified ("elt_divide", ReturnType UVector, [UVector; UReal], SoA);
  add_unqualified ("elt_divide", ReturnType URowVector, [URowVector; UReal], SoA);
  add_unqualified ("elt_divide", ReturnType UMatrix, [UMatrix; UReal], SoA);
  add_unqualified ("elt_divide", ReturnType UVector, [UReal; UVector], SoA);
  add_unqualified ("elt_divide", ReturnType URowVector, [UReal; URowVector], SoA);
  add_unqualified ("elt_divide", ReturnType UMatrix, [UReal; UMatrix], SoA);
  add_unqualified
    ("elt_divide", ReturnType UComplexVector, [UComplexVector; UComplex], SoA);
  add_unqualified
    ( "elt_divide"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UComplex]
    , SoA );
  add_unqualified
    ("elt_divide", ReturnType UComplexMatrix, [UComplexMatrix; UComplex], SoA);
  add_unqualified
    ("elt_divide", ReturnType UComplexVector, [UComplex; UComplexVector], SoA);
  add_unqualified
    ( "elt_divide"
    , ReturnType UComplexRowVector
    , [UComplex; UComplexRowVector]
    , SoA );
  add_unqualified
    ("elt_divide", ReturnType UComplexMatrix, [UComplex; UComplexMatrix], SoA);
  List.iter
    ~f:(fun x -> add_unqualified ("elt_multiply", ReturnType x, [x; x], SoA))
    bare_types;
  add_unqualified ("elt_multiply", ReturnType UVector, [UVector; UReal], SoA);
  add_unqualified
    ("elt_multiply", ReturnType URowVector, [URowVector; UReal], SoA);
  add_unqualified ("elt_multiply", ReturnType UMatrix, [UMatrix; UReal], SoA);
  add_unqualified ("elt_multiply", ReturnType UVector, [UReal; UVector], SoA);
  add_unqualified
    ("elt_multiply", ReturnType URowVector, [UReal; URowVector], SoA);
  add_unqualified ("elt_multiply", ReturnType UMatrix, [UReal; UMatrix], SoA);
  add_unqualified
    ("elt_multiply", ReturnType UComplexVector, [UComplexVector; UComplex], SoA);
  add_unqualified
    ( "elt_multiply"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UComplex]
    , SoA );
  add_unqualified
    ("elt_multiply", ReturnType UComplexMatrix, [UComplexMatrix; UComplex], SoA);
  add_unqualified
    ("elt_multiply", ReturnType UComplexVector, [UComplex; UComplexVector], SoA);
  add_unqualified
    ( "elt_multiply"
    , ReturnType UComplexRowVector
    , [UComplex; UComplexRowVector]
    , SoA );
  add_unqualified
    ("elt_multiply", ReturnType UComplexMatrix, [UComplex; UComplexMatrix], SoA);
  add_unqualified ("exp", ReturnType UComplex, [UComplex], AoS);
  add_binary_vec_int_int "falling_factorial" SoA;
  add_binary_vec_real_int "falling_factorial" SoA;
  add_binary_vec "fdim" AoS;
  add_unqualified ("fft", ReturnType UComplexVector, [UComplexVector], AoS);
  add_unqualified ("fft2", ReturnType UComplexMatrix, [UComplexMatrix], AoS);
  add_ternary_vec "fma" SoA;
  add_binary_vec "fmax" AoS;
  add_binary_vec "fmin" AoS;
  add_binary_vec "fmod" AoS;
  add_binary_vec_real_real "gamma_p" AoS;
  add_binary_vec_real_real "gamma_q" AoS;
  add_unqualified
    ( "gaussian_dlm_obs_lpdf"
    , ReturnType UReal
    , [UMatrix; UMatrix; UMatrix; UVector; UMatrix; UVector; UMatrix]
    , AoS );
  add_unqualified
    ("gp_dot_prod_cov", ReturnType UMatrix, [UArray UReal; UReal], AoS);
  add_unqualified
    ( "gp_dot_prod_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal]
    , AoS );
  add_unqualified
    ("gp_dot_prod_cov", ReturnType UMatrix, [UArray UVector; UReal], AoS);
  add_unqualified
    ( "gp_dot_prod_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal]
    , AoS );
  add_unqualified
    ("gp_exp_quad_cov", ReturnType UMatrix, [UArray UReal; UReal; UReal], AoS);
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal]
    , AoS );
  add_unqualified
    ("gp_exp_quad_cov", ReturnType UMatrix, [UArray UVector; UReal; UReal], AoS);
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal]
    , AoS );
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UArray UReal]
    , AoS );
  add_unqualified
    ( "gp_exp_quad_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UArray UReal]
    , AoS );
  add_unqualified
    ("gp_matern32_cov", ReturnType UMatrix, [UArray UReal; UReal; UReal], AoS);
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal]
    , AoS );
  add_unqualified
    ("gp_matern32_cov", ReturnType UMatrix, [UArray UVector; UReal; UReal], AoS);
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal]
    , AoS );
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UArray UReal]
    , AoS );
  add_unqualified
    ( "gp_matern32_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UArray UReal]
    , AoS );
  add_unqualified
    ("gp_matern52_cov", ReturnType UMatrix, [UArray UReal; UReal; UReal], AoS);
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal]
    , AoS );
  add_unqualified
    ("gp_matern52_cov", ReturnType UMatrix, [UArray UVector; UReal; UReal], AoS);
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal]
    , AoS );
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UArray UReal]
    , AoS );
  add_unqualified
    ( "gp_matern52_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UArray UReal]
    , AoS );
  add_unqualified
    ("gp_exponential_cov", ReturnType UMatrix, [UArray UReal; UReal; UReal], AoS);
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal]
    , AoS );
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UReal]
    , AoS );
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal]
    , AoS );
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UArray UReal]
    , AoS );
  add_unqualified
    ( "gp_exponential_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UArray UReal]
    , AoS );
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [UArray UReal; UReal; UReal; UReal]
    , AoS );
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [UArray UReal; UArray UReal; UReal; UReal; UReal]
    , AoS );
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [UArray UVector; UReal; UReal; UReal]
    , AoS );
  add_unqualified
    ( "gp_periodic_cov"
    , ReturnType UMatrix
    , [UArray UVector; UArray UVector; UReal; UReal; UReal]
    , AoS );
  add_unqualified ("head", ReturnType URowVector, [URowVector; UInt], SoA);
  add_unqualified ("head", ReturnType UVector, [UVector; UInt], SoA);
  add_unqualified
    ("head", ReturnType UComplexRowVector, [UComplexRowVector; UInt], AoS);
  add_unqualified
    ("head", ReturnType UComplexVector, [UComplexVector; UInt], AoS);
  List.iter
    ~f:(fun t ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( "head"
            , ReturnType (bare_array_type (t, j))
            , [bare_array_type (t, j); UInt]
            , SoA ))
        (List.range 1 4))
    bare_types;
  add_unqualified
    ("hmm_marginal", ReturnType UReal, [UMatrix; UMatrix; UVector], AoS);
  add_qualified
    ( "hmm_hidden_state_prob"
    , ReturnType UMatrix
    , [(DataOnly, UMatrix); (DataOnly, UMatrix); (DataOnly, UVector)]
    , AoS );
  add_unqualified ("hypergeometric_1F0", ReturnType UReal, [UReal; UReal], AoS);
  add_unqualified
    ("hypergeometric_2F1", ReturnType UReal, [UReal; UReal; UReal; UReal], AoS);
  let vectors = [UnsizedType.UVector; URowVector; UArray UReal] in
  List.iter
    ~f:(fun t1 ->
      List.iter
        ~f:(fun t2 ->
          add_unqualified
            ("hypergeometric_3F2", ReturnType UReal, [t1; t2; UReal], AoS);
          add_unqualified
            ("hypergeometric_pFq", ReturnType UReal, [t1; t2; UReal], AoS))
        vectors)
    vectors;
  add_binary_vec "hypot" AoS;
  add_unqualified ("identity_matrix", ReturnType UMatrix, [UInt], SoA);
  add_unqualified ("inc_beta", ReturnType UReal, [UReal; UReal; UReal], SoA);
  add_unqualified ("int_step", ReturnType UInt, [UReal], SoA);
  add_unqualified ("int_step", ReturnType UInt, [UInt], SoA);
  add_qualified
    ( "integrate_1d"
    , ReturnType UReal
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
              ; (DataOnly, UArray UInt) ]
            , ReturnType UReal
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UReal); (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt) ]
    , AoS );
  add_qualified
    ( "integrate_1d"
    , ReturnType UReal
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UReal)
              ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
              ; (DataOnly, UArray UInt) ]
            , ReturnType UReal
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UReal); (AutoDiffable, UReal)
      ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt); (DataOnly, UReal) ]
    , AoS );
  add_qualified
    ( "integrate_ode"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
              ; (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt) ]
    , AoS );
  add_qualified
    ( "integrate_ode_adams"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
              ; (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt) ]
    , AoS );
  add_qualified
    ( "integrate_ode_adams"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
              ; (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt); (DataOnly, UReal); (DataOnly, UReal)
      ; (DataOnly, UReal) ]
    , AoS );
  add_qualified
    ( "integrate_ode_bdf"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
              ; (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt) ]
    , AoS );
  add_qualified
    ( "integrate_ode_bdf"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
              ; (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt); (DataOnly, UReal); (DataOnly, UReal)
      ; (DataOnly, UReal) ]
    , AoS );
  add_qualified
    ( "integrate_ode_rk45"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
              ; (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt) ]
    , AoS );
  add_qualified
    ( "integrate_ode_rk45"
    , ReturnType (UArray (UArray UReal))
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
              ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
              ; (DataOnly, UArray UInt) ]
            , ReturnType (UArray UReal)
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UReal); (AutoDiffable, UArray UReal)
      ; (AutoDiffable, UArray UReal); (DataOnly, UArray UReal)
      ; (DataOnly, UArray UInt); (DataOnly, UReal); (DataOnly, UReal)
      ; (DataOnly, UReal) ]
    , AoS );
  add_unqualified ("inv_fft", ReturnType UComplexVector, [UComplexVector], AoS);
  add_unqualified ("inv_fft2", ReturnType UComplexMatrix, [UComplexMatrix], AoS);
  add_unqualified ("inv_inc_beta", ReturnType UReal, [UReal; UReal; UReal], SoA);
  add_unqualified
    ("inv_wishart_cholesky_rng", ReturnType UMatrix, [UReal; UMatrix], AoS);
  add_unqualified ("inv_wishart_rng", ReturnType UMatrix, [UReal; UMatrix], AoS);
  add_unqualified ("inverse", ReturnType UMatrix, [UMatrix], SoA);
  add_unqualified ("inverse_spd", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified ("is_inf", ReturnType UInt, [UReal], SoA);
  add_unqualified ("is_nan", ReturnType UInt, [UReal], SoA);
  add_binary_vec "lbeta" AoS;
  add_binary_vec "lchoose" AoS;
  add_binary_vec_real_int "ldexp" AoS;
  add_qualified
    ( "linspaced_int_array"
    , ReturnType (UArray UInt)
    , [(DataOnly, UInt); (DataOnly, UInt); (DataOnly, UInt)]
    , SoA );
  add_qualified
    ( "linspaced_array"
    , ReturnType (UArray UReal)
    , [(DataOnly, UInt); (DataOnly, UReal); (DataOnly, UReal)]
    , SoA );
  add_qualified
    ( "linspaced_row_vector"
    , ReturnType URowVector
    , [(DataOnly, UInt); (DataOnly, UReal); (DataOnly, UReal)]
    , SoA );
  add_qualified
    ( "linspaced_vector"
    , ReturnType UVector
    , [(DataOnly, UInt); (DataOnly, UReal); (DataOnly, UReal)]
    , SoA );
  add_unqualified
    ("lkj_corr_cholesky_rng", ReturnType UMatrix, [UInt; UReal], AoS);
  add_unqualified ("lkj_corr_rng", ReturnType UMatrix, [UInt; UReal], AoS);
  add_binary_vec_int_real "lmgamma" AoS;
  add_binary_vec "lmultiply" SoA;
  add_unqualified ("log", ReturnType UComplex, [UComplex], AoS);
  add_nullary "log10";
  add_unqualified ("log10", ReturnType UComplex, [UComplex], AoS);
  add_nullary "log2";
  add_unqualified ("log_determinant", ReturnType UReal, [UMatrix], SoA);
  add_unqualified ("log_determinant_spd", ReturnType UReal, [UMatrix], SoA);
  add_binary_vec "log_diff_exp" AoS;
  add_binary_vec "log_falling_factorial" AoS;
  add_binary_vec "log_inv_logit_diff" AoS;
  add_ternary "log_mix" AoS;
  List.iter
    ~f:(fun v1 ->
      List.iter
        ~f:(fun v2 ->
          add_unqualified ("log_mix", ReturnType UReal, [v1; v2], AoS))
        (List.tl_exn vector_types);
      add_unqualified ("log_mix", ReturnType UReal, [v1; UArray UVector], AoS);
      add_unqualified ("log_mix", ReturnType UReal, [v1; UArray URowVector], AoS))
    (List.tl_exn vector_types);
  add_binary_vec "log_modified_bessel_first_kind" AoS;
  add_binary_vec "log_rising_factorial" AoS;
  add_unqualified ("log_softmax", ReturnType UVector, [UVector], SoA);
  add_unqualified ("log_sum_exp", ReturnType UReal, [UArray UReal], SoA);
  add_unqualified ("log_sum_exp", ReturnType UReal, [UVector], SoA);
  add_unqualified ("log_sum_exp", ReturnType UReal, [URowVector], SoA);
  add_unqualified ("log_sum_exp", ReturnType UReal, [UMatrix], SoA);
  add_binary_vec "log_sum_exp" AoS;
  add_unqualified ("logical_and", ReturnType UInt, [UInt; UInt], SoA);
  add_unqualified ("logical_or", ReturnType UInt, [UInt; UInt], SoA);
  add_unqualified ("logical_negation", ReturnType UInt, [UInt], SoA);
  let comparison_binops =
    [ "logical_eq"; "logical_neq"; "logical_lt"; "logical_lte"; "logical_gt"
    ; "logical_gte" ] in
  List.iter
    ~f:(fun t1 ->
      List.iter
        ~f:(fun t2 ->
          List.iter
            ~f:(fun o -> add_unqualified (o, ReturnType UInt, [t1; t2], SoA))
            comparison_binops)
        primitive_types)
    primitive_types;
  add_unqualified ("logical_eq", ReturnType UInt, [UComplex; UReal], SoA);
  add_unqualified ("logical_eq", ReturnType UInt, [UComplex; UComplex], SoA);
  add_unqualified ("logical_neq", ReturnType UInt, [UComplex; UReal], SoA);
  add_unqualified ("logical_neq", ReturnType UInt, [UComplex; UComplex], SoA);
  add_first_arg_vector_binary "lower_bound_jacobian" SoA;
  add_first_arg_vector_binary "lower_bound_constrain" SoA;
  add_first_arg_vector_binary "lower_bound_unconstrain" AoS;
  add_first_arg_vector_ternary "lower_upper_bound_jacobian" SoA;
  add_first_arg_vector_ternary "lower_upper_bound_constrain" SoA;
  add_first_arg_vector_ternary "lower_upper_bound_unconstrain" AoS;
  add_nullary "machine_precision";
  add_qualified
    ( "map_rect"
    , ReturnType UVector
    , [ ( AutoDiffable
        , UFun
            ( [ (AutoDiffable, UVector); (AutoDiffable, UVector)
              ; (DataOnly, UArray UReal); (DataOnly, UArray UInt) ]
            , ReturnType UVector
            , FnPlain
            , Mem_pattern.AoS ) ); (AutoDiffable, UVector)
      ; (AutoDiffable, UArray UVector); (DataOnly, UArray (UArray UReal))
      ; (DataOnly, UArray (UArray UInt)) ]
    , AoS );
  add_unqualified ("matrix_exp", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified
    ("matrix_exp_multiply", ReturnType UMatrix, [UMatrix; UMatrix], AoS);
  add_unqualified ("matrix_power", ReturnType UMatrix, [UMatrix; UInt], SoA);
  add_unqualified ("max", ReturnType UInt, [UArray UInt], AoS);
  add_unqualified ("max", ReturnType UReal, [UArray UReal], AoS);
  add_unqualified ("max", ReturnType UReal, [UVector], AoS);
  add_unqualified ("max", ReturnType UReal, [URowVector], AoS);
  add_unqualified ("max", ReturnType UReal, [UMatrix], AoS);
  add_unqualified ("max", ReturnType UInt, [UInt; UInt], AoS);
  add_unqualified ("mdivide_left", ReturnType UVector, [UMatrix; UVector], SoA);
  add_unqualified ("mdivide_left", ReturnType UMatrix, [UMatrix; UMatrix], SoA);
  add_unqualified
    ("mdivide_left_spd", ReturnType UVector, [UMatrix; UVector], SoA);
  add_unqualified
    ("mdivide_left_spd", ReturnType UMatrix, [UMatrix; UMatrix], SoA);
  add_unqualified
    ("mdivide_left_tri_low", ReturnType UMatrix, [UMatrix; UMatrix], AoS);
  add_unqualified
    ("mdivide_left_tri_low", ReturnType UVector, [UMatrix; UVector], AoS);
  add_unqualified
    ("mdivide_right", ReturnType URowVector, [URowVector; UMatrix], AoS);
  add_unqualified ("mdivide_right", ReturnType UMatrix, [UMatrix; UMatrix], AoS);
  add_unqualified
    ( "mdivide_right"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UComplexMatrix]
    , AoS );
  add_unqualified
    ( "mdivide_right"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexMatrix]
    , AoS );
  add_unqualified
    ("mdivide_right_spd", ReturnType UMatrix, [UMatrix; UMatrix], AoS);
  add_unqualified
    ("mdivide_right_spd", ReturnType URowVector, [URowVector; UMatrix], AoS);
  add_unqualified
    ("mdivide_right_tri_low", ReturnType URowVector, [URowVector; UMatrix], AoS);
  add_unqualified
    ("mdivide_right_tri_low", ReturnType UMatrix, [UMatrix; UMatrix], AoS);
  add_unqualified ("mean", ReturnType UReal, [UArray UReal], SoA);
  add_unqualified ("mean", ReturnType UReal, [UVector], AoS);
  add_unqualified ("mean", ReturnType UReal, [URowVector], AoS);
  add_unqualified ("mean", ReturnType UReal, [UMatrix], AoS);
  add_unqualified ("min", ReturnType UInt, [UArray UInt], AoS);
  add_unqualified ("min", ReturnType UReal, [UArray UReal], AoS);
  add_unqualified ("min", ReturnType UReal, [UVector], AoS);
  add_unqualified ("min", ReturnType UReal, [URowVector], AoS);
  add_unqualified ("min", ReturnType UReal, [UMatrix], AoS);
  add_unqualified ("min", ReturnType UInt, [UInt; UInt], AoS);
  add_binary_vec_int_real "modified_bessel_first_kind" AoS;
  add_binary_vec_int_real "modified_bessel_second_kind" AoS;
  add_unqualified ("modulus", ReturnType UInt, [UInt; UInt], AoS);
  add_unqualified
    ("multi_normal_rng", ReturnType UVector, [UVector; UMatrix], AoS);
  add_unqualified
    ( "multi_normal_rng"
    , ReturnType (UArray UVector)
    , [UArray UVector; UMatrix]
    , AoS );
  add_unqualified
    ("multi_normal_rng", ReturnType UVector, [URowVector; UMatrix], AoS);
  add_unqualified
    ( "multi_normal_rng"
    , ReturnType (UArray UVector)
    , [UArray URowVector; UMatrix]
    , AoS );
  add_unqualified
    ("multi_normal_cholesky_rng", ReturnType UVector, [UVector; UMatrix], AoS);
  add_unqualified
    ( "multi_normal_cholesky_rng"
    , ReturnType (UArray UVector)
    , [UArray UVector; UMatrix]
    , AoS );
  add_unqualified
    ("multi_normal_cholesky_rng", ReturnType UVector, [URowVector; UMatrix], AoS);
  add_unqualified
    ( "multi_normal_cholesky_rng"
    , ReturnType (UArray UVector)
    , [UArray URowVector; UMatrix]
    , AoS );
  add_unqualified
    ("multi_student_t_rng", ReturnType UVector, [UReal; UVector; UMatrix], AoS);
  add_unqualified
    ( "multi_student_t_rng"
    , ReturnType (UArray UVector)
    , [UReal; UArray UVector; UMatrix]
    , AoS );
  add_unqualified
    ( "multi_student_t_rng"
    , ReturnType UVector
    , [UReal; URowVector; UMatrix]
    , AoS );
  add_unqualified
    ( "multi_student_t_rng"
    , ReturnType (UArray UVector)
    , [UReal; UArray URowVector; UMatrix]
    , AoS );
  add_unqualified
    ( "multi_student_t_cholesky_rng"
    , ReturnType UVector
    , [UReal; UVector; UMatrix]
    , AoS );
  add_unqualified
    ( "multi_student_t_cholesky_rng"
    , ReturnType (UArray UVector)
    , [UReal; UArray UVector; UMatrix]
    , AoS );
  add_unqualified
    ( "multi_student_t_cholesky_rng"
    , ReturnType UVector
    , [UReal; URowVector; UMatrix]
    , AoS );
  add_unqualified
    ( "multi_student_t_cholesky_rng"
    , ReturnType (UArray UVector)
    , [UReal; UArray URowVector; UMatrix]
    , AoS );
  add_unqualified
    ("multinomial_logit_rng", ReturnType (UArray UInt), [UVector; UInt], AoS);
  add_unqualified
    ("multinomial_rng", ReturnType (UArray UInt), [UVector; UInt], AoS);
  add_unqualified ("multiply", ReturnType UComplex, [UComplex; UComplex], AoS);
  add_unqualified ("multiply", ReturnType UInt, [UInt; UInt], SoA);
  add_unqualified ("multiply", ReturnType UReal, [UReal; UReal], SoA);
  add_unqualified ("multiply", ReturnType UVector, [UVector; UReal], SoA);
  add_unqualified ("multiply", ReturnType URowVector, [URowVector; UReal], SoA);
  add_unqualified ("multiply", ReturnType UMatrix, [UMatrix; UReal], SoA);
  add_unqualified ("multiply", ReturnType UReal, [URowVector; UVector], SoA);
  add_unqualified ("multiply", ReturnType UMatrix, [UVector; URowVector], SoA);
  add_unqualified ("multiply", ReturnType UVector, [UMatrix; UVector], SoA);
  add_unqualified ("multiply", ReturnType URowVector, [URowVector; UMatrix], SoA);
  add_unqualified ("multiply", ReturnType UMatrix, [UMatrix; UMatrix], SoA);
  add_unqualified ("multiply", ReturnType UVector, [UReal; UVector], SoA);
  add_unqualified ("multiply", ReturnType URowVector, [UReal; URowVector], SoA);
  add_unqualified ("multiply", ReturnType UMatrix, [UReal; UMatrix], SoA);
  (* TODO more complex overloads *)
  add_unqualified
    ( "multiply"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexMatrix]
    , SoA );
  add_unqualified
    ("multiply", ReturnType UComplexMatrix, [UComplexMatrix; UComplex], SoA);
  add_unqualified
    ("multiply", ReturnType UComplexMatrix, [UComplex; UComplexMatrix], SoA);
  add_unqualified
    ( "multiply"
    , ReturnType UComplexMatrix
    , [UComplexVector; UComplexRowVector]
    , SoA );
  add_unqualified
    ("multiply", ReturnType UComplex, [UComplexRowVector; UComplexVector], SoA);
  add_unqualified
    ( "multiply"
    , ReturnType UComplexVector
    , [UComplexMatrix; UComplexVector]
    , SoA );
  add_unqualified
    ("multiply", ReturnType UComplexVector, [UComplexVector; UComplex], SoA);
  add_unqualified
    ("multiply", ReturnType UComplexVector, [UComplex; UComplexVector], SoA);
  add_unqualified
    ( "multiply"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UComplex]
    , SoA );
  add_unqualified
    ( "multiply"
    , ReturnType UComplexRowVector
    , [UComplex; UComplexRowVector]
    , SoA );
  add_unqualified
    ( "multiply"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UComplexMatrix]
    , SoA );
  add_unqualified
    ("multiply_lower_tri_self_transpose", ReturnType UMatrix, [UMatrix], SoA);
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UVector; UVector; UReal]
    , SoA );
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UVector; UReal]
    , SoA );
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UReal; UVector; UReal]
    , SoA );
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UVector; UReal]
    , SoA );
  add_nullary "negative_infinity";
  add_unqualified ("norm", ReturnType UReal, [UComplex], AoS);
  add_unqualified ("norm1", ReturnType UReal, [UArray UReal], SoA);
  add_unqualified ("norm1", ReturnType UReal, [UVector], SoA);
  add_unqualified ("norm1", ReturnType UReal, [URowVector], SoA);
  add_unqualified ("norm2", ReturnType UReal, [UArray UReal], SoA);
  add_unqualified ("norm2", ReturnType UReal, [UVector], SoA);
  add_unqualified ("norm2", ReturnType UReal, [URowVector], SoA);
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; UMatrix; UVector; UVector; UReal]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UReal; UMatrix; UReal; UVector; UReal]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UReal; UMatrix; UVector; UVector; UReal]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UReal; UMatrix; UReal; UVector; UVector]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UReal; UMatrix; UVector; UVector; UVector]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; URowVector; UReal; UVector; UVector]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; URowVector; UVector; UVector; UReal]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; URowVector; UVector; UVector; UVector]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; URowVector; UReal; UVector; UReal]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; UMatrix; UReal; UVector; UVector]
    , SoA );
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; UMatrix; UVector; UVector; UVector]
    , SoA );
  add_nullary "not_a_number";
  add_unqualified ("num_elements", ReturnType UInt, [UMatrix], SoA);
  add_unqualified ("num_elements", ReturnType UInt, [UVector], SoA);
  add_unqualified ("num_elements", ReturnType UInt, [URowVector], SoA);
  add_unqualified ("num_elements", ReturnType UInt, [UComplexMatrix], SoA);
  add_unqualified ("num_elements", ReturnType UInt, [UComplexVector], SoA);
  add_unqualified ("num_elements", ReturnType UInt, [UComplexRowVector], SoA);
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified
            ("num_elements", ReturnType UInt, [bare_array_type (t, i)], SoA))
        bare_types)
    (List.range 1 10);
  add_first_arg_vector_ternary "offset_multiplier_jacobian" SoA;
  add_first_arg_vector_ternary "offset_multiplier_constrain" SoA;
  add_first_arg_vector_ternary "offset_multiplier_unconstrain" AoS;
  add_unqualified
    ("one_hot_int_array", ReturnType (UArray UInt), [UInt; UInt], SoA);
  add_unqualified ("one_hot_array", ReturnType (UArray UReal), [UInt; UInt], SoA);
  add_unqualified
    ("one_hot_row_vector", ReturnType URowVector, [UInt; UInt], SoA);
  add_unqualified ("one_hot_vector", ReturnType UVector, [UInt; UInt], SoA);
  add_unqualified ("ones_int_array", ReturnType (UArray UInt), [UInt], SoA);
  add_unqualified ("ones_array", ReturnType (UArray UReal), [UInt], SoA);
  add_unqualified ("ones_row_vector", ReturnType URowVector, [UInt], SoA);
  add_unqualified ("ones_vector", ReturnType UVector, [UInt], SoA);
  add_nested_unary UVector "ordered_jacobian" SoA;
  add_nested_unary UVector "ordered_constrain" SoA;
  add_nested_unary UVector "ordered_unconstrain" AoS;
  add_unqualified
    ( "ordered_logistic_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UVector]
    , SoA );
  add_unqualified
    ( "ordered_logistic_glm_lpmf"
    , ReturnType UReal
    , [UInt; URowVector; UVector; UVector]
    , SoA );
  add_unqualified
    ( "ordered_logistic_lpmf"
    , ReturnType UReal
    , [UArray UInt; UVector; UVector]
    , SoA );
  add_unqualified
    ( "ordered_logistic_lpmf"
    , ReturnType UReal
    , [UArray UInt; UVector; UArray UVector]
    , SoA );
  add_unqualified
    ("ordered_logistic_rng", ReturnType UInt, [UReal; UVector], AoS);
  add_unqualified
    ("ordered_probit_lpmf", ReturnType UReal, [UArray UInt; UReal; UVector], AoS);
  add_unqualified
    ( "ordered_probit_lpmf"
    , ReturnType UReal
    , [UArray UInt; UReal; UArray UVector]
    , AoS );
  add_unqualified
    ( "ordered_probit_lpmf"
    , ReturnType UReal
    , [UArray UInt; UVector; UVector]
    , AoS );
  add_unqualified
    ( "ordered_probit_lpmf"
    , ReturnType UReal
    , [UArray UInt; UVector; UArray UVector]
    , AoS );
  add_unqualified ("ordered_probit_rng", ReturnType UInt, [UReal; UVector], AoS);
  add_binary_vec_real_real "owens_t" AoS;
  add_nullary "pi";
  add_unqualified ("plus", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("plus", ReturnType UInt, [UInt], SoA);
  add_unqualified ("plus", ReturnType UReal, [UReal], SoA);
  add_unqualified ("plus", ReturnType UVector, [UVector], SoA);
  add_unqualified ("plus", ReturnType URowVector, [URowVector], SoA);
  add_unqualified ("plus", ReturnType UMatrix, [UMatrix], SoA);
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; UMatrix; UVector; UVector]
    , SoA );
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UInt; UMatrix; UVector; UVector]
    , SoA );
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UReal; UVector]
    , SoA );
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [UArray UInt; URowVector; UVector; UVector]
    , SoA );
  add_unqualified ("polar", ReturnType UComplex, [UReal; UReal], AoS);
  add_nullary "positive_infinity";
  add_nested_unary UVector "positive_ordered_jacobian" SoA;
  add_nested_unary UVector "positive_ordered_constrain" SoA;
  add_nested_unary UVector "positive_ordered_unconstrain" AoS;
  add_binary_vec "pow" AoS;
  add_binary_vec_complex_complex "pow" AoS;
  add_unqualified ("prod", ReturnType UInt, [UArray UInt], AoS);
  add_unqualified ("prod", ReturnType UReal, [UArray UReal], AoS);
  add_unqualified ("prod", ReturnType UReal, [UVector], AoS);
  add_unqualified ("prod", ReturnType UReal, [URowVector], AoS);
  add_unqualified ("prod", ReturnType UReal, [UMatrix], AoS);
  add_unqualified ("prod", ReturnType UComplex, [UArray UComplex], AoS);
  add_unqualified ("prod", ReturnType UComplex, [UComplexVector], AoS);
  add_unqualified ("prod", ReturnType UComplex, [UComplexRowVector], AoS);
  add_unqualified ("prod", ReturnType UComplex, [UComplexMatrix], AoS);
  add_unqualified ("proj", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("quad_form", ReturnType UReal, [UMatrix; UVector], SoA);
  add_unqualified ("quad_form", ReturnType UMatrix, [UMatrix; UMatrix], SoA);
  add_unqualified ("quad_form_sym", ReturnType UReal, [UMatrix; UVector], AoS);
  add_unqualified ("quad_form_sym", ReturnType UMatrix, [UMatrix; UMatrix], AoS);
  add_unqualified ("quad_form_diag", ReturnType UMatrix, [UMatrix; UVector], AoS);
  add_unqualified
    ("quad_form_diag", ReturnType UMatrix, [UMatrix; URowVector], AoS);
  add_qualified
    ( "quantile"
    , ReturnType UReal
    , [(DataOnly, UArray UReal); (DataOnly, UReal)]
    , SoA );
  add_qualified
    ( "quantile"
    , ReturnType (UArray UReal)
    , [(DataOnly, UArray UReal); (DataOnly, UArray UReal)]
    , SoA );
  add_qualified
    ("quantile", ReturnType UReal, [(DataOnly, UVector); (DataOnly, UReal)], SoA);
  add_qualified
    ( "quantile"
    , ReturnType (UArray UReal)
    , [(DataOnly, UVector); (DataOnly, UArray UReal)]
    , SoA );
  add_qualified
    ( "quantile"
    , ReturnType UReal
    , [(DataOnly, URowVector); (DataOnly, UReal)]
    , SoA );
  add_qualified
    ( "quantile"
    , ReturnType (UArray UReal)
    , [(DataOnly, URowVector); (DataOnly, UArray UReal)]
    , SoA );
  add_unqualified ("rank", ReturnType UInt, [UArray UInt; UInt], AoS);
  add_unqualified ("rank", ReturnType UInt, [UArray UReal; UInt], AoS);
  add_unqualified ("rank", ReturnType UInt, [UVector; UInt], AoS);
  add_unqualified ("rank", ReturnType UInt, [URowVector; UInt], AoS);
  add_unqualified ("append_row", ReturnType UMatrix, [UMatrix; UMatrix], AoS);
  add_unqualified ("append_row", ReturnType UMatrix, [URowVector; UMatrix], AoS);
  add_unqualified ("append_row", ReturnType UMatrix, [UMatrix; URowVector], AoS);
  add_unqualified
    ("append_row", ReturnType UMatrix, [URowVector; URowVector], AoS);
  add_unqualified ("append_row", ReturnType UVector, [UVector; UVector], AoS);
  add_unqualified ("append_row", ReturnType UVector, [UReal; UVector], AoS);
  add_unqualified ("append_row", ReturnType UVector, [UVector; UReal], AoS);
  add_unqualified
    ( "append_row"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexMatrix]
    , AoS );
  add_unqualified
    ( "append_row"
    , ReturnType UComplexMatrix
    , [UComplexRowVector; UComplexMatrix]
    , AoS );
  add_unqualified
    ( "append_row"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UComplexRowVector]
    , AoS );
  add_unqualified
    ( "append_row"
    , ReturnType UComplexMatrix
    , [UComplexRowVector; UComplexRowVector]
    , AoS );
  add_unqualified
    ( "append_row"
    , ReturnType UComplexVector
    , [UComplexVector; UComplexVector]
    , AoS );
  add_unqualified
    ("append_row", ReturnType UComplexVector, [UComplex; UComplexVector], AoS);
  add_unqualified
    ("append_row", ReturnType UComplexVector, [UComplexVector; UComplex], AoS);
  List.iter
    ~f:(fun t ->
      add_unqualified
        ("rep_array", ReturnType (bare_array_type (t, 1)), [t; UInt], SoA);
      add_unqualified
        ("rep_array", ReturnType (bare_array_type (t, 2)), [t; UInt; UInt], SoA);
      add_unqualified
        ( "rep_array"
        , ReturnType (bare_array_type (t, 3))
        , [t; UInt; UInt; UInt]
        , SoA );
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( "rep_array"
            , ReturnType (bare_array_type (t, j + 1))
            , [bare_array_type (t, j); UInt]
            , SoA );
          add_unqualified
            ( "rep_array"
            , ReturnType (bare_array_type (t, j + 2))
            , [bare_array_type (t, j); UInt; UInt]
            , SoA );
          add_unqualified
            ( "rep_array"
            , ReturnType (bare_array_type (t, j + 3))
            , [bare_array_type (t, j); UInt; UInt; UInt]
            , SoA ))
        (List.range 1 3))
    bare_types;
  add_unqualified ("rep_matrix", ReturnType UMatrix, [UReal; UInt; UInt], SoA);
  add_unqualified ("rep_matrix", ReturnType UMatrix, [UVector; UInt], SoA);
  add_unqualified ("rep_matrix", ReturnType UMatrix, [URowVector; UInt], SoA);
  add_unqualified
    ("rep_matrix", ReturnType UComplexMatrix, [UComplex; UInt; UInt], AoS);
  add_unqualified
    ("rep_matrix", ReturnType UComplexMatrix, [UComplexVector; UInt], AoS);
  add_unqualified
    ("rep_matrix", ReturnType UComplexMatrix, [UComplexRowVector; UInt], AoS);
  add_unqualified ("rep_row_vector", ReturnType URowVector, [UReal; UInt], SoA);
  add_unqualified
    ("rep_row_vector", ReturnType UComplexRowVector, [UComplex; UInt], AoS);
  add_unqualified ("rep_vector", ReturnType UVector, [UReal; UInt], SoA);
  add_unqualified
    ("rep_vector", ReturnType UComplexVector, [UComplex; UInt], AoS);
  add_unqualified ("reverse", ReturnType UVector, [UVector], SoA);
  add_unqualified ("reverse", ReturnType URowVector, [URowVector], SoA);
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified
            ( "reverse"
            , ReturnType (bare_array_type (t, i))
            , [bare_array_type (t, i)]
            , SoA ))
        bare_types)
    (List.range 1 8);
  add_unqualified ("reverse", ReturnType UComplexVector, [UComplexVector], SoA);
  add_unqualified
    ("reverse", ReturnType UComplexRowVector, [UComplexRowVector], SoA);
  add_binary_vec_int_int "rising_factorial" AoS;
  add_binary_vec_real_int "rising_factorial" AoS;
  add_unqualified ("row", ReturnType URowVector, [UMatrix; UInt], SoA);
  add_unqualified
    ("row", ReturnType UComplexRowVector, [UComplexMatrix; UInt], AoS);
  add_unqualified ("rows", ReturnType UInt, [UVector], SoA);
  add_unqualified ("rows", ReturnType UInt, [URowVector], SoA);
  add_unqualified ("rows", ReturnType UInt, [UMatrix], SoA);
  add_unqualified ("rows", ReturnType UInt, [UComplexVector], SoA);
  add_unqualified ("rows", ReturnType UInt, [UComplexRowVector], SoA);
  add_unqualified ("rows", ReturnType UInt, [UComplexMatrix], SoA);
  add_unqualified
    ("rows_dot_product", ReturnType UVector, [UVector; UVector], AoS);
  add_unqualified
    ("rows_dot_product", ReturnType UVector, [URowVector; URowVector], AoS);
  add_unqualified
    ("rows_dot_product", ReturnType UVector, [UMatrix; UMatrix], SoA);
  add_unqualified
    ( "rows_dot_product"
    , ReturnType UComplexVector
    , [UComplexVector; UComplexVector]
    , AoS );
  add_unqualified
    ( "rows_dot_product"
    , ReturnType UComplexVector
    , [UComplexRowVector; UComplexRowVector]
    , AoS );
  add_unqualified
    ( "rows_dot_product"
    , ReturnType UComplexVector
    , [UComplexMatrix; UComplexMatrix]
    , AoS );
  add_unqualified ("rows_dot_self", ReturnType UVector, [UVector], SoA);
  add_unqualified ("rows_dot_self", ReturnType UVector, [URowVector], SoA);
  add_unqualified ("rows_dot_self", ReturnType UVector, [UMatrix], SoA);
  add_unqualified
    ("rows_dot_self", ReturnType UComplexVector, [UComplexVector], AoS);
  add_unqualified
    ("rows_dot_self", ReturnType UComplexVector, [UComplexRowVector], AoS);
  add_unqualified
    ("rows_dot_self", ReturnType UComplexVector, [UComplexMatrix], AoS);
  add_unqualified
    ( "scale_matrix_exp_multiply"
    , ReturnType UMatrix
    , [UReal; UMatrix; UMatrix]
    , AoS );
  add_unqualified ("sd", ReturnType UReal, [UArray UReal], SoA);
  add_unqualified ("sd", ReturnType UReal, [UVector], SoA);
  add_unqualified ("sd", ReturnType UReal, [URowVector], SoA);
  add_unqualified ("sd", ReturnType UReal, [UMatrix], SoA);
  add_unqualified
    ("segment", ReturnType URowVector, [URowVector; UInt; UInt], SoA);
  add_unqualified ("segment", ReturnType UVector, [UVector; UInt; UInt], SoA);
  add_unqualified
    ( "segment"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UInt; UInt]
    , AoS );
  add_unqualified
    ("segment", ReturnType UComplexVector, [UComplexVector; UInt; UInt], AoS);
  List.iter
    ~f:(fun t ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( "segment"
            , ReturnType (bare_array_type (t, j))
            , [bare_array_type (t, j); UInt; UInt]
            , SoA ))
        (List.range 1 4))
    bare_types;
  add_nested_unary UVector "simplex_jacobian" SoA;
  add_nested_unary UVector "simplex_constrain" SoA;
  add_nested_unary UVector "simplex_unconstrain" AoS;
  add_unqualified ("sin", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("sinh", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("singular_values", ReturnType UVector, [UMatrix], SoA);
  add_unqualified ("singular_values", ReturnType UVector, [UComplexMatrix], AoS);
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_unqualified
            ("size", ReturnType UInt, [bare_array_type (t, i)], SoA))
        bare_types)
    (List.range 1 8);
  List.iter
    ~f:(fun t -> add_unqualified ("size", ReturnType UInt, [t], SoA))
    bare_types;
  add_unqualified ("softmax", ReturnType UVector, [UVector], SoA);
  add_unqualified ("sort_asc", ReturnType (UArray UInt), [UArray UInt], AoS);
  add_unqualified ("sort_asc", ReturnType (UArray UReal), [UArray UReal], AoS);
  add_unqualified ("sort_asc", ReturnType UVector, [UVector], AoS);
  add_unqualified ("sort_asc", ReturnType URowVector, [URowVector], AoS);
  add_unqualified ("sort_desc", ReturnType (UArray UInt), [UArray UInt], AoS);
  add_unqualified ("sort_desc", ReturnType (UArray UReal), [UArray UReal], AoS);
  add_unqualified ("sort_desc", ReturnType UVector, [UVector], AoS);
  add_unqualified ("sort_desc", ReturnType URowVector, [URowVector], AoS);
  add_unqualified
    ("sort_indices_asc", ReturnType (UArray UInt), [UArray UInt], AoS);
  add_unqualified
    ("sort_indices_asc", ReturnType (UArray UInt), [UArray UReal], AoS);
  add_unqualified ("sort_indices_asc", ReturnType (UArray UInt), [UVector], AoS);
  add_unqualified
    ("sort_indices_asc", ReturnType (UArray UInt), [URowVector], AoS);
  add_unqualified
    ("sort_indices_desc", ReturnType (UArray UInt), [UArray UInt], AoS);
  add_unqualified
    ("sort_indices_desc", ReturnType (UArray UInt), [UArray UReal], AoS);
  add_unqualified ("sort_indices_desc", ReturnType (UArray UInt), [UVector], AoS);
  add_unqualified
    ("sort_indices_desc", ReturnType (UArray UInt), [URowVector], AoS);
  List.iter
    ~f:(fun i ->
      let t = bare_array_type (UMatrix, i) in
      add_unqualified ("stochastic_column_jacobian", ReturnType t, [t], SoA);
      add_unqualified ("stochastic_column_constrain", ReturnType t, [t], SoA);
      add_unqualified ("stochastic_column_unconstrain", ReturnType t, [t], AoS);
      add_unqualified ("stochastic_row_jacobian", ReturnType t, [t], SoA);
      add_unqualified ("stochastic_row_constrain", ReturnType t, [t], SoA);
      add_unqualified ("stochastic_row_unconstrain", ReturnType t, [t], AoS))
    (List.range 0 8);
  add_unqualified ("squared_distance", ReturnType UReal, [UReal; UReal], SoA);
  add_unqualified ("squared_distance", ReturnType UReal, [UVector; UVector], SoA);
  add_unqualified
    ("squared_distance", ReturnType UReal, [URowVector; URowVector], SoA);
  add_unqualified
    ("squared_distance", ReturnType UReal, [UVector; URowVector], SoA);
  add_unqualified
    ("squared_distance", ReturnType UReal, [URowVector; UVector], SoA);
  add_unqualified ("sqrt", ReturnType UComplex, [UComplex], AoS);
  add_nullary "sqrt2";
  add_unqualified
    ("sub_col", ReturnType UVector, [UMatrix; UInt; UInt; UInt], SoA);
  add_unqualified
    ( "sub_col"
    , ReturnType UComplexVector
    , [UComplexMatrix; UInt; UInt; UInt]
    , AoS );
  add_unqualified
    ("sub_row", ReturnType URowVector, [UMatrix; UInt; UInt; UInt], SoA);
  add_unqualified
    ( "sub_row"
    , ReturnType UComplexRowVector
    , [UComplexMatrix; UInt; UInt; UInt]
    , AoS );
  List.iter
    ~f:(fun x -> add_unqualified ("subtract", ReturnType x, [x; x], SoA))
    bare_types;
  add_unqualified ("subtract", ReturnType UVector, [UVector; UReal], SoA);
  add_unqualified ("subtract", ReturnType URowVector, [URowVector; UReal], SoA);
  add_unqualified ("subtract", ReturnType UMatrix, [UMatrix; UReal], SoA);
  add_unqualified ("subtract", ReturnType UVector, [UReal; UVector], SoA);
  add_unqualified ("subtract", ReturnType URowVector, [UReal; URowVector], SoA);
  add_unqualified ("subtract", ReturnType UMatrix, [UReal; UMatrix], SoA);
  add_unqualified
    ("subtract", ReturnType UComplexVector, [UComplexVector; UComplex], SoA);
  add_unqualified
    ( "subtract"
    , ReturnType UComplexRowVector
    , [UComplexRowVector; UComplex]
    , SoA );
  add_unqualified
    ("subtract", ReturnType UComplexMatrix, [UComplexMatrix; UComplex], SoA);
  add_unqualified
    ("subtract", ReturnType UComplexVector, [UComplex; UComplexVector], SoA);
  add_unqualified
    ( "subtract"
    , ReturnType UComplexRowVector
    , [UComplex; UComplexRowVector]
    , SoA );
  add_unqualified
    ("subtract", ReturnType UComplexMatrix, [UComplex; UComplexMatrix], SoA);
  add_unqualified ("sum", ReturnType UInt, [UArray UInt], SoA);
  add_unqualified ("sum", ReturnType UReal, [UArray UReal], SoA);
  add_unqualified ("sum", ReturnType UReal, [UVector], SoA);
  add_unqualified ("sum", ReturnType UReal, [URowVector], SoA);
  add_unqualified ("sum", ReturnType UReal, [UMatrix], SoA);
  add_unqualified ("sum", ReturnType UComplex, [UArray UComplex], SoA);
  add_unqualified ("sum", ReturnType UComplex, [UComplexVector], SoA);
  add_unqualified ("sum", ReturnType UComplex, [UComplexRowVector], SoA);
  add_unqualified ("sum", ReturnType UComplex, [UComplexMatrix], SoA);
  add_nested_unary UVector "sum_to_zero_jacobian" SoA;
  add_nested_unary UMatrix "sum_to_zero_jacobian" SoA;
  add_nested_unary UVector "sum_to_zero_constrain" SoA;
  add_nested_unary UMatrix "sum_to_zero_constrain" SoA;
  add_nested_unary UVector "sum_to_zero_unconstrain" AoS;
  add_nested_unary UMatrix "sum_to_zero_unconstrain" AoS;
  (* TODO (future): SoA inside of tuples, update following signatures *)
  add_unqualified
    ("svd", ReturnType (UTuple [UMatrix; UVector; UMatrix]), [UMatrix], AoS);
  add_unqualified
    ( "svd"
    , ReturnType (UTuple [UComplexMatrix; UVector; UComplexMatrix])
    , [UComplexMatrix]
    , AoS );
  add_unqualified ("svd_U", ReturnType UMatrix, [UMatrix], SoA);
  add_unqualified ("svd_U", ReturnType UComplexMatrix, [UComplexMatrix], SoA);
  add_unqualified ("svd_V", ReturnType UMatrix, [UMatrix], SoA);
  add_unqualified ("svd_V", ReturnType UComplexMatrix, [UComplexMatrix], SoA);
  add_unqualified
    ("symmetrize_from_lower_tri", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified
    ( "symmetrize_from_lower_tri"
    , ReturnType UComplexMatrix
    , [UComplexMatrix]
    , AoS );
  add_unqualified ("tail", ReturnType URowVector, [URowVector; UInt], SoA);
  add_unqualified ("tail", ReturnType UVector, [UVector; UInt], SoA);
  add_unqualified
    ("tail", ReturnType UComplexRowVector, [UComplexRowVector; UInt], AoS);
  add_unqualified
    ("tail", ReturnType UComplexVector, [UComplexVector; UInt], AoS);
  List.iter
    ~f:(fun t ->
      List.iter
        ~f:(fun j ->
          add_unqualified
            ( "tail"
            , ReturnType (bare_array_type (t, j))
            , [bare_array_type (t, j); UInt]
            , SoA ))
        (List.range 1 4))
    bare_types;
  add_unqualified ("tan", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("tanh", ReturnType UComplex, [UComplex], AoS);
  add_unqualified ("tcrossprod", ReturnType UMatrix, [UMatrix], SoA);
  add_unqualified ("to_array_1d", ReturnType (UArray UReal), [UMatrix], AoS);
  add_unqualified ("to_array_1d", ReturnType (UArray UReal), [UVector], AoS);
  add_unqualified ("to_array_1d", ReturnType (UArray UReal), [URowVector], AoS);
  add_unqualified
    ("to_array_1d", ReturnType (UArray UComplex), [UComplexMatrix], AoS);
  add_unqualified
    ("to_array_1d", ReturnType (UArray UComplex), [UComplexVector], AoS);
  add_unqualified
    ("to_array_1d", ReturnType (UArray UComplex), [UComplexRowVector], AoS);
  List.iter
    ~f:(fun i ->
      add_unqualified
        ( "to_array_1d"
        , ReturnType (UArray UReal)
        , [bare_array_type (UReal, i)]
        , AoS );
      add_unqualified
        ( "to_array_1d"
        , ReturnType (UArray UInt)
        , [bare_array_type (UInt, i)]
        , AoS ))
    (List.range 1 10);
  add_unqualified
    ("to_array_2d", ReturnType (bare_array_type (UReal, 2)), [UMatrix], AoS);
  add_unqualified
    ( "to_array_2d"
    , ReturnType (bare_array_type (UComplex, 2))
    , [UComplexMatrix]
    , AoS );
  add_unqualified ("to_complex", ReturnType UComplex, [], AoS);
  add_unqualified ("to_complex", ReturnType UComplex, [UReal], AoS);
  add_binary_vec_reals_to_complex "to_complex" AoS;
  List.iter
    ~f:(fun i ->
      List.iter
        ~f:(fun t ->
          add_qualified
            ( "to_int"
            , ReturnType (bare_array_type (UInt, i))
            , [(UnsizedType.DataOnly, bare_array_type (t, i))]
            , AoS ))
        [UnsizedType.UReal; UInt])
    (List.range 0 8);
  add_unqualified ("to_matrix", ReturnType UMatrix, [UMatrix], AoS);
  add_unqualified ("to_matrix", ReturnType UMatrix, [UMatrix; UInt; UInt], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [UMatrix; UInt; UInt; UInt], AoS);
  add_unqualified ("to_matrix", ReturnType UMatrix, [UVector], AoS);
  add_unqualified ("to_matrix", ReturnType UMatrix, [UVector; UInt; UInt], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [UVector; UInt; UInt; UInt], AoS);
  add_unqualified ("to_matrix", ReturnType UMatrix, [URowVector], AoS);
  add_unqualified ("to_matrix", ReturnType UMatrix, [UArray URowVector], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [URowVector; UInt; UInt], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [URowVector; UInt; UInt; UInt], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [UArray UReal; UInt; UInt], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [UArray UReal; UInt; UInt; UInt], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [UArray UInt; UInt; UInt], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [UArray UInt; UInt; UInt; UInt], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [bare_array_type (UReal, 2)], AoS);
  add_unqualified
    ("to_matrix", ReturnType UMatrix, [bare_array_type (UInt, 2)], AoS);
  add_unqualified ("to_matrix", ReturnType UComplexMatrix, [UComplexMatrix], AoS);
  add_unqualified
    ("to_matrix", ReturnType UComplexMatrix, [UComplexMatrix; UInt; UInt], AoS);
  add_unqualified
    ( "to_matrix"
    , ReturnType UComplexMatrix
    , [UComplexMatrix; UInt; UInt; UInt]
    , AoS );
  add_unqualified ("to_matrix", ReturnType UComplexMatrix, [UComplexVector], AoS);
  add_unqualified
    ("to_matrix", ReturnType UComplexMatrix, [UComplexVector; UInt; UInt], AoS);
  add_unqualified
    ( "to_matrix"
    , ReturnType UComplexMatrix
    , [UComplexVector; UInt; UInt; UInt]
    , AoS );
  add_unqualified
    ("to_matrix", ReturnType UComplexMatrix, [UComplexRowVector], AoS);
  add_unqualified
    ("to_matrix", ReturnType UComplexMatrix, [UArray UComplexRowVector], AoS);
  add_unqualified
    ( "to_matrix"
    , ReturnType UComplexMatrix
    , [UComplexRowVector; UInt; UInt]
    , AoS );
  add_unqualified
    ( "to_matrix"
    , ReturnType UComplexMatrix
    , [UComplexRowVector; UInt; UInt; UInt]
    , AoS );
  add_unqualified
    ("to_matrix", ReturnType UComplexMatrix, [UArray UComplex; UInt; UInt], AoS);
  add_unqualified
    ( "to_matrix"
    , ReturnType UComplexMatrix
    , [UArray UComplex; UInt; UInt; UInt]
    , AoS );
  add_unqualified
    ( "to_matrix"
    , ReturnType UComplexMatrix
    , [bare_array_type (UComplex, 2)]
    , AoS );
  add_unqualified ("to_row_vector", ReturnType URowVector, [UMatrix], AoS);
  add_unqualified ("to_row_vector", ReturnType URowVector, [UVector], AoS);
  add_unqualified ("to_row_vector", ReturnType URowVector, [URowVector], AoS);
  add_unqualified ("to_row_vector", ReturnType URowVector, [UArray UReal], AoS);
  add_unqualified ("to_row_vector", ReturnType URowVector, [UArray UInt], AoS);
  add_unqualified
    ("to_row_vector", ReturnType UComplexRowVector, [UComplexMatrix], AoS);
  add_unqualified
    ("to_row_vector", ReturnType UComplexRowVector, [UComplexVector], AoS);
  add_unqualified
    ("to_row_vector", ReturnType UComplexRowVector, [UComplexRowVector], AoS);
  add_unqualified
    ("to_row_vector", ReturnType UComplexRowVector, [UArray UComplex], AoS);
  add_unqualified ("to_vector", ReturnType UVector, [UMatrix], SoA);
  add_unqualified ("to_vector", ReturnType UVector, [UVector], SoA);
  add_unqualified ("to_vector", ReturnType UVector, [URowVector], SoA);
  add_unqualified ("to_vector", ReturnType UVector, [UArray UReal], AoS);
  add_unqualified ("to_vector", ReturnType UVector, [UArray UInt], AoS);
  add_unqualified ("to_vector", ReturnType UComplexVector, [UComplexMatrix], AoS);
  add_unqualified ("to_vector", ReturnType UComplexVector, [UComplexVector], AoS);
  add_unqualified
    ("to_vector", ReturnType UComplexVector, [UComplexRowVector], AoS);
  add_unqualified
    ("to_vector", ReturnType UComplexVector, [UArray UComplex], AoS);
  add_unqualified ("trace", ReturnType UReal, [UMatrix], SoA);
  add_unqualified ("trace", ReturnType UComplex, [UComplexMatrix], AoS);
  add_unqualified
    ("trace_gen_quad_form", ReturnType UReal, [UMatrix; UMatrix; UMatrix], SoA);
  add_unqualified ("trace_quad_form", ReturnType UReal, [UMatrix; UVector], SoA);
  add_unqualified ("trace_quad_form", ReturnType UReal, [UMatrix; UMatrix], SoA);
  add_unqualified ("transpose", ReturnType URowVector, [UVector], SoA);
  add_unqualified ("transpose", ReturnType UVector, [URowVector], SoA);
  add_unqualified ("transpose", ReturnType UMatrix, [UMatrix], SoA);
  add_unqualified
    ("transpose", ReturnType UComplexRowVector, [UComplexVector], SoA);
  add_unqualified
    ("transpose", ReturnType UComplexVector, [UComplexRowVector], SoA);
  add_unqualified ("transpose", ReturnType UComplexMatrix, [UComplexMatrix], SoA);
  add_unqualified ("uniform_simplex", ReturnType UVector, [UInt], SoA);
  add_nested_unary UVector "unit_vector_jacobian" SoA;
  add_nested_unary UVector "unit_vector_constrain" SoA;
  add_nested_unary UVector "unit_vector_unconstrain" AoS;
  add_first_arg_vector_binary "upper_bound_jacobian" SoA;
  add_first_arg_vector_binary "upper_bound_constrain" SoA;
  add_first_arg_vector_binary "upper_bound_unconstrain" AoS;
  add_unqualified ("variance", ReturnType UReal, [UArray UReal], SoA);
  add_unqualified ("variance", ReturnType UReal, [UVector], SoA);
  add_unqualified ("variance", ReturnType UReal, [URowVector], SoA);
  add_unqualified ("variance", ReturnType UReal, [UMatrix], SoA);
  add_unqualified
    ("wishart_cholesky_rng", ReturnType UMatrix, [UReal; UMatrix], AoS);
  add_unqualified ("wishart_rng", ReturnType UMatrix, [UReal; UMatrix], AoS);
  add_unqualified ("zeros_int_array", ReturnType (UArray UInt), [UInt], SoA);
  add_unqualified ("zeros_array", ReturnType (UArray UReal), [UInt], SoA);
  add_unqualified ("zeros_row_vector", ReturnType URowVector, [UInt], SoA);
  add_unqualified ("zeros_vector", ReturnType UVector, [UInt], SoA)

(* variadics *)

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
  ; (DataOnly, UInt) (* int solver_backward *) ]

let variadic_ode_tol_arg_types =
  [ (UnsizedType.DataOnly, UnsizedType.UReal); (DataOnly, UReal)
  ; (DataOnly, UInt) ]

let variadic_ode_mandatory_arg_types =
  [ (UnsizedType.AutoDiffable, UnsizedType.UVector); (AutoDiffable, UReal)
  ; (AutoDiffable, UArray UReal) ]

let variadic_ode_mandatory_fun_args =
  [ (UnsizedType.AutoDiffable, UnsizedType.UReal)
  ; (UnsizedType.AutoDiffable, UnsizedType.UVector) ]

let variadic_ode_fun_return_type = UnsizedType.UVector
let variadic_ode_return_type = UnsizedType.UArray UnsizedType.UVector

let variadic_dae_tol_arg_types =
  [ (UnsizedType.DataOnly, UnsizedType.UReal); (DataOnly, UReal)
  ; (DataOnly, UInt) ]

let variadic_dae_mandatory_arg_types =
  [ (UnsizedType.AutoDiffable, UnsizedType.UVector); (* yy *)
    (AutoDiffable, UnsizedType.UVector); (* yp *) (DataOnly, UReal); (* t0 *)
    (DataOnly, UArray UReal) (* ts *) ]

let variadic_dae_mandatory_fun_args =
  [ (UnsizedType.AutoDiffable, UnsizedType.UReal)
  ; (UnsizedType.AutoDiffable, UnsizedType.UVector)
  ; (UnsizedType.AutoDiffable, UnsizedType.UVector) ]

let variadic_ode_adjoint_fn = "ode_adjoint_tol_ctl"

let variadic_ode_nonadjoint_fns =
  String.Set.of_list
    [ "ode_bdf_tol"; "ode_rk45_tol"; "ode_adams_tol"; "ode_bdf"; "ode_rk45"
    ; "ode_adams"; "ode_ckrk"; "ode_ckrk_tol" ]

let ode_tolerances_suffix = "_tol"
let variadic_dae_fun_return_type = UnsizedType.UVector
let variadic_dae_return_type = UnsizedType.UArray UnsizedType.UVector

let add_variadic_fn name ~return_type ?control_args ~required_fn_rt
    ?required_fn_args () =
  let control_args = Option.value control_args ~default:[] in
  let required_fn_args = Option.value required_fn_args ~default:[] in
  Hashtbl.add_exn stan_math_variadic_signatures ~key:name
    ~data:
      UnsizedType.{return_type; control_args; required_fn_rt; required_fn_args}

let () =
  (* DAEs *)
  add_variadic_fn "dae" ~return_type:variadic_dae_return_type
    ~control_args:variadic_dae_mandatory_arg_types
    ~required_fn_args:variadic_dae_mandatory_fun_args
    ~required_fn_rt:variadic_dae_fun_return_type ();
  add_variadic_fn "dae_tol" ~return_type:variadic_dae_return_type
    ~control_args:(variadic_dae_mandatory_arg_types @ variadic_dae_tol_arg_types)
    ~required_fn_args:variadic_dae_mandatory_fun_args
    ~required_fn_rt:variadic_dae_fun_return_type ();
  (* non-adjoint ODES - same for all *)
  let add_ode name =
    add_variadic_fn name ~return_type:variadic_ode_return_type
      ~control_args:
        (if String.is_suffix name ~suffix:ode_tolerances_suffix then
           variadic_ode_mandatory_arg_types @ variadic_ode_tol_arg_types
         else variadic_ode_mandatory_arg_types)
      ~required_fn_rt:variadic_ode_fun_return_type
      ~required_fn_args:variadic_ode_mandatory_fun_args () in
  Set.iter ~f:add_ode variadic_ode_nonadjoint_fns;
  (* Adjoint ODE function *)
  add_variadic_fn variadic_ode_adjoint_fn ~return_type:variadic_ode_return_type
    ~control_args:
      (variadic_ode_mandatory_arg_types @ variadic_ode_adjoint_ctl_tol_arg_types)
    ~required_fn_rt:variadic_ode_fun_return_type
    ~required_fn_args:variadic_ode_mandatory_fun_args ();
  (* Algebra solvers *)
  add_variadic_fn "solve_newton" ~return_type:UnsizedType.UVector
    ~control_args:[UnsizedType.(AutoDiffable, UVector)]
    ~required_fn_rt:UnsizedType.UVector
    ~required_fn_args:[UnsizedType.(AutoDiffable, UVector)]
    ();
  add_variadic_fn "solve_powell" ~return_type:UnsizedType.UVector
    ~control_args:[UnsizedType.(AutoDiffable, UVector)]
    ~required_fn_rt:UnsizedType.UVector
    ~required_fn_args:[UnsizedType.(AutoDiffable, UVector)]
    ();
  add_variadic_fn "solve_newton_tol" ~return_type:UnsizedType.UVector
    ~control_args:
      UnsizedType.
        [ (AutoDiffable, UVector); (DataOnly, UReal); (DataOnly, UReal)
        ; (DataOnly, UInt) ]
    ~required_fn_rt:UnsizedType.UVector
    ~required_fn_args:[UnsizedType.(AutoDiffable, UVector)]
    ();
  add_variadic_fn "solve_powell_tol" ~return_type:UnsizedType.UVector
    ~control_args:
      UnsizedType.
        [ (AutoDiffable, UVector); (DataOnly, UReal); (DataOnly, UReal)
        ; (DataOnly, UInt) ]
    ~required_fn_rt:UnsizedType.UVector
    ~required_fn_args:[UnsizedType.(AutoDiffable, UVector)]
    ()

(** Print a module definition to [file]
  that contains the signatures computed above *)
let generate_module () =
  let marshal e = Marshal.to_string e [] in
  (* Core's Hashtbl cannot be safely Marshal'd, so we
     round trip through an associative list *)
  let marshal_hashtbl (type value) (t : value String.Table.t) =
    marshal (Hashtbl.to_alist t) in
  let distributions_simplified =
    distributions
    |> List.map ~f:(fun (kind, name, _, _) ->
           (name, List.map ~f:(Fn.compose String.lowercase show_fkind) kind))
       (* combine any common keys *)
    |> String.Map.of_alist_reduce ~f:(fun v1 v2 ->
           v1 @ v2 |> Set.Poly.of_list |> Set.to_list)
    |> Map.to_alist in
  (* TODO: in OCaml 5.4+, use GC.ramp_up to avoid performance regressions.
     See https://github.com/ocaml/ocaml/issues/13300 *)
  Printf.printf
    {|
let unmarshal s = Marshal.from_string s 0
let unmarshal_hashtbl s : 'a Core.String.Table.t =
  unmarshal s |> Core.String.Table.of_alist_exn |};
  Printf.printf
    {|
let stan_math_signatures :
    Middle.UnsizedType.signature list Core.String.Table.t =
  unmarshal_hashtbl %S |}
    (marshal_hashtbl stan_math_signatures);
  Printf.printf
    {|
let stan_math_variadic_signatures :
    Middle.UnsizedType.variadic_signature Core.String.Table.t =
  unmarshal_hashtbl %S |}
    (marshal_hashtbl stan_math_variadic_signatures);
  Printf.printf
    {|
let distributions : (string * string list) list = unmarshal %S |}
    (marshal distributions_simplified);
  Printf.eprintf
    "Generated signatures for %d functions, %d distributions, and %d variadic \
     functions.\n"
    (Hashtbl.length stan_math_signatures)
    (List.length distributions_simplified)
    (Hashtbl.length stan_math_variadic_signatures)

let () = generate_module ()
