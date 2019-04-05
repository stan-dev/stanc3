(** The signatures of the Stan Math library, which are used for type checking *)

open Core_kernel
open Mir
open Errors
open Type_conversion
open Pretty_printing

(** The signatures hash table *)
let stan_math_signatures = String.Table.create ()

(* -- Querying stan_math_signatures -- *)
let stan_math_returntype name args =
  let namematches = Hashtbl.find_multi stan_math_signatures name in
  let filteredmatches =
    List.filter
      ~f:(fun x -> check_compatible_arguments_mod_conv name (snd x) args)
      namematches
  in
  if List.length filteredmatches = 0 then None
    (* Return the least return type in case there are multiple options (due to implicit UInt-UReal conversion), where UInt<UReal *)
  else
    Some
      (List.hd_exn
         (List.sort ~compare:compare_returntype
            (List.map ~f:fst filteredmatches)))

let is_stan_math_function_name name = Hashtbl.mem stan_math_signatures name

let pretty_print_all_math_lib_fn_sigs name =
  let namematches = Hashtbl.find_multi stan_math_signatures name in
  if List.length namematches = 0 then ""
  else
    "\n"
    ^ String.concat ~sep:"\n"
        (List.map
           ~f:(fun (x, y) -> pretty_print_unsizedtype (UFun (y, x)))
           namematches)

(* -- Some helper definitions to populate stan_math_signatures -- *)
let rec bare_array_type (t, i) =
  match i with 0 -> t | j -> UArray (bare_array_type (t, j - 1))

let bare_types = function
  | 0 -> UInt
  | 1 -> UReal
  | 2 -> UVector
  | 3 -> URowVector
  | 4 -> UMatrix
  | _ -> fatal_error ()

let bare_types_size = 5

let vector_types = function
  | 0 -> UReal
  | 1 -> UArray UReal
  | 2 -> UVector
  | 3 -> URowVector
  | _ -> fatal_error ()

let vector_types_size = 4

let int_vector_types = function
  | 0 -> UInt
  | 1 -> UArray UInt
  | _ -> fatal_error ()

let int_vector_types_size = 2
let primitive_types = function 0 -> UInt | 1 -> UReal | _ -> fatal_error ()
let primitive_types_size = 2

let all_vector_types = function
  | 0 -> UReal
  | 1 -> UArray UReal
  | 2 -> UVector
  | 3 -> URowVector
  | 4 -> UInt
  | 5 -> UArray UInt
  | _ -> fatal_error ()

let all_vector_types_size = 6

let eigen_vector_types = function
  | 0 -> UVector
  | 1 -> UArray UVector
  | 2 -> URowVector
  | 3 -> UArray URowVector
  | _ -> fatal_error ()

let eigen_vector_types_size = 4
let is_primitive = function UReal -> true | UInt -> true | _ -> false

let rng_return_type t lt =
  if List.for_all ~f:is_primitive lt then t else UArray t

let add_unqualified (name, rt, uqargts) =
  let _ =
    Hashtbl.add_multi stan_math_signatures ~key:name
      ~data:(rt, List.map ~f:(fun x -> (AutoDiffable, x)) uqargts)
  in
  ()

let add_qualified (name, rt, argts) =
  Hashtbl.add_multi stan_math_signatures ~key:name ~data:(rt, argts)

let add_nullary name = add_unqualified (name, ReturnType UReal, [])
let add_unary name = add_unqualified (name, ReturnType UReal, [UReal])

let add_unary_vectorized name =
  for j = 0 to 8 - 1 do
    add_unqualified
      ( name
      , ReturnType (bare_array_type (UReal, j))
      , [bare_array_type (UInt, j)] ) ;
    add_unqualified
      ( name
      , ReturnType (bare_array_type (UReal, j))
      , [bare_array_type (UReal, j)] ) ;
    add_unqualified
      ( name
      , ReturnType (bare_array_type (UVector, j))
      , [bare_array_type (UVector, j)] ) ;
    add_unqualified
      ( name
      , ReturnType (bare_array_type (URowVector, j))
      , [bare_array_type (URowVector, j)] ) ;
    add_unqualified
      ( name
      , ReturnType (bare_array_type (UMatrix, j))
      , [bare_array_type (UMatrix, j)] )
  done

let add_binary name = add_unqualified (name, ReturnType UReal, [UReal; UReal])

let add_ternary name =
  add_unqualified (name, ReturnType UReal, [UReal; UReal; UReal])

let for_all_vector_types s =
  for i = 0 to all_vector_types_size - 1 do
    s (all_vector_types i)
  done

let for_int_vector_types s =
  for i = 0 to int_vector_types_size - 1 do
    s (int_vector_types i)
  done

let for_vector_types s =
  for i = 0 to vector_types_size - 1 do
    s (vector_types i)
  done

(* -- Start populating stan_math_signaturess -- *)
let _ =
  add_unqualified ("abs", ReturnType UInt, [UInt]) ;
  add_unqualified ("abs", ReturnType UReal, [UReal]) ;
  add_unary_vectorized "acos" ;
  add_unary_vectorized "acosh" ;
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
  add_unary_vectorized "asin" ;
  add_unary_vectorized "asinh" ;
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
  add_unary_vectorized "atan" ;
  add_binary "atan2" ;
  add_unary_vectorized "atanh" ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "bernoulli_ccdf_log"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_cdf"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_cdf_log"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_log"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_lccdf"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_lcdf"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_lpmf"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] )
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("bernoulli_rng", ReturnType (rng_return_type UInt [t]), [t]) ) ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("bernoulli_logit_rng", ReturnType (rng_return_type UInt [t]), [t]) ) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "bernoulli_logit_log"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_logit_lpmf"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] )
    done
  done ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UReal; UVector] ) ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UVector; UVector] ) ;
  add_unqualified ("bessel_first_kind", ReturnType UReal, [UInt; UReal]) ;
  add_unqualified ("bessel_second_kind", ReturnType UReal, [UInt; UReal]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to int_vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_unqualified
            ( "beta_binomial_ccdf_log"
            , ReturnType UReal
            , [ int_vector_types i; int_vector_types j; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_cdf"
            , ReturnType UReal
            , [ int_vector_types i; int_vector_types j; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_cdf_log"
            , ReturnType UReal
            , [ int_vector_types i; int_vector_types j; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_log"
            , ReturnType UReal
            , [ int_vector_types i; int_vector_types j; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_lccdf"
            , ReturnType UReal
            , [ int_vector_types i; int_vector_types j; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_lcdf"
            , ReturnType UReal
            , [ int_vector_types i; int_vector_types j; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_lpmf"
            , ReturnType UReal
            , [ int_vector_types i; int_vector_types j; vector_types k
              ; vector_types l ] )
        done
      done
    done
  done ;
  for_int_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_unqualified
                ( "beta_binomial_rng"
                , ReturnType (rng_return_type UInt [t; u; v])
                , [t; u; v] ) ) ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "beta_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("beta_rng", ReturnType (rng_return_type UReal [t; u]), [t; u]) )
  ) ;
  for_vector_types (fun t ->
      for_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_unqualified
                ("beta_proportion_ccdf_log", ReturnType UReal, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_cdf_log", ReturnType UReal, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_log", ReturnType UReal, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_lccdf", ReturnType UReal, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_lcdf", ReturnType UReal, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_lpdf", ReturnType UReal, [t; u; v]) ) ) ) ;
  for_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "beta_proportion_rng"
            , ReturnType (rng_return_type UReal [t; u])
            , [t; u] ) ) ) ;
  add_unqualified ("binary_log_loss", ReturnType UReal, [UInt; UReal]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to int_vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "binomial_ccdf_log"
          , ReturnType UReal
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_cdf"
          , ReturnType UReal
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_cdf_log"
          , ReturnType UReal
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_log"
          , ReturnType UReal
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_lccdf"
          , ReturnType UReal
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_lcdf"
          , ReturnType UReal
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_lpmf"
          , ReturnType UReal
          , [int_vector_types i; int_vector_types j; vector_types k] )
      done
    done
  done ;
  for_int_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("binomial_rng", ReturnType (rng_return_type UInt [t; u]), [t; u])
      ) ) ;
  add_binary "binomial_coefficient_log" ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to int_vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "binomial_logit_log"
          , ReturnType UReal
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_logit_lpmf"
          , ReturnType UReal
          , [int_vector_types i; int_vector_types j; vector_types k] )
      done
    done
  done ;
  add_unqualified
    ("block", ReturnType UMatrix, [UMatrix; UInt; UInt; UInt; UInt]) ;
  for i = 0 to int_vector_types_size - 1 do
    add_unqualified
      ("categorical_log", ReturnType UReal, [int_vector_types i; UVector]) ;
    add_unqualified
      ("categorical_logit_log", ReturnType UReal, [int_vector_types i; UVector]) ;
    add_unqualified
      ("categorical_lpmf", ReturnType UReal, [int_vector_types i; UVector]) ;
    add_unqualified
      ( "categorical_logit_lpmf"
      , ReturnType UReal
      , [int_vector_types i; UVector] )
  done ;
  add_unqualified ("categorical_rng", ReturnType UInt, [UVector]) ;
  add_unqualified ("categorical_logit_rng", ReturnType UInt, [UVector]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "cauchy_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("cauchy_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
  add_unqualified ("append_col", ReturnType UMatrix, [UMatrix; UMatrix]) ;
  add_unqualified ("append_col", ReturnType UMatrix, [UVector; UMatrix]) ;
  add_unqualified ("append_col", ReturnType UMatrix, [UMatrix; UVector]) ;
  add_unqualified ("append_col", ReturnType UMatrix, [UVector; UVector]) ;
  add_unqualified
    ("append_col", ReturnType URowVector, [URowVector; URowVector]) ;
  add_unqualified ("append_col", ReturnType URowVector, [UReal; URowVector]) ;
  add_unqualified ("append_col", ReturnType URowVector, [URowVector; UReal]) ;
  add_unary_vectorized "cbrt" ;
  add_unary_vectorized "ceil" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "chi_square_ccdf_log"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("chi_square_cdf", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ( "chi_square_cdf_log"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("chi_square_log", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ("chi_square_lccdf", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ("chi_square_lcdf", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ("chi_square_lpdf", ReturnType UReal, [vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("chi_square_rng", ReturnType (rng_return_type UReal [t]), [t]) ) ;
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
  add_unary_vectorized "cos" ;
  add_unary_vectorized "cosh" ;
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
  add_unary_vectorized "digamma" ;
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
  add_unqualified ("dirichlet_log", ReturnType UReal, [UVector; UVector]) ;
  add_unqualified ("dirichlet_lpdf", ReturnType UReal, [UVector; UVector]) ;
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
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "double_exponential_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "double_exponential_rng"
            , ReturnType (rng_return_type UReal [t; u])
            , [t; u] ) ) ) ;
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
  add_unary_vectorized "erf" ;
  add_unary_vectorized "erfc" ;
  add_unary_vectorized "exp" ;
  add_unary_vectorized "exp2" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_unqualified
            ( "exp_mod_normal_ccdf_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_cdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_cdf_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_lccdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_lcdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_lpdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            )
        done
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_unqualified
                ( "exp_mod_normal_rng"
                , ReturnType (rng_return_type UReal [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_unary_vectorized "expm1" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "exponential_ccdf_log"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("exponential_cdf", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ( "exponential_cdf_log"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("exponential_log", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ( "exponential_lccdf"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("exponential_lcdf", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ("exponential_lpdf", ReturnType UReal, [vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("exponential_rng", ReturnType (rng_return_type UReal [t]), [t]) ) ;
  add_unary_vectorized "fabs" ;
  add_unqualified ("falling_factorial", ReturnType UReal, [UReal; UInt]) ;
  add_unqualified ("falling_factorial", ReturnType UInt, [UInt; UInt]) ;
  add_binary "fdim" ;
  add_unary_vectorized "floor" ;
  add_ternary "fma" ;
  add_binary "fmax" ;
  add_binary "fmin" ;
  add_binary "fmod" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "frechet_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("frechet_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "gamma_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  add_binary "gamma_p" ;
  add_binary "gamma_q" ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("gamma_rng", ReturnType (rng_return_type UReal [t; u]), [t; u]) )
  ) ;
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
    , [UMatrix; UMatrix; UMatrix; UVector; UMatrix; UVector; UMatrix] )
  (* ; add_nullary ("get_lp")   *) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "gumbel_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("gumbel_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
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
    ("hypergeometric_log", ReturnType UReal, [UInt; UInt; UInt; UInt]) ;
  add_unqualified
    ("hypergeometric_lpmf", ReturnType UReal, [UInt; UInt; UInt; UInt]) ;
  add_unqualified ("hypergeometric_rng", ReturnType UInt, [UInt; UInt; UInt]) ;
  add_binary "hypot" ;
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
      ; (DataOnly, UReal); (DataOnly, UArray UReal)
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
      ; (DataOnly, UReal); (DataOnly, UArray UReal)
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
      ; (DataOnly, UReal); (DataOnly, UArray UReal)
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
      ; (DataOnly, UReal); (DataOnly, UArray UReal)
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
      ; (DataOnly, UReal); (DataOnly, UArray UReal)
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
      ; (DataOnly, UReal); (DataOnly, UArray UReal)
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
      ; (DataOnly, UReal); (DataOnly, UArray UReal)
      ; (AutoDiffable, UArray UReal)
      ; (DataOnly, UArray UReal); (DataOnly, UArray UInt); (DataOnly, UReal)
      ; (DataOnly, UReal); (DataOnly, UReal) ] ) ;
  add_unary_vectorized "inv" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "inv_chi_square_ccdf_log"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_cdf"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_cdf_log"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_log"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_lccdf"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_lcdf"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_lpdf"
        , ReturnType UReal
        , [vector_types i; vector_types j] )
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("inv_chi_square_rng", ReturnType (rng_return_type UReal [t]), [t]) ) ;
  add_unary_vectorized "inv_cloglog" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "inv_gamma_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("inv_gamma_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
  add_unary_vectorized "inv_logit" ;
  add_unary_vectorized "inv_Phi" ;
  add_unary_vectorized "inv_sqrt" ;
  add_unary_vectorized "inv_square" ;
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
  add_unary_vectorized "lgamma" ;
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
  add_unary_vectorized "log" ;
  add_nullary "log10" ;
  add_unary_vectorized "log10" ;
  add_unary_vectorized "log1m" ;
  add_unary_vectorized "log1m_exp" ;
  add_unary_vectorized "log1m_inv_logit" ;
  add_unary_vectorized "log1p" ;
  add_unary_vectorized "log1p_exp" ;
  add_nullary "log2" ;
  add_unary_vectorized "log2" ;
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
  add_unary_vectorized "log_inv_logit" ;
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
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "logistic_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("logistic_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
  add_unary_vectorized "logit" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "lognormal_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("lognormal_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
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
  add_unqualified
    ("multi_gp_log", ReturnType UReal, [UMatrix; UMatrix; UVector]) ;
  add_unqualified
    ("multi_gp_lpdf", ReturnType UReal, [UMatrix; UMatrix; UVector]) ;
  add_unqualified
    ("multi_gp_cholesky_log", ReturnType UReal, [UMatrix; UMatrix; UVector]) ;
  add_unqualified
    ("multi_gp_cholesky_lpdf", ReturnType UReal, [UMatrix; UMatrix; UVector]) ;
  for k = 0 to eigen_vector_types_size - 1 do
    for l = 0 to eigen_vector_types_size - 1 do
      add_unqualified
        ( "multi_normal_cholesky_log"
        , ReturnType UReal
        , [eigen_vector_types k; eigen_vector_types l; UMatrix] ) ;
      add_unqualified
        ( "multi_normal_cholesky_lpdf"
        , ReturnType UReal
        , [eigen_vector_types k; eigen_vector_types l; UMatrix] ) ;
      add_unqualified
        ( "multi_normal_log"
        , ReturnType UReal
        , [eigen_vector_types k; eigen_vector_types l; UMatrix] ) ;
      add_unqualified
        ( "multi_normal_lpdf"
        , ReturnType UReal
        , [eigen_vector_types k; eigen_vector_types l; UMatrix] ) ;
      add_unqualified
        ( "multi_normal_prec_log"
        , ReturnType UReal
        , [eigen_vector_types k; eigen_vector_types l; UMatrix] ) ;
      add_unqualified
        ( "multi_normal_prec_lpdf"
        , ReturnType UReal
        , [eigen_vector_types k; eigen_vector_types l; UMatrix] ) ;
      add_unqualified
        ( "multi_student_t_log"
        , ReturnType UReal
        , [eigen_vector_types k; UReal; eigen_vector_types l; UMatrix] ) ;
      add_unqualified
        ( "multi_student_t_lpdf"
        , ReturnType UReal
        , [eigen_vector_types k; UReal; eigen_vector_types l; UMatrix] )
    done
  done ;
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
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "neg_binomial_ccdf_log"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_cdf"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_cdf_log"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_log"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_lccdf"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_lcdf"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_lpmf"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_ccdf_log"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_cdf"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_cdf_log"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_log"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_lccdf"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_lcdf"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_lpmf"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_log_log"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_log_lpmf"
          , ReturnType UReal
          , [int_vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "neg_binomial_rng"
            , ReturnType (rng_return_type UInt [t; u])
            , [t; u] ) ) ) ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "neg_binomial_2_rng"
            , ReturnType (rng_return_type UInt [t; u])
            , [t; u] ) ) ) ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "neg_binomial_2_log_rng"
            , ReturnType (rng_return_type UInt [t; u])
            , [t; u] ) ) ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UReal; UVector; UReal] ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UVector; UVector; UReal] ) ;
  add_nullary "negative_infinity" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "normal_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("normal_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; UMatrix; UReal; UVector; UReal] ) ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType UReal
    , [UVector; UMatrix; UVector; UVector; UReal] ) ;
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
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "pareto_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("pareto_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_unqualified
            ( "pareto_type_2_ccdf_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_cdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_cdf_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_lccdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_lcdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_lpdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            )
        done
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_unqualified
                ( "pareto_type_2_rng"
                , ReturnType (rng_return_type UReal [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_unary_vectorized "Phi" ;
  add_unary_vectorized "Phi_approx" ;
  add_nullary "pi" ;
  add_unqualified ("plus", ReturnType UInt, [UInt]) ;
  add_unqualified ("plus", ReturnType UReal, [UReal]) ;
  add_unqualified ("plus", ReturnType UVector, [UVector]) ;
  add_unqualified ("plus", ReturnType URowVector, [URowVector]) ;
  add_unqualified ("plus", ReturnType UMatrix, [UMatrix]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "poisson_ccdf_log"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ("poisson_cdf", ReturnType UReal, [int_vector_types i; vector_types j]) ;
      add_unqualified
        ( "poisson_cdf_log"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ("poisson_log", ReturnType UReal, [int_vector_types i; vector_types j]) ;
      add_unqualified
        ( "poisson_lccdf"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ("poisson_lcdf", ReturnType UReal, [int_vector_types i; vector_types j]) ;
      add_unqualified
        ("poisson_lpmf", ReturnType UReal, [int_vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("poisson_rng", ReturnType (rng_return_type UInt [t]), [t]) ) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "poisson_log_log"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "poisson_log_lpmf"
        , ReturnType UReal
        , [int_vector_types i; vector_types j] )
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("poisson_log_rng", ReturnType (rng_return_type UInt [t]), [t]) ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UReal; UVector] ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType UReal
    , [bare_array_type (UInt, 1); UMatrix; UVector; UVector] ) ;
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
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "rayleigh_ccdf_log"
        , ReturnType UReal
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("rayleigh_cdf", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_cdf_log", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_log", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_lccdf", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_lcdf", ReturnType UReal, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_lpdf", ReturnType UReal, [vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("rayleigh_rng", ReturnType (rng_return_type UReal [t]), [t]) ) ;
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
  add_unqualified ("rising_factorial", ReturnType UReal, [UReal; UInt]) ;
  add_unqualified ("rising_factorial", ReturnType UInt, [UInt; UInt]) ;
  add_unary_vectorized "round" ;
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
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "scaled_inv_chi_square_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "scaled_inv_chi_square_rng"
            , ReturnType (rng_return_type UReal [t; u])
            , [t; u] ) ) ) ;
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
  add_unary_vectorized "sin" ;
  add_unqualified ("singular_values", ReturnType UVector, [UMatrix]) ;
  add_unary_vectorized "sinh" ;
  for i = 1 to 8 - 1 do
    add_unqualified ("size", ReturnType UInt, [bare_array_type (UInt, i)]) ;
    add_unqualified ("size", ReturnType UInt, [bare_array_type (UReal, i)]) ;
    add_unqualified ("size", ReturnType UInt, [bare_array_type (UVector, i)]) ;
    add_unqualified ("size", ReturnType UInt, [bare_array_type (URowVector, i)]) ;
    add_unqualified ("size", ReturnType UInt, [bare_array_type (UMatrix, i)])
  done ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_unqualified
            ( "skew_normal_ccdf_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_cdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_cdf_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_lccdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_lcdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_lpdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            )
        done
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_unqualified
                ( "skew_normal_rng"
                , ReturnType (rng_return_type UReal [t; u; v])
                , [t; u; v] ) ) ) ) ;
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
  add_unary_vectorized "sqrt" ;
  add_nullary "sqrt2" ;
  add_unary_vectorized "square" ;
  for i = 0 to vector_types_size - 1 do
    add_unqualified ("std_normal_log", ReturnType UReal, [vector_types i]) ;
    add_unqualified ("std_normal_lpdf", ReturnType UReal, [vector_types i])
  done ;
  add_unary "step" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_unqualified
            ( "student_t_ccdf_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_cdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_cdf_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_log"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_lccdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_lcdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_lpdf"
            , ReturnType UReal
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            )
        done
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_unqualified
                ( "student_t_rng"
                , ReturnType (rng_return_type UReal [t; u; v])
                , [t; u; v] ) ) ) ) ;
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
  add_unary_vectorized "tan" ;
  add_unary_vectorized "tanh" (* ; add_nullary ("target") *) ;
  add_unqualified ("tcrossprod", ReturnType UMatrix, [UMatrix]) ;
  add_unary_vectorized "tgamma" ;
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
  add_unary_vectorized "trunc" ;
  add_unary_vectorized "trigamma" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "uniform_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("uniform_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
  add_unqualified ("variance", ReturnType UReal, [bare_array_type (UReal, 1)]) ;
  add_unqualified ("variance", ReturnType UReal, [UVector]) ;
  add_unqualified ("variance", ReturnType UReal, [URowVector]) ;
  add_unqualified ("variance", ReturnType UReal, [UMatrix]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "von_mises_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "von_mises_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("von_mises_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "weibull_ccdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_cdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_cdf_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_log"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_lccdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_lcdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_lpdf"
          , ReturnType UReal
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("weibull_rng", ReturnType (rng_return_type UReal [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          for m = 0 to vector_types_size - 1 do
            add_unqualified
              ( "wiener_log"
              , ReturnType UReal
              , [ vector_types i; vector_types j; vector_types k; vector_types l
                ; vector_types m ] ) ;
            add_unqualified
              ( "wiener_lpdf"
              , ReturnType UReal
              , [ vector_types i; vector_types j; vector_types k; vector_types l
                ; vector_types m ] )
          done
        done
      done
    done
  done ;
  add_unqualified ("wishart_log", ReturnType UReal, [UMatrix; UReal; UMatrix]) ;
  add_unqualified ("wishart_lpdf", ReturnType UReal, [UMatrix; UReal; UMatrix]) ;
  add_unqualified ("wishart_rng", ReturnType UMatrix, [UReal; UMatrix])
