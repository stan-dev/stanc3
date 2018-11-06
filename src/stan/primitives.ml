(* Here, we define type checking ; for the Stan Math library *)

(* This is ugly. An ideal treatment of function overloading works by carrying around*
   a LAZY set of types ; for each expression. However, that's awkward in OCaml.
   Perhaps an argument ; for Haskell after all?
   OCaml-1 does have lazy lists. Perhaps those could be used ; for this purpose?
   Or implement our own lazy sets?
   
*)

(* TODO: do appropriate checking ; for higher order functions here *)

(* TODO: first load whole math library into try_get_primitive_return_type --
we are using a predicate here because the functions are overloaded so heavily  *)

open Syntax

(* A semantic error reported by the toplevel *)
let semantic_error ?loc msg =
  Zoo.error ~kind:"Semantic error" ?loc (Scanf.format_from_string msg "")

(* We allow implicit conversion from int to real, except for assignment operators *)
let check_of_same_type_mod_conv name t1 t2 =
  if Core_kernel.String.is_prefix name ~prefix:"assign_" then t1 = t2
  else t1 = t2 || (t1 = Real && t1 = Int)

let primitive_signatures = Hashtbl.create 3000

let bare_types = function
  | 0 -> Int
  | 1 -> Real
  | 2 -> Vector
  | 3 -> RowVector
  | 4 -> Matrix
  | _ ->
      semantic_error
        "This should never happen. Please report a bug. Error code 9."

let bare_types_size = 5

let vector_types = function
  | 0 -> Real
  | 1 -> Array Real
  | 2 -> Vector
  | 3 -> RowVector
  | _ ->
      semantic_error
        "This should never happen. Please report a bug. Error code 10."

let vector_types_size = 4

let int_vector_types = function
  | 0 -> Int
  | 1 -> Array Int
  | _ ->
      semantic_error
        "This should never happen. Please report a bug. Error code 11."

let int_vector_types_size = 2

let primitive_types = function
  | 0 -> Int
  | 1 -> Real
  | _ ->
      semantic_error
        "This should never happen. Please report a bug. Error code 12."

let primitive_types_size = 2

let all_vector_types = function
  | 0 -> Real
  | 1 -> Array Real
  | 2 -> Vector
  | 3 -> RowVector
  | 4 -> Int
  | 5 -> Array Int
  | _ ->
      semantic_error
        "This should never happen. Please report a bug. Error code 13."

let all_vector_types_size = 6

let eigen_vector_types = function
  | 0 -> Vector
  | 1 -> Array Vector
  | 2 -> RowVector
  | 3 -> Array RowVector
  | _ ->
      semantic_error
        "This should never happen. Please report a bug. Error code 14."

let eigen_vector_types_size = 4

let is_primitive = function Real -> true | Int -> true | _ -> false

let rng_return_type t lt = if List.for_all is_primitive lt then t else Array t

let add_plain (name, rt, argts) =
  Hashtbl.add primitive_signatures name (rt, argts)

let add_nullary name = add_plain (name, ReturnType Real, [])

let add_unary name = add_plain (name, ReturnType Real, [Real])

let add_unary_vectorized name =
  add_plain (name, ReturnType Real, [Int]) ;
  add_plain (name, ReturnType Real, [Real]) ;
  add_plain (name, ReturnType Vector, [Vector]) ;
  add_plain (name, ReturnType RowVector, [RowVector]) ;
  add_plain (name, ReturnType Matrix, [Matrix]) ;
  add_plain (name, ReturnType (Array Real), [Array Int]) ;
  add_plain (name, ReturnType (Array Real), [Array Real]) ;
  add_plain (name, ReturnType (Array Vector), [Array Vector]) ;
  add_plain (name, ReturnType (Array RowVector), [Array RowVector]) ;
  add_plain (name, ReturnType (Array Matrix), [Array Matrix])

let add_binary name = add_plain (name, ReturnType Real, [Real; Real])

let add_ternary name = add_plain (name, ReturnType Real, [Real; Real; Real])

let add_quaternary name =
  add_plain (name, ReturnType Real, [Real; Real; Real; Real])

let rec bare_array_type (t, i) =
  match i with 0 -> t | j -> Array (bare_array_type (t, j - 1))

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

let _ =
  add_plain ("abs", ReturnType Int, [Int]) ;
  add_plain ("abs", ReturnType Real, [Real]) ;
  add_unary_vectorized "acos" ;
  add_unary_vectorized "acosh" ;
  for i = 0 to bare_types_size - 1 do
    add_plain ("add", ReturnType (bare_types i), [bare_types i; bare_types i])
  done ;
  add_plain ("add", ReturnType Vector, [Vector; Real]) ;
  add_plain ("add", ReturnType RowVector, [RowVector; Real]) ;
  add_plain ("add", ReturnType Matrix, [Matrix; Real]) ;
  add_plain ("add", ReturnType Vector, [Real; Vector]) ;
  add_plain ("add", ReturnType RowVector, [Real; RowVector]) ;
  add_plain ("add", ReturnType Matrix, [Real; Matrix]) ;
  for i = 0 to bare_types_size - 1 do
    add_plain ("add", ReturnType (bare_types i), [bare_types i])
  done ;
  for i = 1 to 8 - 1 do
    add_plain
      ( "append_array"
      , ReturnType (bare_array_type (Int, i))
      , [bare_array_type (Int, i); bare_array_type (Int, i)] ) ;
    add_plain
      ( "append_array"
      , ReturnType (bare_array_type (Real, i))
      , [bare_array_type (Real, i); bare_array_type (Real, i)] ) ;
    add_plain
      ( "append_array"
      , ReturnType (bare_array_type (Vector, i))
      , [bare_array_type (Vector, i); bare_array_type (Vector, i)] ) ;
    add_plain
      ( "append_array"
      , ReturnType (bare_array_type (RowVector, i))
      , [bare_array_type (RowVector, i); bare_array_type (RowVector, i)] ) ;
    add_plain
      ( "append_array"
      , ReturnType (bare_array_type (Matrix, i))
      , [bare_array_type (Matrix, i); bare_array_type (Matrix, i)] )
  done ;
  add_unary_vectorized "asin" ;
  add_unary_vectorized "asinh" ;
  add_plain ("assign_multiply", Void, [Int; Int]) ;
  add_plain ("assign_multiply", Void, [Matrix; Matrix]) ;
  add_plain ("assign_multiply", Void, [Matrix; Real]) ;
  add_plain ("assign_multiply", Void, [Real; Real]) ;
  add_plain ("assign_multiply", Void, [RowVector; Real]) ;
  add_plain ("assign_multiply", Void, [RowVector; Matrix]) ;
  add_plain ("assign_multiply", Void, [Vector; Real]) ;
  add_plain ("assign_add", Void, [Int; Int]) ;
  add_plain ("assign_add", Void, [Matrix; Matrix]) ;
  add_plain ("assign_add", Void, [Matrix; Real]) ;
  add_plain ("assign_add", Void, [Real; Real]) ;
  add_plain ("assign_add", Void, [RowVector; Real]) ;
  add_plain ("assign_add", Void, [RowVector; RowVector]) ;
  add_plain ("assign_add", Void, [Vector; Real]) ;
  add_plain ("assign_add", Void, [Vector; Vector]) ;
  add_plain ("assign_subtract", Void, [Int; Int]) ;
  add_plain ("assign_subtract", Void, [Matrix; Matrix]) ;
  add_plain ("assign_subtract", Void, [Matrix; Real]) ;
  add_plain ("assign_subtract", Void, [Real; Real]) ;
  add_plain ("assign_subtract", Void, [RowVector; Real]) ;
  add_plain ("assign_subtract", Void, [RowVector; RowVector]) ;
  add_plain ("assign_subtract", Void, [Vector; Real]) ;
  add_plain ("assign_subtract", Void, [Vector; Vector]) ;
  add_plain ("assign_elt_times", Void, [Matrix; Matrix]) ;
  add_plain ("assign_elt_times", Void, [RowVector; RowVector]) ;
  add_plain ("assign_elt_times", Void, [Vector; Vector]) ;
  add_plain ("assign_elt_divide", Void, [Matrix; Matrix]) ;
  add_plain ("assign_elt_divide", Void, [Matrix; Real]) ;
  add_plain ("assign_elt_divide", Void, [RowVector; Real]) ;
  add_plain ("assign_elt_divide", Void, [RowVector; RowVector]) ;
  add_plain ("assign_elt_divide", Void, [Vector; Real]) ;
  add_plain ("assign_elt_divide", Void, [Vector; Vector]) ;
  add_plain ("assign_divide", Void, [Int; Int]) ;
  add_plain ("assign_divide", Void, [Matrix; Real]) ;
  add_plain ("assign_divide", Void, [Real; Real]) ;
  add_plain ("assign_divide", Void, [RowVector; Real]) ;
  add_plain ("assign_divide", Void, [Vector; Real]) ;
  add_unary_vectorized "atan" ;
  add_binary "atan2" ;
  add_unary_vectorized "atanh" ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_plain
        ( "bernoulli_ccdf_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_plain
        ("bernoulli_cdf", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_plain
        ( "bernoulli_cdf_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_plain
        ("bernoulli_log", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_plain
        ( "bernoulli_lccdf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_plain
        ( "bernoulli_lcdf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_plain
        ( "bernoulli_lpmf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] )
    done
  done ;
  for_all_vector_types (fun t ->
      add_plain ("bernoulli_rng", ReturnType (rng_return_type Int [t]), [t]) ) ;
  for_all_vector_types (fun t ->
      add_plain
        ("bernoulli_logit_rng", ReturnType (rng_return_type Int [t]), [t]) ) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_plain
        ( "bernoulli_logit_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_plain
        ( "bernoulli_logit_lpmf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] )
    done
  done ;
  add_plain
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Real; Vector] ) ;
  add_plain
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Vector; Vector] ) ;
  add_plain ("bessel_first_kind", ReturnType Real, [Int; Real]) ;
  add_plain ("bessel_second_kind", ReturnType Real, [Int; Real]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to int_vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_plain
            ( "beta_binomial_ccdf_log"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_plain
            ( "beta_binomial_cdf"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_plain
            ( "beta_binomial_cdf_log"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_plain
            ( "beta_binomial_log"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_plain
            ( "beta_binomial_lccdf"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_plain
            ( "beta_binomial_lcdf"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_plain
            ( "beta_binomial_lpmf"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] )
        done
      done
    done
  done ;
  for_int_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_plain
                ( "beta_binomial_rng"
                , ReturnType (rng_return_type Int [t; u; v])
                , [t; u; v] ) ) ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "beta_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "beta_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "beta_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "beta_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "beta_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "beta_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "beta_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("beta_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) ) ) ;
  for_vector_types (fun t ->
      for_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_plain ("beta_proportion_ccdf_log", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_cdf_log", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_log", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_lccdf", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_lcdf", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_lpdf", ReturnType Real, [t; u; v]) )
      ) ) ;
  (* TODO: from here!! *)
  for_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ( "beta_proportion_rng"
            , ReturnType (rng_return_type Real [t; u])
            , [t; u] ) ) ) ;
  add_plain ("binary_log_loss", ReturnType Real, [Int; Real]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to int_vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "binomial_ccdf_log"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_plain
          ( "binomial_cdf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_plain
          ( "binomial_cdf_log"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_plain
          ( "binomial_log"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_plain
          ( "binomial_lccdf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_plain
          ( "binomial_lcdf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_plain
          ( "binomial_lpmf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] )
      done
    done
  done ;
  for_int_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("binomial_rng", ReturnType (rng_return_type Int [t; u]), [t; u])
      ) ) ;
  add_binary "binomial_coefficient_log" ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to int_vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "binomial_logit_log"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_plain
          ( "binomial_logit_lpmf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] )
      done
    done
  done ;
  add_plain ("block", ReturnType Matrix, [Matrix; Int; Int; Int; Int]) ;
  for i = 0 to int_vector_types_size - 1 do
    add_plain ("categorical_log", ReturnType Real, [int_vector_types i; Vector]) ;
    add_plain
      ("categorical_logit_log", ReturnType Real, [int_vector_types i; Vector]) ;
    add_plain
      ("categorical_lpmf", ReturnType Real, [int_vector_types i; Vector]) ;
    add_plain
      ("categorical_logit_lpmf", ReturnType Real, [int_vector_types i; Vector])
  done ;
  add_plain ("categorical_rng", ReturnType Int, [Vector]) ;
  add_plain ("categorical_logit_rng", ReturnType Int, [Vector]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "cauchy_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "cauchy_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "cauchy_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "cauchy_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "cauchy_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "cauchy_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "cauchy_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("cauchy_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  add_plain ("append_col", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("append_col", ReturnType Matrix, [Vector; Matrix]) ;
  add_plain ("append_col", ReturnType Matrix, [Matrix; Vector]) ;
  add_plain ("append_col", ReturnType Matrix, [Vector; Vector]) ;
  add_plain ("append_col", ReturnType RowVector, [RowVector; RowVector]) ;
  add_plain ("append_col", ReturnType RowVector, [Real; RowVector]) ;
  add_plain ("append_col", ReturnType RowVector, [RowVector; Real]) ;
  add_unary_vectorized "cbrt" ;
  add_unary_vectorized "ceil" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_plain
        ( "chi_square_ccdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ("chi_square_cdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ( "chi_square_cdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ("chi_square_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("chi_square_lccdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("chi_square_lcdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("chi_square_lpdf", ReturnType Real, [vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_plain ("chi_square_rng", ReturnType (rng_return_type Real [t]), [t])
  ) ;
  add_plain ("cholesky_decompose", ReturnType Matrix, [Matrix]) ;
  add_plain ("choose", ReturnType Int, [Int; Int]) ;
  add_plain ("col", ReturnType Vector, [Matrix; Int]) ;
  add_plain ("cols", ReturnType Int, [Vector]) ;
  add_plain ("cols", ReturnType Int, [RowVector]) ;
  add_plain ("cols", ReturnType Int, [Matrix]) ;
  add_plain ("columns_dot_product", ReturnType RowVector, [Vector; Vector]) ;
  add_plain
    ("columns_dot_product", ReturnType RowVector, [RowVector; RowVector]) ;
  add_plain ("columns_dot_product", ReturnType RowVector, [Matrix; Matrix]) ;
  add_plain ("columns_dot_self", ReturnType RowVector, [Vector]) ;
  add_plain ("columns_dot_self", ReturnType RowVector, [RowVector]) ;
  add_plain ("columns_dot_self", ReturnType RowVector, [Matrix]) ;
  add_unary_vectorized "cos" ;
  add_unary_vectorized "cosh" ;
  add_plain
    ("cov_exp_quad", ReturnType Matrix, [bare_array_type (Real, 1); Real; Real]) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (Vector, 1); Real; Real] ) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (RowVector, 1); Real; Real] ) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (Real, 1); bare_array_type (Real, 1); Real; Real] ) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (Vector, 1); bare_array_type (Vector, 1); Real; Real] ) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [ bare_array_type (RowVector, 1)
      ; bare_array_type (RowVector, 1)
      ; Real
      ; Real ] ) ;
  add_plain ("crossprod", ReturnType Matrix, [Matrix]) ;
  add_plain
    ( "csr_matrix_times_vector"
    , ReturnType Vector
    , [ Int
      ; Int
      ; Vector
      ; bare_array_type (Int, 1)
      ; bare_array_type (Int, 1)
      ; Vector ] ) ;
  add_plain
    ( "csr_to_dense_matrix"
    , ReturnType Matrix
    , [Int; Int; Vector; bare_array_type (Int, 1); bare_array_type (Int, 1)] ) ;
  add_plain ("csr_extract_w", ReturnType Vector, [Matrix]) ;
  add_plain ("csr_extract_v", ReturnType (bare_array_type (Int, 1)), [Matrix]) ;
  add_plain ("csr_extract_u", ReturnType (bare_array_type (Int, 1)), [Matrix]) ;
  add_plain
    ( "cumulative_sum"
    , ReturnType (bare_array_type (Real, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_plain ("cumulative_sum", ReturnType Vector, [Vector]) ;
  add_plain ("cumulative_sum", ReturnType RowVector, [RowVector]) ;
  add_plain ("determinant", ReturnType Real, [Matrix]) ;
  add_plain ("diag_matrix", ReturnType Matrix, [Vector]) ;
  add_plain ("diag_post_multiply", ReturnType Matrix, [Matrix; Vector]) ;
  add_plain ("diag_post_multiply", ReturnType Matrix, [Matrix; RowVector]) ;
  add_plain ("diag_pre_multiply", ReturnType Matrix, [Vector; Matrix]) ;
  add_plain ("diag_pre_multiply", ReturnType Matrix, [RowVector; Matrix]) ;
  add_plain ("diagonal", ReturnType Vector, [Matrix]) ;
  add_unary_vectorized "digamma" ;
  add_plain ("dims", ReturnType (bare_array_type (Int, 1)), [Int]) ;
  add_plain ("dims", ReturnType (bare_array_type (Int, 1)), [Real]) ;
  add_plain ("dims", ReturnType (bare_array_type (Int, 1)), [Vector]) ;
  add_plain ("dims", ReturnType (bare_array_type (Int, 1)), [RowVector]) ;
  add_plain ("dims", ReturnType (bare_array_type (Int, 1)), [Matrix]) ;
  for i = 0 to 8 - 1 do
    add_plain
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Int, i + 1)] ) ;
    add_plain
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Real, i + 1)] ) ;
    add_plain
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Vector, i + 1)] ) ;
    add_plain
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (RowVector, i + 1)] ) ;
    add_plain
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Matrix, i + 1)] )
  done ;
  add_plain ("dirichlet_log", ReturnType Real, [Vector; Vector]) ;
  add_plain ("dirichlet_lpdf", ReturnType Real, [Vector; Vector]) ;
  add_plain ("dirichlet_rng", ReturnType Vector, [Vector]) ;
  add_plain ("distance", ReturnType Real, [Vector; Vector]) ;
  add_plain ("distance", ReturnType Real, [RowVector; RowVector]) ;
  add_plain ("distance", ReturnType Real, [Vector; RowVector]) ;
  add_plain ("distance", ReturnType Real, [RowVector; Vector]) ;
  add_plain ("divide", ReturnType Int, [Int; Int]) ;
  add_plain ("divide", ReturnType Real, [Real; Real]) ;
  add_plain ("divide", ReturnType Vector, [Vector; Real]) ;
  add_plain ("divide", ReturnType RowVector, [RowVector; Real]) ;
  add_plain ("divide", ReturnType Matrix, [Matrix; Real]) ;
  add_plain ("dot_product", ReturnType Real, [Vector; Vector]) ;
  add_plain ("dot_product", ReturnType Real, [RowVector; RowVector]) ;
  add_plain ("dot_product", ReturnType Real, [Vector; RowVector]) ;
  add_plain ("dot_product", ReturnType Real, [RowVector; Vector]) ;
  add_plain
    ( "dot_product"
    , ReturnType Real
    , [bare_array_type (Real, 1); bare_array_type (Real, 1)] ) ;
  add_plain ("dot_self", ReturnType Real, [Vector]) ;
  add_plain ("dot_self", ReturnType Real, [RowVector]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "double_exponential_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "double_exponential_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "double_exponential_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "double_exponential_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "double_exponential_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "double_exponential_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "double_exponential_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ( "double_exponential_rng"
            , ReturnType (rng_return_type Real [t; u])
            , [t; u] ) ) ) ;
  add_nullary "e" ;
  add_plain ("eigenvalues_sym", ReturnType Vector, [Matrix]) ;
  add_plain ("eigenvectors_sym", ReturnType Matrix, [Matrix]) ;
  add_plain ("qr_Q", ReturnType Matrix, [Matrix]) ;
  add_plain ("qr_R", ReturnType Matrix, [Matrix]) ;
  add_plain ("qr_thin_Q", ReturnType Matrix, [Matrix]) ;
  add_plain ("qr_thin_R", ReturnType Matrix, [Matrix]) ;
  add_plain ("elt_divide", ReturnType Vector, [Vector; Vector]) ;
  add_plain ("elt_divide", ReturnType RowVector, [RowVector; RowVector]) ;
  add_plain ("elt_divide", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("elt_divide", ReturnType Vector, [Vector; Real]) ;
  add_plain ("elt_divide", ReturnType RowVector, [RowVector; Real]) ;
  add_plain ("elt_divide", ReturnType Matrix, [Matrix; Real]) ;
  add_plain ("elt_divide", ReturnType Vector, [Real; Vector]) ;
  add_plain ("elt_divide", ReturnType RowVector, [Real; RowVector]) ;
  add_plain ("elt_divide", ReturnType Matrix, [Real; Matrix]) ;
  add_plain ("elt_multiply", ReturnType Vector, [Vector; Vector]) ;
  add_plain ("elt_multiply", ReturnType RowVector, [RowVector; RowVector]) ;
  add_plain ("elt_multiply", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unary_vectorized "erf" ;
  add_unary_vectorized "erfc" ;
  add_unary_vectorized "exp" ;
  add_unary_vectorized "exp2" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_plain
            ( "exp_mod_normal_ccdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "exp_mod_normal_cdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "exp_mod_normal_cdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "exp_mod_normal_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "exp_mod_normal_lccdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "exp_mod_normal_lcdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "exp_mod_normal_lpdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            )
        done
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_plain
                ( "exp_mod_normal_rng"
                , ReturnType (rng_return_type Real [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_unary_vectorized "expm1" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_plain
        ( "exponential_ccdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ("exponential_cdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ( "exponential_cdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ("exponential_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("exponential_lccdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("exponential_lcdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("exponential_lpdf", ReturnType Real, [vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_plain ("exponential_rng", ReturnType (rng_return_type Real [t]), [t])
  ) ;
  add_unary_vectorized "fabs" ;
  add_plain ("falling_factorial", ReturnType Real, [Real; Int]) ;
  add_plain ("falling_factorial", ReturnType Int, [Int; Int]) ;
  add_binary "fdim" ;
  add_unary_vectorized "floor" ;
  add_ternary "fma" ;
  add_binary "fmax" ;
  add_binary "fmin" ;
  add_binary "fmod" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "frechet_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "frechet_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "frechet_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "frechet_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "frechet_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "frechet_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "frechet_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("frechet_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "gamma_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gamma_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gamma_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gamma_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gamma_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gamma_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gamma_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  add_binary "gamma_p" ;
  add_binary "gamma_q" ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("gamma_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  add_plain
    ( "gaussian_dlm_obs_log"
    , ReturnType Real
    , [Matrix; Matrix; Matrix; Matrix; Matrix; Vector; Matrix] ) ;
  add_plain
    ( "gaussian_dlm_obs_log"
    , ReturnType Real
    , [Matrix; Matrix; Matrix; Vector; Matrix; Vector; Matrix] ) ;
  add_plain
    ( "gaussian_dlm_obs_lpdf"
    , ReturnType Real
    , [Matrix; Matrix; Matrix; Matrix; Matrix; Vector; Matrix] ) ;
  add_plain
    ( "gaussian_dlm_obs_lpdf"
    , ReturnType Real
    , [Matrix; Matrix; Matrix; Vector; Matrix; Vector; Matrix] )
  (* ; add_nullary ("get_lp")   *) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "gumbel_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gumbel_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gumbel_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gumbel_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gumbel_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gumbel_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "gumbel_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("gumbel_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  add_plain ("head", ReturnType RowVector, [RowVector; Int]) ;
  add_plain ("head", ReturnType Vector, [Vector; Int]) ;
  for i = 0 to bare_types_size - 1 do
    add_plain
      ( "head"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_array_type (bare_types i, 1); Int] ) ;
    add_plain
      ( "head"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_array_type (bare_types i, 2); Int] ) ;
    add_plain
      ( "head"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_array_type (bare_types i, 3); Int] )
  done ;
  add_plain ("hypergeometric_log", ReturnType Real, [Int; Int; Int; Int]) ;
  add_plain ("hypergeometric_lpmf", ReturnType Real, [Int; Int; Int; Int]) ;
  add_plain ("hypergeometric_rng", ReturnType Int, [Int; Int; Int]) ;
  add_binary "hypot" ;
  add_plain ("if_else", ReturnType Real, [Int; Real; Real]) ;
  add_plain ("inc_beta", ReturnType Real, [Real; Real; Real]) ;
  add_plain ("int_step", ReturnType Int, [Real]) ;
  add_plain ("int_step", ReturnType Int, [Int]) ;
  add_unary_vectorized "inv" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_plain
        ( "inv_chi_square_ccdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ( "inv_chi_square_cdf"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ( "inv_chi_square_cdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ( "inv_chi_square_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ( "inv_chi_square_lccdf"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ( "inv_chi_square_lcdf"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_plain
        ( "inv_chi_square_lpdf"
        , ReturnType Real
        , [vector_types i; vector_types j] )
    done
  done ;
  for_all_vector_types (fun t ->
      add_plain
        ("inv_chi_square_rng", ReturnType (rng_return_type Real [t]), [t]) ) ;
  add_unary_vectorized "inv_cloglog" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "inv_gamma_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "inv_gamma_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "inv_gamma_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "inv_gamma_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "inv_gamma_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "inv_gamma_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "inv_gamma_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("inv_gamma_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  add_unary_vectorized "inv_logit" ;
  add_unary_vectorized "inv_Phi" ;
  add_unary_vectorized "inv_sqrt" ;
  add_unary_vectorized "inv_square" ;
  add_plain ("inv_wishart_log", ReturnType Real, [Matrix; Real; Matrix]) ;
  add_plain ("inv_wishart_lpdf", ReturnType Real, [Matrix; Real; Matrix]) ;
  add_plain ("inv_wishart_rng", ReturnType Matrix, [Real; Matrix]) ;
  add_plain ("inverse", ReturnType Matrix, [Matrix]) ;
  add_plain ("inverse_spd", ReturnType Matrix, [Matrix]) ;
  add_plain ("is_inf", ReturnType Int, [Real]) ;
  add_plain ("is_nan", ReturnType Int, [Real]) ;
  add_binary "lbeta" ;
  add_binary "lchoose" ;
  add_unary_vectorized "lgamma" ;
  add_plain ("lkj_corr_cholesky_log", ReturnType Real, [Matrix; Real]) ;
  add_plain ("lkj_corr_cholesky_lpdf", ReturnType Real, [Matrix; Real]) ;
  add_plain ("lkj_corr_cholesky_rng", ReturnType Matrix, [Int; Real]) ;
  add_plain ("lkj_corr_log", ReturnType Real, [Matrix; Real]) ;
  add_plain ("lkj_corr_lpdf", ReturnType Real, [Matrix; Real]) ;
  add_plain ("lkj_corr_rng", ReturnType Matrix, [Int; Real]) ;
  add_plain ("lkj_cov_log", ReturnType Real, [Matrix; Vector; Vector; Real]) ;
  add_plain ("lmgamma", ReturnType Real, [Int; Real]) ;
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
  add_plain ("log_determinant", ReturnType Real, [Matrix]) ;
  add_binary "log_diff_exp" ;
  add_binary "log_falling_factorial" ;
  add_ternary "log_mix" ;
  for i = 1 to vector_types_size - 1 do
    for j = 1 to vector_types_size - 1 do
      add_plain ("log_mix", ReturnType Real, [vector_types i; vector_types j])
    done ;
    add_plain
      ( "log_mix"
      , ReturnType Real
      , [vector_types i; bare_array_type (Vector, 1)] ) ;
    add_plain
      ( "log_mix"
      , ReturnType Real
      , [vector_types i; bare_array_type (RowVector, 1)] )
  done ;
  add_binary "log_rising_factorial" ;
  add_unary_vectorized "log_inv_logit" ;
  add_plain ("log_softmax", ReturnType Vector, [Vector]) ;
  add_plain ("log_sum_exp", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_plain ("log_sum_exp", ReturnType Real, [Vector]) ;
  add_plain ("log_sum_exp", ReturnType Real, [RowVector]) ;
  add_plain ("log_sum_exp", ReturnType Real, [Matrix]) ;
  add_binary "log_sum_exp" ;
  for i = 0 to primitive_types_size - 1 do
    add_plain ("logical_negation", ReturnType Int, [primitive_types i]) ;
    for j = 0 to primitive_types_size - 1 do
      add_plain
        ("logical_or", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_plain
        ("logical_and", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_plain
        ("logical_eq", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_plain
        ("logical_neq", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_plain
        ("logical_lt", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_plain
        ("logical_lte", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_plain
        ("logical_gt", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_plain
        ("logical_gte", ReturnType Int, [primitive_types i; primitive_types j])
    done
  done ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "logistic_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "logistic_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "logistic_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "logistic_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "logistic_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "logistic_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "logistic_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("logistic_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  add_unary_vectorized "logit" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "lognormal_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "lognormal_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "lognormal_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "lognormal_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "lognormal_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "lognormal_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "lognormal_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("lognormal_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  add_nullary "machine_precision" ;
  add_plain ("matrix_exp", ReturnType Matrix, [Matrix]) ;
  add_plain ("matrix_exp_multiply", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("max", ReturnType Int, [bare_array_type (Int, 1)]) ;
  add_plain ("max", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_plain ("max", ReturnType Real, [Vector]) ;
  add_plain ("max", ReturnType Real, [RowVector]) ;
  add_plain ("max", ReturnType Real, [Matrix]) ;
  add_plain ("max", ReturnType Int, [Int; Int]) ;
  add_plain ("mdivide_left", ReturnType Vector, [Matrix; Vector]) ;
  add_plain ("mdivide_left", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("mdivide_left_spd", ReturnType Vector, [Matrix; Vector]) ;
  add_plain ("mdivide_left_spd", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("mdivide_left_tri_low", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("mdivide_left_tri_low", ReturnType Vector, [Matrix; Vector]) ;
  add_plain ("mdivide_right", ReturnType RowVector, [RowVector; Matrix]) ;
  add_plain ("mdivide_right_spd", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("mdivide_right_spd", ReturnType RowVector, [RowVector; Matrix]) ;
  add_plain ("mdivide_right", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("mdivide_right_tri_low", ReturnType RowVector, [RowVector; Matrix]) ;
  add_plain ("mdivide_right_tri_low", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("mean", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_plain ("mean", ReturnType Real, [Vector]) ;
  add_plain ("mean", ReturnType Real, [RowVector]) ;
  add_plain ("mean", ReturnType Real, [Matrix]) ;
  add_plain ("min", ReturnType Int, [bare_array_type (Int, 1)]) ;
  add_plain ("min", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_plain ("min", ReturnType Real, [Vector]) ;
  add_plain ("min", ReturnType Real, [RowVector]) ;
  add_plain ("min", ReturnType Real, [Matrix]) ;
  add_plain ("min", ReturnType Int, [Int; Int]) ;
  add_plain ("minus", ReturnType Real, [Real]) ;
  add_plain ("minus", ReturnType Vector, [Vector]) ;
  add_plain ("minus", ReturnType RowVector, [RowVector]) ;
  add_plain ("minus", ReturnType Matrix, [Matrix]) ;
  add_plain ("modified_bessel_first_kind", ReturnType Real, [Int; Real]) ;
  add_plain ("modified_bessel_second_kind", ReturnType Real, [Int; Real]) ;
  add_plain ("modulus", ReturnType Int, [Int; Int]) ;
  add_plain ("multi_gp_log", ReturnType Real, [Matrix; Matrix; Vector]) ;
  add_plain ("multi_gp_lpdf", ReturnType Real, [Matrix; Matrix; Vector]) ;
  add_plain ("multi_gp_cholesky_log", ReturnType Real, [Matrix; Matrix; Vector]) ;
  add_plain
    ("multi_gp_cholesky_lpdf", ReturnType Real, [Matrix; Matrix; Vector]) ;
  for k = 0 to 4 - 1 do
    for l = 0 to 4 - 1 do
      add_plain
        ( "multi_normal_cholesky_log"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_plain
        ( "multi_normal_cholesky_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_plain
        ( "multi_normal_log"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_plain
        ( "multi_normal_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_plain
        ( "multi_normal_prec_log"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_plain
        ( "multi_normal_prec_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_plain
        ( "multi_student_t_log"
        , ReturnType Real
        , [eigen_vector_types k; Real; eigen_vector_types l; Matrix] ) ;
      add_plain
        ( "multi_student_t_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; Real; eigen_vector_types l; Matrix] )
    done
  done ;
  add_plain ("multi_normal_rng", ReturnType Vector, [Vector; Matrix]) ;
  add_plain
    ( "multi_normal_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [bare_array_type (Vector, 1); Matrix] ) ;
  add_plain ("multi_normal_rng", ReturnType Vector, [RowVector; Matrix]) ;
  add_plain
    ( "multi_normal_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [bare_array_type (RowVector, 1); Matrix] ) ;
  add_plain ("multi_normal_cholesky_rng", ReturnType Vector, [Vector; Matrix]) ;
  add_plain
    ( "multi_normal_cholesky_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [bare_array_type (Vector, 1); Matrix] ) ;
  add_plain
    ("multi_normal_cholesky_rng", ReturnType Vector, [RowVector; Matrix]) ;
  add_plain
    ( "multi_normal_cholesky_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [bare_array_type (RowVector, 1); Matrix] ) ;
  add_plain ("multi_student_t_rng", ReturnType Vector, [Real; Vector; Matrix]) ;
  add_plain
    ( "multi_student_t_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [Real; bare_array_type (Vector, 1); Matrix] ) ;
  add_plain
    ("multi_student_t_rng", ReturnType Vector, [Real; RowVector; Matrix]) ;
  add_plain
    ( "multi_student_t_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [Real; bare_array_type (RowVector, 1); Matrix] ) ;
  add_plain
    ("multinomial_log", ReturnType Real, [bare_array_type (Int, 1); Vector]) ;
  add_plain
    ("multinomial_lpmf", ReturnType Real, [bare_array_type (Int, 1); Vector]) ;
  add_plain
    ("multinomial_rng", ReturnType (bare_array_type (Int, 1)), [Vector; Int]) ;
  add_plain ("multiply", ReturnType Real, [Real; Real]) ;
  add_plain ("multiply", ReturnType Vector, [Vector; Real]) ;
  add_plain ("multiply", ReturnType RowVector, [RowVector; Real]) ;
  add_plain ("multiply", ReturnType Matrix, [Matrix; Real]) ;
  add_plain ("multiply", ReturnType Real, [RowVector; Vector]) ;
  add_plain ("multiply", ReturnType Matrix, [Vector; RowVector]) ;
  add_plain ("multiply", ReturnType Vector, [Matrix; Vector]) ;
  add_plain ("multiply", ReturnType RowVector, [RowVector; Matrix]) ;
  add_plain ("multiply", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("multiply", ReturnType Vector, [Real; Vector]) ;
  add_plain ("multiply", ReturnType RowVector, [Real; RowVector]) ;
  add_plain ("multiply", ReturnType Matrix, [Real; Matrix]) ;
  add_binary "multiply_log" ;
  add_plain ("multiply_lower_tri_self_transpose", ReturnType Matrix, [Matrix]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "neg_binomial_ccdf_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_cdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_cdf_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_lccdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_lcdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_lpmf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_2_ccdf_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_2_cdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_2_cdf_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_2_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_2_lccdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_2_lcdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_2_lpmf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_2_log_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "neg_binomial_2_log_lpmf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ( "neg_binomial_rng"
            , ReturnType (rng_return_type Int [t; u])
            , [t; u] ) ) ) ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ( "neg_binomial_2_rng"
            , ReturnType (rng_return_type Int [t; u])
            , [t; u] ) ) ) ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ( "neg_binomial_2_log_rng"
            , ReturnType (rng_return_type Int [t; u])
            , [t; u] ) ) ) ;
  add_plain
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Real; Vector; Real] ) ;
  add_plain
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Vector; Vector; Real] ) ;
  add_nullary "negative_infinity" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "normal_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "normal_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "normal_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "normal_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "normal_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "normal_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "normal_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("normal_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  add_plain
    ( "normal_id_glm_lpdf"
    , ReturnType Real
    , [Vector; Matrix; Real; Vector; Real] ) ;
  add_plain
    ( "normal_id_glm_lpdf"
    , ReturnType Real
    , [Vector; Matrix; Vector; Vector; Real] ) ;
  add_nullary "not_a_number" ;
  add_plain ("num_elements", ReturnType Int, [Matrix]) ;
  add_plain ("num_elements", ReturnType Int, [Vector]) ;
  add_plain ("num_elements", ReturnType Int, [RowVector]) ;
  for i = 1 to 10 - 1 do
    add_plain ("num_elements", ReturnType Int, [bare_array_type (Int, i)]) ;
    add_plain ("num_elements", ReturnType Int, [bare_array_type (Real, i)]) ;
    add_plain ("num_elements", ReturnType Int, [bare_array_type (Matrix, i)]) ;
    add_plain ("num_elements", ReturnType Int, [bare_array_type (RowVector, i)]) ;
    add_plain ("num_elements", ReturnType Int, [bare_array_type (Vector, i)])
  done ;
  add_plain ("ordered_logistic_log", ReturnType Real, [Int; Real; Vector]) ;
  add_plain
    ( "ordered_logistic_log"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; Vector] ) ;
  add_plain
    ( "ordered_logistic_log"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; bare_array_type (Vector, 1)] ) ;
  add_plain ("ordered_logistic_lpmf", ReturnType Real, [Int; Real; Vector]) ;
  add_plain
    ( "ordered_logistic_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; Vector] ) ;
  add_plain
    ( "ordered_logistic_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; bare_array_type (Vector, 1)] ) ;
  add_plain ("ordered_logistic_rng", ReturnType Int, [Real; Vector]) ;
  add_plain ("ordered_probit_log", ReturnType Real, [Int; Real; Vector]) ;
  add_plain
    ( "ordered_probit_log"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; Vector] ) ;
  add_plain
    ( "ordered_probit_log"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; bare_array_type (Vector, 1)] ) ;
  add_plain ("ordered_probit_lpmf", ReturnType Real, [Int; Real; Vector]) ;
  add_plain
    ( "ordered_probit_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Real; Vector] ) ;
  add_plain
    ( "ordered_probit_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Real; bare_array_type (Vector, 1)] ) ;
  add_plain ("ordered_probit_rng", ReturnType Int, [Real; Vector]) ;
  add_binary "owens_t" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "pareto_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "pareto_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "pareto_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "pareto_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "pareto_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "pareto_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "pareto_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("pareto_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_plain
            ( "pareto_type_2_ccdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "pareto_type_2_cdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "pareto_type_2_cdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "pareto_type_2_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "pareto_type_2_lccdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "pareto_type_2_lcdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "pareto_type_2_lpdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            )
        done
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_plain
                ( "pareto_type_2_rng"
                , ReturnType (rng_return_type Real [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_unary_vectorized "Phi" ;
  add_unary_vectorized "Phi_approx" ;
  add_nullary "pi" ;
  add_plain ("plus", ReturnType Real, [Real]) ;
  add_plain ("plus", ReturnType Vector, [Vector]) ;
  add_plain ("plus", ReturnType RowVector, [RowVector]) ;
  add_plain ("plus", ReturnType Matrix, [Matrix]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_plain
        ( "poisson_ccdf_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_plain
        ("poisson_cdf", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_plain
        ( "poisson_cdf_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_plain
        ("poisson_log", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_plain
        ("poisson_lccdf", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_plain
        ("poisson_lcdf", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_plain
        ("poisson_lpmf", ReturnType Real, [int_vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_plain ("poisson_rng", ReturnType (rng_return_type Int [t]), [t]) ) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_plain
        ( "poisson_log_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_plain
        ( "poisson_log_lpmf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] )
    done
  done ;
  for_all_vector_types (fun t ->
      add_plain ("poisson_log_rng", ReturnType (rng_return_type Int [t]), [t])
  ) ;
  add_plain
    ( "poisson_log_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Real; Vector] ) ;
  add_plain
    ( "poisson_log_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Vector; Vector] ) ;
  add_nullary "positive_infinity" ;
  add_binary "pow" ;
  add_plain ("prod", ReturnType Int, [bare_array_type (Int, 1)]) ;
  add_plain ("prod", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_plain ("prod", ReturnType Real, [Vector]) ;
  add_plain ("prod", ReturnType Real, [RowVector]) ;
  add_plain ("prod", ReturnType Real, [Matrix]) ;
  add_plain ("quad_form", ReturnType Real, [Matrix; Vector]) ;
  add_plain ("quad_form", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("quad_form_sym", ReturnType Real, [Matrix; Vector]) ;
  add_plain ("quad_form_sym", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("quad_form_diag", ReturnType Matrix, [Matrix; Vector]) ;
  add_plain ("quad_form_diag", ReturnType Matrix, [Matrix; RowVector]) ;
  add_plain ("rank", ReturnType Int, [bare_array_type (Int, 1); Int]) ;
  add_plain ("rank", ReturnType Int, [bare_array_type (Real, 1); Int]) ;
  add_plain ("rank", ReturnType Int, [Vector; Int]) ;
  add_plain ("rank", ReturnType Int, [RowVector; Int]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_plain
        ("rayleigh_ccdf_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("rayleigh_cdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("rayleigh_cdf_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("rayleigh_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("rayleigh_lccdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("rayleigh_lcdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_plain
        ("rayleigh_lpdf", ReturnType Real, [vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_plain ("rayleigh_rng", ReturnType (rng_return_type Real [t]), [t]) ) ;
  add_plain ("append_row", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("append_row", ReturnType Matrix, [RowVector; Matrix]) ;
  add_plain ("append_row", ReturnType Matrix, [Matrix; RowVector]) ;
  add_plain ("append_row", ReturnType Matrix, [RowVector; RowVector]) ;
  add_plain ("append_row", ReturnType Vector, [Vector; Vector]) ;
  add_plain ("append_row", ReturnType Vector, [Real; Vector]) ;
  add_plain ("append_row", ReturnType Vector, [Vector; Real]) ;
  for i = 0 to bare_types_size - 1 do
    add_plain
      ( "rep_array"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_types i; Int] ) ;
    add_plain
      ( "rep_array"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_types i; Int; Int] ) ;
    add_plain
      ( "rep_array"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_types i; Int; Int; Int] ) ;
    for j = 1 to 3 - 1 do
      add_plain
        ( "rep_array"
        , ReturnType (bare_array_type (bare_types i, j + 1))
        , [bare_array_type (bare_types i, j); Int] ) ;
      add_plain
        ( "rep_array"
        , ReturnType (bare_array_type (bare_types i, j + 2))
        , [bare_array_type (bare_types i, j); Int; Int] ) ;
      add_plain
        ( "rep_array"
        , ReturnType (bare_array_type (bare_types i, j + 3))
        , [bare_array_type (bare_types i, j); Int; Int; Int] )
    done
  done ;
  add_plain ("rep_matrix", ReturnType Matrix, [Real; Int; Int]) ;
  add_plain ("rep_matrix", ReturnType Matrix, [Vector; Int]) ;
  add_plain ("rep_matrix", ReturnType Matrix, [RowVector; Int]) ;
  add_plain ("rep_row_vector", ReturnType RowVector, [Real; Int]) ;
  add_plain ("rep_vector", ReturnType Vector, [Real; Int]) ;
  add_plain ("rising_factorial", ReturnType Real, [Real; Int]) ;
  add_plain ("rising_factorial", ReturnType Int, [Int; Int]) ;
  add_unary_vectorized "round" ;
  add_plain ("row", ReturnType RowVector, [Matrix; Int]) ;
  add_plain ("rows", ReturnType Int, [Vector]) ;
  add_plain ("rows", ReturnType Int, [RowVector]) ;
  add_plain ("rows", ReturnType Int, [Matrix]) ;
  add_plain ("rows_dot_product", ReturnType Vector, [Vector; Vector]) ;
  add_plain ("rows_dot_product", ReturnType Vector, [RowVector; RowVector]) ;
  add_plain ("rows_dot_product", ReturnType Vector, [Matrix; Matrix]) ;
  add_plain ("rows_dot_self", ReturnType Vector, [Vector]) ;
  add_plain ("rows_dot_self", ReturnType Vector, [RowVector]) ;
  add_plain ("rows_dot_self", ReturnType Vector, [Matrix]) ;
  add_plain
    ("scale_matrix_exp_multiply", ReturnType Matrix, [Real; Matrix; Matrix]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "scaled_inv_chi_square_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "scaled_inv_chi_square_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "scaled_inv_chi_square_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "scaled_inv_chi_square_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "scaled_inv_chi_square_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "scaled_inv_chi_square_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "scaled_inv_chi_square_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ( "scaled_inv_chi_square_rng"
            , ReturnType (rng_return_type Real [t; u])
            , [t; u] ) ) ) ;
  add_plain ("sd", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_plain ("sd", ReturnType Real, [Vector]) ;
  add_plain ("sd", ReturnType Real, [RowVector]) ;
  add_plain ("sd", ReturnType Real, [Matrix]) ;
  add_plain ("segment", ReturnType RowVector, [RowVector; Int; Int]) ;
  add_plain ("segment", ReturnType Vector, [Vector; Int; Int]) ;
  for i = 0 to bare_types_size - 1 do
    add_plain
      ( "segment"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_array_type (bare_types i, 1); Int; Int] ) ;
    add_plain
      ( "segment"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_array_type (bare_types i, 2); Int; Int] ) ;
    add_plain
      ( "segment"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_array_type (bare_types i, 3); Int; Int] )
  done ;
  add_unary_vectorized "sin" ;
  add_plain ("singular_values", ReturnType Vector, [Matrix]) ;
  add_unary_vectorized "sinh" ;
  for i = 1 to 8 - 1 do
    add_plain ("size", ReturnType Int, [bare_array_type (Int, i)]) ;
    add_plain ("size", ReturnType Int, [bare_array_type (Real, i)]) ;
    add_plain ("size", ReturnType Int, [bare_array_type (Vector, i)]) ;
    add_plain ("size", ReturnType Int, [bare_array_type (RowVector, i)]) ;
    add_plain ("size", ReturnType Int, [bare_array_type (Matrix, i)])
  done ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_plain
            ( "skew_normal_ccdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "skew_normal_cdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "skew_normal_cdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "skew_normal_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "skew_normal_lccdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "skew_normal_lcdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "skew_normal_lpdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            )
        done
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_plain
                ( "skew_normal_rng"
                , ReturnType (rng_return_type Real [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_plain ("softmax", ReturnType Vector, [Vector]) ;
  add_plain
    ( "sort_asc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Int, 1)] ) ;
  add_plain
    ( "sort_asc"
    , ReturnType (bare_array_type (Real, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_plain ("sort_asc", ReturnType Vector, [Vector]) ;
  add_plain ("sort_asc", ReturnType RowVector, [RowVector]) ;
  add_plain
    ( "sort_desc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Int, 1)] ) ;
  add_plain
    ( "sort_desc"
    , ReturnType (bare_array_type (Real, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_plain ("sort_desc", ReturnType Vector, [Vector]) ;
  add_plain ("sort_desc", ReturnType RowVector, [RowVector]) ;
  add_plain
    ( "sort_indices_asc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Int, 1)] ) ;
  add_plain
    ( "sort_indices_asc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_plain
    ("sort_indices_asc", ReturnType (bare_array_type (Int, 1)), [Vector]) ;
  add_plain
    ("sort_indices_asc", ReturnType (bare_array_type (Int, 1)), [RowVector]) ;
  add_plain
    ( "sort_indices_desc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Int, 1)] ) ;
  add_plain
    ( "sort_indices_desc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_plain
    ("sort_indices_desc", ReturnType (bare_array_type (Int, 1)), [Vector]) ;
  add_plain
    ("sort_indices_desc", ReturnType (bare_array_type (Int, 1)), [RowVector]) ;
  add_plain ("squared_distance", ReturnType Real, [Real; Real]) ;
  add_plain ("squared_distance", ReturnType Real, [Vector; Vector]) ;
  add_plain ("squared_distance", ReturnType Real, [RowVector; RowVector]) ;
  add_plain ("squared_distance", ReturnType Real, [Vector; RowVector]) ;
  add_plain ("squared_distance", ReturnType Real, [RowVector; Vector]) ;
  add_unary_vectorized "sqrt" ;
  add_nullary "sqrt2" ;
  add_unary_vectorized "square" ;
  for i = 0 to vector_types_size - 1 do
    add_plain ("std_normal_log", ReturnType Real, [vector_types i]) ;
    add_plain ("std_normal_lpdf", ReturnType Real, [vector_types i])
  done ;
  add_unary "step" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_plain
            ( "student_t_ccdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "student_t_cdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "student_t_cdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "student_t_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "student_t_lccdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "student_t_lcdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_plain
            ( "student_t_lpdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            )
        done
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_plain
                ( "student_t_rng"
                , ReturnType (rng_return_type Real [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_plain ("sub_col", ReturnType Vector, [Matrix; Int; Int; Int]) ;
  add_plain ("sub_row", ReturnType RowVector, [Matrix; Int; Int; Int]) ;
  add_plain ("subtract", ReturnType Vector, [Vector; Vector]) ;
  add_plain ("subtract", ReturnType RowVector, [RowVector; RowVector]) ;
  add_plain ("subtract", ReturnType Matrix, [Matrix; Matrix]) ;
  add_plain ("subtract", ReturnType Vector, [Vector; Real]) ;
  add_plain ("subtract", ReturnType RowVector, [RowVector; Real]) ;
  add_plain ("subtract", ReturnType Matrix, [Matrix; Real]) ;
  add_plain ("subtract", ReturnType Vector, [Real; Vector]) ;
  add_plain ("subtract", ReturnType RowVector, [Real; RowVector]) ;
  add_plain ("subtract", ReturnType Matrix, [Real; Matrix]) ;
  add_plain ("sum", ReturnType Int, [bare_array_type (Int, 1)]) ;
  add_plain ("sum", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_plain ("sum", ReturnType Real, [Vector]) ;
  add_plain ("sum", ReturnType Real, [RowVector]) ;
  add_plain ("sum", ReturnType Real, [Matrix]) ;
  add_plain ("tail", ReturnType RowVector, [RowVector; Int]) ;
  add_plain ("tail", ReturnType Vector, [Vector; Int]) ;
  for i = 0 to bare_types_size - 1 do
    add_plain
      ( "tail"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_array_type (bare_types i, 1); Int] ) ;
    add_plain
      ( "tail"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_array_type (bare_types i, 2); Int] ) ;
    add_plain
      ( "tail"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_array_type (bare_types i, 3); Int] )
  done ;
  add_unary_vectorized "tan" ;
  add_unary_vectorized "tanh" (* ; add_nullary ("target") *) ;
  add_plain ("tcrossprod", ReturnType Matrix, [Matrix]) ;
  add_unary_vectorized "tgamma" ;
  add_plain ("to_array_1d", ReturnType (bare_array_type (Real, 1)), [Matrix]) ;
  add_plain ("to_array_1d", ReturnType (bare_array_type (Real, 1)), [Vector]) ;
  add_plain ("to_array_1d", ReturnType (bare_array_type (Real, 1)), [RowVector]) ;
  for i = 1 to 10 - 1 do
    add_plain
      ( "to_array_1d"
      , ReturnType (bare_array_type (Real, 1))
      , [bare_array_type (Real, i)] ) ;
    add_plain
      ( "to_array_1d"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Int, i)] )
  done ;
  add_plain ("to_array_2d", ReturnType (bare_array_type (Real, 2)), [Matrix]) ;
  add_plain ("to_matrix", ReturnType Matrix, [Matrix]) ;
  add_plain ("to_matrix", ReturnType Matrix, [Matrix; Int; Int]) ;
  add_plain ("to_matrix", ReturnType Matrix, [Matrix; Int; Int; Int]) ;
  add_plain ("to_matrix", ReturnType Matrix, [Vector]) ;
  add_plain ("to_matrix", ReturnType Matrix, [Vector; Int; Int]) ;
  add_plain ("to_matrix", ReturnType Matrix, [Vector; Int; Int; Int]) ;
  add_plain ("to_matrix", ReturnType Matrix, [RowVector]) ;
  add_plain ("to_matrix", ReturnType Matrix, [RowVector; Int; Int]) ;
  add_plain ("to_matrix", ReturnType Matrix, [RowVector; Int; Int; Int]) ;
  add_plain
    ("to_matrix", ReturnType Matrix, [bare_array_type (Real, 1); Int; Int]) ;
  add_plain
    ("to_matrix", ReturnType Matrix, [bare_array_type (Real, 1); Int; Int; Int]) ;
  add_plain
    ("to_matrix", ReturnType Matrix, [bare_array_type (Int, 1); Int; Int]) ;
  add_plain
    ("to_matrix", ReturnType Matrix, [bare_array_type (Int, 1); Int; Int; Int]) ;
  add_plain ("to_matrix", ReturnType Matrix, [bare_array_type (Real, 2)]) ;
  add_plain ("to_matrix", ReturnType Matrix, [bare_array_type (Int, 2)]) ;
  add_plain ("to_row_vector", ReturnType RowVector, [Matrix]) ;
  add_plain ("to_row_vector", ReturnType RowVector, [Vector]) ;
  add_plain ("to_row_vector", ReturnType RowVector, [RowVector]) ;
  add_plain ("to_row_vector", ReturnType RowVector, [bare_array_type (Real, 1)]) ;
  add_plain ("to_row_vector", ReturnType RowVector, [bare_array_type (Int, 1)]) ;
  add_plain ("to_vector", ReturnType Vector, [Matrix]) ;
  add_plain ("to_vector", ReturnType Vector, [Vector]) ;
  add_plain ("to_vector", ReturnType Vector, [RowVector]) ;
  add_plain ("to_vector", ReturnType Vector, [bare_array_type (Real, 1)]) ;
  add_plain ("to_vector", ReturnType Vector, [bare_array_type (Int, 1)]) ;
  add_plain ("trace", ReturnType Real, [Matrix]) ;
  add_plain ("trace_gen_quad_form", ReturnType Real, [Matrix; Matrix; Matrix]) ;
  add_plain ("trace_quad_form", ReturnType Real, [Matrix; Vector]) ;
  add_plain ("trace_quad_form", ReturnType Real, [Matrix; Matrix]) ;
  add_plain ("transpose", ReturnType RowVector, [Vector]) ;
  add_plain ("transpose", ReturnType Vector, [RowVector]) ;
  add_plain ("transpose", ReturnType Matrix, [Matrix]) ;
  add_unary_vectorized "trunc" ;
  add_unary_vectorized "trigamma" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "uniform_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "uniform_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "uniform_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "uniform_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "uniform_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "uniform_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "uniform_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("uniform_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  add_plain ("variance", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_plain ("variance", ReturnType Real, [Vector]) ;
  add_plain ("variance", ReturnType Real, [RowVector]) ;
  add_plain ("variance", ReturnType Real, [Matrix]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "von_mises_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "von_mises_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("von_mises_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_plain
          ( "weibull_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "weibull_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "weibull_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "weibull_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "weibull_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "weibull_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_plain
          ( "weibull_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("weibull_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          for m = 0 to vector_types_size - 1 do
            add_plain
              ( "wiener_log"
              , ReturnType Real
              , [ vector_types i
                ; vector_types j
                ; vector_types k
                ; vector_types l
                ; vector_types m ] ) ;
            add_plain
              ( "wiener_lpdf"
              , ReturnType Real
              , [ vector_types i
                ; vector_types j
                ; vector_types k
                ; vector_types l
                ; vector_types m ] )
          done
        done
      done
    done
  done ;
  add_plain ("wishart_log", ReturnType Real, [Matrix; Real; Matrix]) ;
  add_plain ("wishart_lpdf", ReturnType Real, [Matrix; Real; Matrix]) ;
  add_plain ("wishart_rng", ReturnType Matrix, [Real; Matrix])

(* TODO *)
let try_get_primitive_return_type name optargtypes =
  if List.exists (fun x -> x = None) optargtypes then None
  else
    let argtypes =
      List.map
        (function
          | Some x -> x
          | None ->
              semantic_error
                "This should never happen. Please report a bug. Error code 15.")
        optargtypes
    in
    let uts = List.map snd argtypes in
    let namematches = Hashtbl.find_all primitive_signatures name in
    let filteredmatches =
      List.filter
        (fun x ->
          List.length (snd x) = List.length uts
          && List.for_all
               (fun y -> y = true)
               (List.map2 (check_of_same_type_mod_conv name) (snd x) uts) )
        namematches
    in
    let _ =
      if List.length filteredmatches > 1 then
        semantic_error
          "This should never happen. Please file a bug. Error code 16."
    in
    if List.length filteredmatches = 0 then None
    else Some (fst (List.hd filteredmatches))

(* TODO: deal with data only arguments *)

let is_primitive_name name = Hashtbl.mem primitive_signatures name

let operator_names = Hashtbl.create 50

let _ = Hashtbl.add operator_names "Plus" "add"

let _ = Hashtbl.add operator_names "Minus" "subtract"

let _ = Hashtbl.add operator_names "Times" "multiply"

let _ = Hashtbl.add operator_names "Divide" "divide"

let _ = Hashtbl.add operator_names "Divide" "mdivide_right"

let _ = Hashtbl.add operator_names "Modulo" "modulus"

let _ = Hashtbl.add operator_names "LDivide" "mdivide_left"

let _ = Hashtbl.add operator_names "EltTimes" "elt_multiply"

let _ = Hashtbl.add operator_names "EltDivide" "elt_divide"

let _ = Hashtbl.add operator_names "Exp" "pow"

let _ = Hashtbl.add operator_names "Or" "logical_or"

let _ = Hashtbl.add operator_names "And" "logical_and"

let _ = Hashtbl.add operator_names "Equals" "logical_eq"

let _ = Hashtbl.add operator_names "NEquals" "logical_neq"

let _ = Hashtbl.add operator_names "Less" "logical_lt"

let _ = Hashtbl.add operator_names "Leq" "logical_lte"

let _ = Hashtbl.add operator_names "Greater" "logical_gt"

let _ = Hashtbl.add operator_names "Geq" "logical_gte"

let _ = Hashtbl.add operator_names "Not" "logical_negation"

let _ = Hashtbl.add operator_names "UMinus" "minus"

let _ = Hashtbl.add operator_names "Uplus" "plus"

let _ = Hashtbl.add operator_names "Transpose" "transpose"

let _ = Hashtbl.add operator_names "Conditional" "if_else"

let _ = Hashtbl.add operator_names "PlusAssign" "assign_add"

let _ = Hashtbl.add operator_names "MinusAssign" "assign_subtract"

let _ = Hashtbl.add operator_names "TimesAssign" "assign_multiply"

let _ = Hashtbl.add operator_names "DivideAssign" "assign_divide"

let _ = Hashtbl.add operator_names "EltTimesAssign" "assign_elt_times"

let _ = Hashtbl.add operator_names "EltDivideAssign" "assign_elt_divide"

let try_get_operator_return_type op_name optargtypes =
  if op_name = "Assign" || op_name = "Arrow_Assign" then
    match optargtypes with
    | [Some (_, ut1); Some (_, ut2)] ->
        if check_of_same_type_mod_conv "" ut1 ut2 then Some Void else None
    | _ -> None
  else
    let rec try_recursive_find = function
      | [] -> None
      | name :: names -> (
        match try_get_primitive_return_type name optargtypes with
        | None -> try_recursive_find names
        | Some ut -> Some ut )
    in
    try_recursive_find (Hashtbl.find_all operator_names op_name)
