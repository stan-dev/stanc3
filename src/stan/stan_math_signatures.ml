(* Here, we define type checking ; for the Stan Math library *)

(* This is ugly. An ideal treatment of function overloading works by carrying around*
   a LAZY set of types ; for each expression. However, that's awkward in OCaml.
   Perhaps an argument ; for Haskell after all?
   OCaml-1 does have lazy lists. Perhaps those could be used ; for this purpose?
   Or implement our own lazy sets?
   
*)

open Ast

(* A semantic error reported by the toplevel *)
let semantic_error ?loc msg =
  Command_line_app.error ~kind:"Semantic error" ?loc (Scanf.format_from_string msg "")

(* We allow implicit conversion from int to real, except for assignment operators *)
let check_of_same_type_mod_conv name t1 t2 =
  if Core_kernel.String.is_prefix name ~prefix:"assign_" then t1 = t2
  else t1 = t2 || (t1 = Real && t2 = Int)

let rec check_of_same_type_mod_array_conv name t1 t2 =
  if Core_kernel.String.is_prefix name ~prefix:"assign_" then t1 = t2
  else
    match (t1, t2) with
    | Array t1elt, Array t2elt -> check_of_same_type_mod_conv name t1elt t2elt
    | _ -> t1 = t2 || (t1 = Real && t2 = Int)

let check_of_compatible_return_type rt1 rt2 =
  match (rt1, rt2) with
  | Void, Void -> true
  | ReturnType Real, ReturnType Int -> true
  | _ -> rt1 = rt2

let check_compatible_arguments_mod_conv name args1 args2 =
  List.length args1 = List.length args2
  && List.for_all
       (fun y -> y = true)
       (List.map2
          (fun w1 w2 ->
            check_of_same_type_mod_conv name (snd w1) (snd w2)
            && compare_originblock (fst w1) (fst w2) > -1 )
          args1 args2)

let primitive_signatures = Hashtbl.create 3000

let rec bare_array_type (t, i) =
  match i with 0 -> t | j -> Array (bare_array_type (t, j - 1))

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

let add_unqualified (name, rt, uqargts) =
  Hashtbl.add primitive_signatures name
    (rt, List.map (fun x -> (GQuant, x)) uqargts)

let add_qualified (name, rt, argts) =
  Hashtbl.add primitive_signatures name (rt, argts)

let add_nullary name = add_unqualified (name, ReturnType Real, [])

let add_unary name = add_unqualified (name, ReturnType Real, [Real])

let add_unary_vectorized name =
  for j = 0 to 8 - 1 do
    add_unqualified
      (name, ReturnType (bare_array_type (Real, j)), [bare_array_type (Int, j)]) ;
    add_unqualified
      ( name
      , ReturnType (bare_array_type (Real, j))
      , [bare_array_type (Real, j)] ) ;
    add_unqualified
      ( name
      , ReturnType (bare_array_type (Vector, j))
      , [bare_array_type (Vector, j)] ) ;
    add_unqualified
      ( name
      , ReturnType (bare_array_type (RowVector, j))
      , [bare_array_type (RowVector, j)] ) ;
    add_unqualified
      ( name
      , ReturnType (bare_array_type (Matrix, j))
      , [bare_array_type (Matrix, j)] )
  done

let add_binary name = add_unqualified (name, ReturnType Real, [Real; Real])

let add_ternary name =
  add_unqualified (name, ReturnType Real, [Real; Real; Real])

let add_quaternary name =
  add_unqualified (name, ReturnType Real, [Real; Real; Real; Real])

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
  add_unqualified ("abs", ReturnType Int, [Int]) ;
  add_unqualified ("abs", ReturnType Real, [Real]) ;
  add_unary_vectorized "acos" ;
  add_unary_vectorized "acosh" ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ("add", ReturnType (bare_types i), [bare_types i; bare_types i])
  done ;
  add_unqualified ("add", ReturnType Vector, [Vector; Real]) ;
  add_unqualified ("add", ReturnType RowVector, [RowVector; Real]) ;
  add_unqualified ("add", ReturnType Matrix, [Matrix; Real]) ;
  add_unqualified ("add", ReturnType Vector, [Real; Vector]) ;
  add_unqualified ("add", ReturnType RowVector, [Real; RowVector]) ;
  add_unqualified ("add", ReturnType Matrix, [Real; Matrix]) ;
  add_qualified
    ( "algebra_solver"
    , ReturnType Vector
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Vector)
              ; (GQuant, Vector)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType Vector ) )
      ; (GQuant, Vector)
      ; (GQuant, Vector)
      ; (TData, Array Real)
      ; (TData, Array Int) ] ) ;
  add_qualified
    ( "algebra_solver"
    , ReturnType Vector
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Vector)
              ; (GQuant, Vector)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType Vector ) )
      ; (GQuant, Vector)
      ; (GQuant, Vector)
      ; (TData, Array Real)
      ; (TData, Array Int)
      ; (TData, Real)
      ; (TData, Real)
      ; (TData, Real) ] ) ;
  for i = 1 to 8 - 1 do
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (Int, i))
      , [bare_array_type (Int, i); bare_array_type (Int, i)] ) ;
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (Real, i))
      , [bare_array_type (Real, i); bare_array_type (Real, i)] ) ;
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (Vector, i))
      , [bare_array_type (Vector, i); bare_array_type (Vector, i)] ) ;
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (RowVector, i))
      , [bare_array_type (RowVector, i); bare_array_type (RowVector, i)] ) ;
    add_unqualified
      ( "append_array"
      , ReturnType (bare_array_type (Matrix, i))
      , [bare_array_type (Matrix, i); bare_array_type (Matrix, i)] )
  done ;
  add_unary_vectorized "asin" ;
  add_unary_vectorized "asinh" ;
  add_unqualified ("assign_multiply", Void, [Int; Int]) ;
  add_unqualified ("assign_multiply", Void, [Matrix; Matrix]) ;
  add_unqualified ("assign_multiply", Void, [Matrix; Real]) ;
  add_unqualified ("assign_multiply", Void, [Real; Real]) ;
  add_unqualified ("assign_multiply", Void, [RowVector; Real]) ;
  add_unqualified ("assign_multiply", Void, [Matrix; Int]) ;
  add_unqualified ("assign_multiply", Void, [Real; Int]) ;
  add_unqualified ("assign_multiply", Void, [RowVector; Int]) ;
  add_unqualified ("assign_multiply", Void, [RowVector; Matrix]) ;
  add_unqualified ("assign_multiply", Void, [Vector; Real]) ;
  add_unqualified ("assign_multiply", Void, [Vector; Int]) ;
  add_unqualified ("assign_add", Void, [Int; Int]) ;
  add_unqualified ("assign_add", Void, [Matrix; Matrix]) ;
  add_unqualified ("assign_add", Void, [Matrix; Real]) ;
  add_unqualified ("assign_add", Void, [Real; Real]) ;
  add_unqualified ("assign_add", Void, [RowVector; Real]) ;
  add_unqualified ("assign_add", Void, [Matrix; Int]) ;
  add_unqualified ("assign_add", Void, [Real; Int]) ;
  add_unqualified ("assign_add", Void, [RowVector; Int]) ;
  add_unqualified ("assign_add", Void, [RowVector; RowVector]) ;
  add_unqualified ("assign_add", Void, [Vector; Real]) ;
  add_unqualified ("assign_add", Void, [Vector; Int]) ;
  add_unqualified ("assign_add", Void, [Vector; Vector]) ;
  add_unqualified ("assign_subtract", Void, [Int; Int]) ;
  add_unqualified ("assign_subtract", Void, [Matrix; Matrix]) ;
  add_unqualified ("assign_subtract", Void, [Matrix; Real]) ;
  add_unqualified ("assign_subtract", Void, [Real; Real]) ;
  add_unqualified ("assign_subtract", Void, [RowVector; Real]) ;
  add_unqualified ("assign_subtract", Void, [Matrix; Int]) ;
  add_unqualified ("assign_subtract", Void, [Real; Int]) ;
  add_unqualified ("assign_subtract", Void, [RowVector; Int]) ;
  add_unqualified ("assign_subtract", Void, [RowVector; RowVector]) ;
  add_unqualified ("assign_subtract", Void, [Vector; Real]) ;
  add_unqualified ("assign_subtract", Void, [Vector; Int]) ;
  add_unqualified ("assign_subtract", Void, [Vector; Vector]) ;
  add_unqualified ("assign_elt_times", Void, [Matrix; Matrix]) ;
  add_unqualified ("assign_elt_times", Void, [RowVector; RowVector]) ;
  add_unqualified ("assign_elt_times", Void, [Vector; Vector]) ;
  add_unqualified ("assign_elt_divide", Void, [Matrix; Matrix]) ;
  add_unqualified ("assign_elt_divide", Void, [Matrix; Real]) ;
  add_unqualified ("assign_elt_divide", Void, [RowVector; Real]) ;
  add_unqualified ("assign_elt_divide", Void, [Matrix; Int]) ;
  add_unqualified ("assign_elt_divide", Void, [RowVector; Int]) ;
  add_unqualified ("assign_elt_divide", Void, [RowVector; RowVector]) ;
  add_unqualified ("assign_elt_divide", Void, [Vector; Real]) ;
  add_unqualified ("assign_elt_divide", Void, [Vector; Int]) ;
  add_unqualified ("assign_elt_divide", Void, [Vector; Vector]) ;
  add_unqualified ("assign_divide", Void, [Int; Int]) ;
  add_unqualified ("assign_divide", Void, [Matrix; Real]) ;
  add_unqualified ("assign_divide", Void, [Real; Real]) ;
  add_unqualified ("assign_divide", Void, [RowVector; Real]) ;
  add_unqualified ("assign_divide", Void, [Vector; Real]) ;
  add_unqualified ("assign_divide", Void, [Matrix; Int]) ;
  add_unqualified ("assign_divide", Void, [Real; Int]) ;
  add_unqualified ("assign_divide", Void, [RowVector; Int]) ;
  add_unqualified ("assign_divide", Void, [Vector; Int]) ;
  add_unary_vectorized "atan" ;
  add_binary "atan2" ;
  add_unary_vectorized "atanh" ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "bernoulli_ccdf_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ("bernoulli_cdf", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_unqualified
        ( "bernoulli_cdf_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ("bernoulli_log", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_unqualified
        ( "bernoulli_lccdf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_lcdf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_lpmf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] )
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("bernoulli_rng", ReturnType (rng_return_type Int [t]), [t]) ) ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("bernoulli_logit_rng", ReturnType (rng_return_type Int [t]), [t]) ) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "bernoulli_logit_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "bernoulli_logit_lpmf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] )
    done
  done ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Real; Vector] ) ;
  add_unqualified
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Vector; Vector] ) ;
  add_unqualified ("bessel_first_kind", ReturnType Real, [Int; Real]) ;
  add_unqualified ("bessel_second_kind", ReturnType Real, [Int; Real]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to int_vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_unqualified
            ( "beta_binomial_ccdf_log"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_cdf"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_cdf_log"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_log"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_lccdf"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
            ( "beta_binomial_lcdf"
            , ReturnType Real
            , [ int_vector_types i
              ; int_vector_types j
              ; vector_types k
              ; vector_types l ] ) ;
          add_unqualified
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
              add_unqualified
                ( "beta_binomial_rng"
                , ReturnType (rng_return_type Int [t; u; v])
                , [t; u; v] ) ) ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "beta_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "beta_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("beta_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) ) ) ;
  for_vector_types (fun t ->
      for_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_unqualified
                ("beta_proportion_ccdf_log", ReturnType Real, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_cdf_log", ReturnType Real, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_log", ReturnType Real, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_lccdf", ReturnType Real, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_lcdf", ReturnType Real, [t; u; v]) ;
              add_unqualified
                ("beta_proportion_lpdf", ReturnType Real, [t; u; v]) ) ) ) ;
  for_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "beta_proportion_rng"
            , ReturnType (rng_return_type Real [t; u])
            , [t; u] ) ) ) ;
  add_unqualified ("binary_log_loss", ReturnType Real, [Int; Real]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to int_vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "binomial_ccdf_log"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_cdf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_cdf_log"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_log"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_lccdf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_lcdf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_lpmf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] )
      done
    done
  done ;
  for_int_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("binomial_rng", ReturnType (rng_return_type Int [t; u]), [t; u])
      ) ) ;
  add_binary "binomial_coefficient_log" ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to int_vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "binomial_logit_log"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] ) ;
        add_unqualified
          ( "binomial_logit_lpmf"
          , ReturnType Real
          , [int_vector_types i; int_vector_types j; vector_types k] )
      done
    done
  done ;
  add_unqualified ("block", ReturnType Matrix, [Matrix; Int; Int; Int; Int]) ;
  for i = 0 to int_vector_types_size - 1 do
    add_unqualified
      ("categorical_log", ReturnType Real, [int_vector_types i; Vector]) ;
    add_unqualified
      ("categorical_logit_log", ReturnType Real, [int_vector_types i; Vector]) ;
    add_unqualified
      ("categorical_lpmf", ReturnType Real, [int_vector_types i; Vector]) ;
    add_unqualified
      ("categorical_logit_lpmf", ReturnType Real, [int_vector_types i; Vector])
  done ;
  add_unqualified ("categorical_rng", ReturnType Int, [Vector]) ;
  add_unqualified ("categorical_logit_rng", ReturnType Int, [Vector]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "cauchy_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "cauchy_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("cauchy_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  add_unqualified ("append_col", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("append_col", ReturnType Matrix, [Vector; Matrix]) ;
  add_unqualified ("append_col", ReturnType Matrix, [Matrix; Vector]) ;
  add_unqualified ("append_col", ReturnType Matrix, [Vector; Vector]) ;
  add_unqualified ("append_col", ReturnType RowVector, [RowVector; RowVector]) ;
  add_unqualified ("append_col", ReturnType RowVector, [Real; RowVector]) ;
  add_unqualified ("append_col", ReturnType RowVector, [RowVector; Real]) ;
  add_unary_vectorized "cbrt" ;
  add_unary_vectorized "ceil" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "chi_square_ccdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("chi_square_cdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ( "chi_square_cdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("chi_square_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("chi_square_lccdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("chi_square_lcdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("chi_square_lpdf", ReturnType Real, [vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("chi_square_rng", ReturnType (rng_return_type Real [t]), [t]) ) ;
  add_unqualified ("cholesky_decompose", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("choose", ReturnType Int, [Int; Int]) ;
  add_unqualified ("col", ReturnType Vector, [Matrix; Int]) ;
  add_unqualified ("cols", ReturnType Int, [Vector]) ;
  add_unqualified ("cols", ReturnType Int, [RowVector]) ;
  add_unqualified ("cols", ReturnType Int, [Matrix]) ;
  add_unqualified
    ("columns_dot_product", ReturnType RowVector, [Vector; Vector]) ;
  add_unqualified
    ("columns_dot_product", ReturnType RowVector, [RowVector; RowVector]) ;
  add_unqualified
    ("columns_dot_product", ReturnType RowVector, [Matrix; Matrix]) ;
  add_unqualified ("columns_dot_self", ReturnType RowVector, [Vector]) ;
  add_unqualified ("columns_dot_self", ReturnType RowVector, [RowVector]) ;
  add_unqualified ("columns_dot_self", ReturnType RowVector, [Matrix]) ;
  add_unary_vectorized "cos" ;
  add_unary_vectorized "cosh" ;
  add_unqualified
    ("cov_exp_quad", ReturnType Matrix, [bare_array_type (Real, 1); Real; Real]) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (Vector, 1); Real; Real] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (RowVector, 1); Real; Real] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (Real, 1); bare_array_type (Real, 1); Real; Real] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (Vector, 1); bare_array_type (Vector, 1); Real; Real] ) ;
  add_unqualified
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [ bare_array_type (RowVector, 1)
      ; bare_array_type (RowVector, 1)
      ; Real
      ; Real ] ) ;
  add_unqualified ("crossprod", ReturnType Matrix, [Matrix]) ;
  add_unqualified
    ( "csr_matrix_times_vector"
    , ReturnType Vector
    , [ Int
      ; Int
      ; Vector
      ; bare_array_type (Int, 1)
      ; bare_array_type (Int, 1)
      ; Vector ] ) ;
  add_unqualified
    ( "csr_to_dense_matrix"
    , ReturnType Matrix
    , [Int; Int; Vector; bare_array_type (Int, 1); bare_array_type (Int, 1)] ) ;
  add_unqualified ("csr_extract_w", ReturnType Vector, [Matrix]) ;
  add_unqualified
    ("csr_extract_v", ReturnType (bare_array_type (Int, 1)), [Matrix]) ;
  add_unqualified
    ("csr_extract_u", ReturnType (bare_array_type (Int, 1)), [Matrix]) ;
  add_unqualified
    ( "cumulative_sum"
    , ReturnType (bare_array_type (Real, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_unqualified ("cumulative_sum", ReturnType Vector, [Vector]) ;
  add_unqualified ("cumulative_sum", ReturnType RowVector, [RowVector]) ;
  add_unqualified ("determinant", ReturnType Real, [Matrix]) ;
  add_unqualified ("diag_matrix", ReturnType Matrix, [Vector]) ;
  add_unqualified ("diag_post_multiply", ReturnType Matrix, [Matrix; Vector]) ;
  add_unqualified ("diag_post_multiply", ReturnType Matrix, [Matrix; RowVector]) ;
  add_unqualified ("diag_pre_multiply", ReturnType Matrix, [Vector; Matrix]) ;
  add_unqualified ("diag_pre_multiply", ReturnType Matrix, [RowVector; Matrix]) ;
  add_unqualified ("diagonal", ReturnType Vector, [Matrix]) ;
  add_unary_vectorized "digamma" ;
  add_unqualified ("dims", ReturnType (bare_array_type (Int, 1)), [Int]) ;
  add_unqualified ("dims", ReturnType (bare_array_type (Int, 1)), [Real]) ;
  add_unqualified ("dims", ReturnType (bare_array_type (Int, 1)), [Vector]) ;
  add_unqualified ("dims", ReturnType (bare_array_type (Int, 1)), [RowVector]) ;
  add_unqualified ("dims", ReturnType (bare_array_type (Int, 1)), [Matrix]) ;
  for i = 0 to 8 - 1 do
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Int, i + 1)] ) ;
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Real, i + 1)] ) ;
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Vector, i + 1)] ) ;
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (RowVector, i + 1)] ) ;
    add_unqualified
      ( "dims"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Matrix, i + 1)] )
  done ;
  add_unqualified ("dirichlet_log", ReturnType Real, [Vector; Vector]) ;
  add_unqualified ("dirichlet_lpdf", ReturnType Real, [Vector; Vector]) ;
  add_unqualified ("dirichlet_rng", ReturnType Vector, [Vector]) ;
  add_unqualified ("distance", ReturnType Real, [Vector; Vector]) ;
  add_unqualified ("distance", ReturnType Real, [RowVector; RowVector]) ;
  add_unqualified ("distance", ReturnType Real, [Vector; RowVector]) ;
  add_unqualified ("distance", ReturnType Real, [RowVector; Vector]) ;
  add_unqualified ("divide", ReturnType Int, [Int; Int]) ;
  add_unqualified ("divide", ReturnType Real, [Real; Real]) ;
  add_unqualified ("divide", ReturnType Vector, [Vector; Real]) ;
  add_unqualified ("divide", ReturnType RowVector, [RowVector; Real]) ;
  add_unqualified ("divide", ReturnType Matrix, [Matrix; Real]) ;
  add_unqualified ("dot_product", ReturnType Real, [Vector; Vector]) ;
  add_unqualified ("dot_product", ReturnType Real, [RowVector; RowVector]) ;
  add_unqualified ("dot_product", ReturnType Real, [Vector; RowVector]) ;
  add_unqualified ("dot_product", ReturnType Real, [RowVector; Vector]) ;
  add_unqualified
    ( "dot_product"
    , ReturnType Real
    , [bare_array_type (Real, 1); bare_array_type (Real, 1)] ) ;
  add_unqualified ("dot_self", ReturnType Real, [Vector]) ;
  add_unqualified ("dot_self", ReturnType Real, [RowVector]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "double_exponential_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "double_exponential_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "double_exponential_rng"
            , ReturnType (rng_return_type Real [t; u])
            , [t; u] ) ) ) ;
  add_nullary "e" ;
  add_unqualified ("eigenvalues_sym", ReturnType Vector, [Matrix]) ;
  add_unqualified ("eigenvectors_sym", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("qr_Q", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("qr_R", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("qr_thin_Q", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("qr_thin_R", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("elt_divide", ReturnType Vector, [Vector; Vector]) ;
  add_unqualified ("elt_divide", ReturnType RowVector, [RowVector; RowVector]) ;
  add_unqualified ("elt_divide", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("elt_divide", ReturnType Vector, [Vector; Real]) ;
  add_unqualified ("elt_divide", ReturnType RowVector, [RowVector; Real]) ;
  add_unqualified ("elt_divide", ReturnType Matrix, [Matrix; Real]) ;
  add_unqualified ("elt_divide", ReturnType Vector, [Real; Vector]) ;
  add_unqualified ("elt_divide", ReturnType RowVector, [Real; RowVector]) ;
  add_unqualified ("elt_divide", ReturnType Matrix, [Real; Matrix]) ;
  add_unqualified ("elt_multiply", ReturnType Vector, [Vector; Vector]) ;
  add_unqualified ("elt_multiply", ReturnType RowVector, [RowVector; RowVector]) ;
  add_unqualified ("elt_multiply", ReturnType Matrix, [Matrix; Matrix]) ;
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
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_cdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_cdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_lccdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "exp_mod_normal_lcdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
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
              add_unqualified
                ( "exp_mod_normal_rng"
                , ReturnType (rng_return_type Real [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_unary_vectorized "expm1" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "exponential_ccdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("exponential_cdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ( "exponential_cdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ("exponential_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("exponential_lccdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("exponential_lcdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("exponential_lpdf", ReturnType Real, [vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("exponential_rng", ReturnType (rng_return_type Real [t]), [t]) ) ;
  add_unary_vectorized "fabs" ;
  add_unqualified ("falling_factorial", ReturnType Real, [Real; Int]) ;
  add_unqualified ("falling_factorial", ReturnType Int, [Int; Int]) ;
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
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "frechet_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("frechet_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "gamma_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gamma_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
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
          add_unqualified
            ("gamma_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  add_unqualified
    ( "gaussian_dlm_obs_log"
    , ReturnType Real
    , [Matrix; Matrix; Matrix; Matrix; Matrix; Vector; Matrix] ) ;
  add_unqualified
    ( "gaussian_dlm_obs_log"
    , ReturnType Real
    , [Matrix; Matrix; Matrix; Vector; Matrix; Vector; Matrix] ) ;
  add_unqualified
    ( "gaussian_dlm_obs_lpdf"
    , ReturnType Real
    , [Matrix; Matrix; Matrix; Matrix; Matrix; Vector; Matrix] ) ;
  add_unqualified
    ( "gaussian_dlm_obs_lpdf"
    , ReturnType Real
    , [Matrix; Matrix; Matrix; Vector; Matrix; Vector; Matrix] )
  (* ; add_nullary ("get_lp")   *) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "gumbel_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "gumbel_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("gumbel_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  add_unqualified ("head", ReturnType RowVector, [RowVector; Int]) ;
  add_unqualified ("head", ReturnType Vector, [Vector; Int]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ( "head"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_array_type (bare_types i, 1); Int] ) ;
    add_unqualified
      ( "head"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_array_type (bare_types i, 2); Int] ) ;
    add_unqualified
      ( "head"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_array_type (bare_types i, 3); Int] )
  done ;
  add_unqualified ("hypergeometric_log", ReturnType Real, [Int; Int; Int; Int]) ;
  add_unqualified ("hypergeometric_lpmf", ReturnType Real, [Int; Int; Int; Int]) ;
  add_unqualified ("hypergeometric_rng", ReturnType Int, [Int; Int; Int]) ;
  add_binary "hypot" ;
  add_unqualified ("if_else", ReturnType Real, [Int; Real; Real]) ;
  add_unqualified ("if_else", ReturnType Int, [Int; Int; Int]) ;
  add_unqualified ("inc_beta", ReturnType Real, [Real; Real; Real]) ;
  add_unqualified ("int_step", ReturnType Int, [Real]) ;
  add_unqualified ("int_step", ReturnType Int, [Int]) ;
  add_qualified
    ( "integrate_ode"
    , ReturnType (Array (Array Real))
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType (Array Real) ) )
      ; (GQuant, Array Real)
      ; (TData, Real)
      ; (TData, Array Real)
      ; (GQuant, Array Real)
      ; (TData, Array Real)
      ; (TData, Array Int) ] ) ;
  add_qualified
    ( "integrate_ode_adams"
    , ReturnType (Array (Array Real))
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType (Array Real) ) )
      ; (GQuant, Array Real)
      ; (TData, Real)
      ; (TData, Array Real)
      ; (GQuant, Array Real)
      ; (TData, Array Real)
      ; (TData, Array Int) ] ) ;
  add_qualified
    ( "integrate_ode_adams"
    , ReturnType (Array (Array Real))
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType (Array Real) ) )
      ; (GQuant, Array Real)
      ; (TData, Real)
      ; (TData, Array Real)
      ; (GQuant, Array Real)
      ; (TData, Array Real)
      ; (TData, Array Int)
      ; (TData, Real)
      ; (TData, Real)
      ; (TData, Real) ] ) ;
  add_qualified
    ( "integrate_ode_bdf"
    , ReturnType (Array (Array Real))
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType (Array Real) ) )
      ; (GQuant, Array Real)
      ; (TData, Real)
      ; (TData, Array Real)
      ; (GQuant, Array Real)
      ; (TData, Array Real)
      ; (TData, Array Int) ] ) ;
  add_qualified
    ( "integrate_ode_bdf"
    , ReturnType (Array (Array Real))
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType (Array Real) ) )
      ; (GQuant, Array Real)
      ; (TData, Real)
      ; (TData, Array Real)
      ; (GQuant, Array Real)
      ; (TData, Array Real)
      ; (TData, Array Int)
      ; (TData, Real)
      ; (TData, Real)
      ; (TData, Real) ] ) ;
  add_qualified
    ( "integrate_ode_rk45"
    , ReturnType (Array (Array Real))
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType (Array Real) ) )
      ; (GQuant, Array Real)
      ; (TData, Real)
      ; (TData, Array Real)
      ; (GQuant, Array Real)
      ; (TData, Array Real)
      ; (TData, Array Int) ] ) ;
  add_qualified
    ( "integrate_ode_rk45"
    , ReturnType (Array (Array Real))
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType (Array Real) ) )
      ; (GQuant, Array Real)
      ; (TData, Real)
      ; (TData, Array Real)
      ; (GQuant, Array Real)
      ; (TData, Array Real)
      ; (TData, Array Int)
      ; (TData, Real)
      ; (TData, Real)
      ; (TData, Real) ] ) ;
  add_unary_vectorized "inv" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "inv_chi_square_ccdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_cdf"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_cdf_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_log"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_lccdf"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_lcdf"
        , ReturnType Real
        , [vector_types i; vector_types j] ) ;
      add_unqualified
        ( "inv_chi_square_lpdf"
        , ReturnType Real
        , [vector_types i; vector_types j] )
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("inv_chi_square_rng", ReturnType (rng_return_type Real [t]), [t]) ) ;
  add_unary_vectorized "inv_cloglog" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "inv_gamma_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "inv_gamma_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("inv_gamma_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  add_unary_vectorized "inv_logit" ;
  add_unary_vectorized "inv_Phi" ;
  add_unary_vectorized "inv_sqrt" ;
  add_unary_vectorized "inv_square" ;
  add_unqualified ("inv_wishart_log", ReturnType Real, [Matrix; Real; Matrix]) ;
  add_unqualified ("inv_wishart_lpdf", ReturnType Real, [Matrix; Real; Matrix]) ;
  add_unqualified ("inv_wishart_rng", ReturnType Matrix, [Real; Matrix]) ;
  add_unqualified ("inverse", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("inverse_spd", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("is_inf", ReturnType Int, [Real]) ;
  add_unqualified ("is_nan", ReturnType Int, [Real]) ;
  add_binary "lbeta" ;
  add_binary "lchoose" ;
  add_unary_vectorized "lgamma" ;
  add_unqualified ("lkj_corr_cholesky_log", ReturnType Real, [Matrix; Real]) ;
  add_unqualified ("lkj_corr_cholesky_lpdf", ReturnType Real, [Matrix; Real]) ;
  add_unqualified ("lkj_corr_cholesky_rng", ReturnType Matrix, [Int; Real]) ;
  add_unqualified ("lkj_corr_log", ReturnType Real, [Matrix; Real]) ;
  add_unqualified ("lkj_corr_lpdf", ReturnType Real, [Matrix; Real]) ;
  add_unqualified ("lkj_corr_rng", ReturnType Matrix, [Int; Real]) ;
  add_unqualified
    ("lkj_cov_log", ReturnType Real, [Matrix; Vector; Vector; Real]) ;
  add_unqualified ("lmgamma", ReturnType Real, [Int; Real]) ;
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
  add_unqualified ("log_determinant", ReturnType Real, [Matrix]) ;
  add_binary "log_diff_exp" ;
  add_binary "log_falling_factorial" ;
  add_ternary "log_mix" ;
  for i = 1 to vector_types_size - 1 do
    for j = 1 to vector_types_size - 1 do
      add_unqualified
        ("log_mix", ReturnType Real, [vector_types i; vector_types j])
    done ;
    add_unqualified
      ( "log_mix"
      , ReturnType Real
      , [vector_types i; bare_array_type (Vector, 1)] ) ;
    add_unqualified
      ( "log_mix"
      , ReturnType Real
      , [vector_types i; bare_array_type (RowVector, 1)] )
  done ;
  add_binary "log_rising_factorial" ;
  add_unary_vectorized "log_inv_logit" ;
  add_unqualified ("log_softmax", ReturnType Vector, [Vector]) ;
  add_unqualified ("log_sum_exp", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_unqualified ("log_sum_exp", ReturnType Real, [Vector]) ;
  add_unqualified ("log_sum_exp", ReturnType Real, [RowVector]) ;
  add_unqualified ("log_sum_exp", ReturnType Real, [Matrix]) ;
  add_binary "log_sum_exp" ;
  for i = 0 to primitive_types_size - 1 do
    add_unqualified ("logical_negation", ReturnType Int, [primitive_types i]) ;
    for j = 0 to primitive_types_size - 1 do
      add_unqualified
        ("logical_or", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_and", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_eq", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_neq", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_lt", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_lte", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_gt", ReturnType Int, [primitive_types i; primitive_types j]) ;
      add_unqualified
        ("logical_gte", ReturnType Int, [primitive_types i; primitive_types j])
    done
  done ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "logistic_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "logistic_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("logistic_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  add_unary_vectorized "logit" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "lognormal_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "lognormal_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("lognormal_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  add_nullary "machine_precision" ;
  add_qualified
    ( "map_rect"
    , ReturnType Vector
    , [ ( GQuant
        , Fun
            ( [ (GQuant, Vector)
              ; (GQuant, Vector)
              ; (GQuant, Array Real)
              ; (GQuant, Array Int) ]
            , ReturnType Vector ) )
      ; (GQuant, Vector)
      ; (GQuant, Array Vector)
      ; (TData, Array (Array Real))
      ; (TData, Array (Array Int)) ] ) ;
  add_unqualified ("matrix_exp", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("matrix_exp_multiply", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("max", ReturnType Int, [bare_array_type (Int, 1)]) ;
  add_unqualified ("max", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_unqualified ("max", ReturnType Real, [Vector]) ;
  add_unqualified ("max", ReturnType Real, [RowVector]) ;
  add_unqualified ("max", ReturnType Real, [Matrix]) ;
  add_unqualified ("max", ReturnType Int, [Int; Int]) ;
  add_unqualified ("mdivide_left", ReturnType Vector, [Matrix; Vector]) ;
  add_unqualified ("mdivide_left", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("mdivide_left_spd", ReturnType Vector, [Matrix; Vector]) ;
  add_unqualified ("mdivide_left_spd", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("mdivide_left_tri_low", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("mdivide_left_tri_low", ReturnType Vector, [Matrix; Vector]) ;
  add_unqualified ("mdivide_right", ReturnType RowVector, [RowVector; Matrix]) ;
  add_unqualified ("mdivide_right_spd", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified
    ("mdivide_right_spd", ReturnType RowVector, [RowVector; Matrix]) ;
  add_unqualified ("mdivide_right", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified
    ("mdivide_right_tri_low", ReturnType RowVector, [RowVector; Matrix]) ;
  add_unqualified ("mdivide_right_tri_low", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("mean", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_unqualified ("mean", ReturnType Real, [Vector]) ;
  add_unqualified ("mean", ReturnType Real, [RowVector]) ;
  add_unqualified ("mean", ReturnType Real, [Matrix]) ;
  add_unqualified ("min", ReturnType Int, [bare_array_type (Int, 1)]) ;
  add_unqualified ("min", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_unqualified ("min", ReturnType Real, [Vector]) ;
  add_unqualified ("min", ReturnType Real, [RowVector]) ;
  add_unqualified ("min", ReturnType Real, [Matrix]) ;
  add_unqualified ("min", ReturnType Int, [Int; Int]) ;
  add_unqualified ("minus", ReturnType Int, [Int]) ;
  add_unqualified ("minus", ReturnType Real, [Real]) ;
  add_unqualified ("minus", ReturnType Vector, [Vector]) ;
  add_unqualified ("minus", ReturnType RowVector, [RowVector]) ;
  add_unqualified ("minus", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("modified_bessel_first_kind", ReturnType Real, [Int; Real]) ;
  add_unqualified ("modified_bessel_second_kind", ReturnType Real, [Int; Real]) ;
  add_unqualified ("modulus", ReturnType Int, [Int; Int]) ;
  add_unqualified ("multi_gp_log", ReturnType Real, [Matrix; Matrix; Vector]) ;
  add_unqualified ("multi_gp_lpdf", ReturnType Real, [Matrix; Matrix; Vector]) ;
  add_unqualified
    ("multi_gp_cholesky_log", ReturnType Real, [Matrix; Matrix; Vector]) ;
  add_unqualified
    ("multi_gp_cholesky_lpdf", ReturnType Real, [Matrix; Matrix; Vector]) ;
  for k = 0 to 4 - 1 do
    for l = 0 to 4 - 1 do
      add_unqualified
        ( "multi_normal_cholesky_log"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_unqualified
        ( "multi_normal_cholesky_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_unqualified
        ( "multi_normal_log"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_unqualified
        ( "multi_normal_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_unqualified
        ( "multi_normal_prec_log"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_unqualified
        ( "multi_normal_prec_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; Matrix] ) ;
      add_unqualified
        ( "multi_student_t_log"
        , ReturnType Real
        , [eigen_vector_types k; Real; eigen_vector_types l; Matrix] ) ;
      add_unqualified
        ( "multi_student_t_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; Real; eigen_vector_types l; Matrix] )
    done
  done ;
  add_unqualified ("multi_normal_rng", ReturnType Vector, [Vector; Matrix]) ;
  add_unqualified
    ( "multi_normal_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [bare_array_type (Vector, 1); Matrix] ) ;
  add_unqualified ("multi_normal_rng", ReturnType Vector, [RowVector; Matrix]) ;
  add_unqualified
    ( "multi_normal_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [bare_array_type (RowVector, 1); Matrix] ) ;
  add_unqualified
    ("multi_normal_cholesky_rng", ReturnType Vector, [Vector; Matrix]) ;
  add_unqualified
    ( "multi_normal_cholesky_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [bare_array_type (Vector, 1); Matrix] ) ;
  add_unqualified
    ("multi_normal_cholesky_rng", ReturnType Vector, [RowVector; Matrix]) ;
  add_unqualified
    ( "multi_normal_cholesky_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [bare_array_type (RowVector, 1); Matrix] ) ;
  add_unqualified
    ("multi_student_t_rng", ReturnType Vector, [Real; Vector; Matrix]) ;
  add_unqualified
    ( "multi_student_t_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [Real; bare_array_type (Vector, 1); Matrix] ) ;
  add_unqualified
    ("multi_student_t_rng", ReturnType Vector, [Real; RowVector; Matrix]) ;
  add_unqualified
    ( "multi_student_t_rng"
    , ReturnType (bare_array_type (Vector, 1))
    , [Real; bare_array_type (RowVector, 1); Matrix] ) ;
  add_unqualified
    ("multinomial_log", ReturnType Real, [bare_array_type (Int, 1); Vector]) ;
  add_unqualified
    ("multinomial_lpmf", ReturnType Real, [bare_array_type (Int, 1); Vector]) ;
  add_unqualified
    ("multinomial_rng", ReturnType (bare_array_type (Int, 1)), [Vector; Int]) ;
  add_unqualified ("multiply", ReturnType Int, [Int; Int]) ;
  add_unqualified ("multiply", ReturnType Real, [Real; Real]) ;
  add_unqualified ("multiply", ReturnType Vector, [Vector; Real]) ;
  add_unqualified ("multiply", ReturnType RowVector, [RowVector; Real]) ;
  add_unqualified ("multiply", ReturnType Matrix, [Matrix; Real]) ;
  add_unqualified ("multiply", ReturnType Real, [RowVector; Vector]) ;
  add_unqualified ("multiply", ReturnType Matrix, [Vector; RowVector]) ;
  add_unqualified ("multiply", ReturnType Vector, [Matrix; Vector]) ;
  add_unqualified ("multiply", ReturnType RowVector, [RowVector; Matrix]) ;
  add_unqualified ("multiply", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("multiply", ReturnType Vector, [Real; Vector]) ;
  add_unqualified ("multiply", ReturnType RowVector, [Real; RowVector]) ;
  add_unqualified ("multiply", ReturnType Matrix, [Real; Matrix]) ;
  add_binary "multiply_log" ;
  add_unqualified
    ("multiply_lower_tri_self_transpose", ReturnType Matrix, [Matrix]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "neg_binomial_ccdf_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_cdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_cdf_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_lccdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_lcdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_lpmf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_ccdf_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_cdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_cdf_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_lccdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_lcdf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_lpmf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_log_log"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "neg_binomial_2_log_lpmf"
          , ReturnType Real
          , [int_vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "neg_binomial_rng"
            , ReturnType (rng_return_type Int [t; u])
            , [t; u] ) ) ) ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "neg_binomial_2_rng"
            , ReturnType (rng_return_type Int [t; u])
            , [t; u] ) ) ) ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "neg_binomial_2_log_rng"
            , ReturnType (rng_return_type Int [t; u])
            , [t; u] ) ) ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Real; Vector; Real] ) ;
  add_unqualified
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Vector; Vector; Real] ) ;
  add_nullary "negative_infinity" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "normal_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "normal_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("normal_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType Real
    , [Vector; Matrix; Real; Vector; Real] ) ;
  add_unqualified
    ( "normal_id_glm_lpdf"
    , ReturnType Real
    , [Vector; Matrix; Vector; Vector; Real] ) ;
  add_nullary "not_a_number" ;
  add_unqualified ("num_elements", ReturnType Int, [Matrix]) ;
  add_unqualified ("num_elements", ReturnType Int, [Vector]) ;
  add_unqualified ("num_elements", ReturnType Int, [RowVector]) ;
  for i = 1 to 10 - 1 do
    add_unqualified ("num_elements", ReturnType Int, [bare_array_type (Int, i)]) ;
    add_unqualified
      ("num_elements", ReturnType Int, [bare_array_type (Real, i)]) ;
    add_unqualified
      ("num_elements", ReturnType Int, [bare_array_type (Matrix, i)]) ;
    add_unqualified
      ("num_elements", ReturnType Int, [bare_array_type (RowVector, i)]) ;
    add_unqualified
      ("num_elements", ReturnType Int, [bare_array_type (Vector, i)])
  done ;
  add_unqualified ("ordered_logistic_log", ReturnType Real, [Int; Real; Vector]) ;
  add_unqualified
    ( "ordered_logistic_log"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; Vector] ) ;
  add_unqualified
    ( "ordered_logistic_log"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; bare_array_type (Vector, 1)] ) ;
  add_unqualified
    ("ordered_logistic_lpmf", ReturnType Real, [Int; Real; Vector]) ;
  add_unqualified
    ( "ordered_logistic_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; Vector] ) ;
  add_unqualified
    ( "ordered_logistic_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; bare_array_type (Vector, 1)] ) ;
  add_unqualified ("ordered_logistic_rng", ReturnType Int, [Real; Vector]) ;
  add_unqualified ("ordered_probit_log", ReturnType Real, [Int; Real; Vector]) ;
  add_unqualified
    ( "ordered_probit_log"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; Vector] ) ;
  add_unqualified
    ( "ordered_probit_log"
    , ReturnType Real
    , [bare_array_type (Int, 1); Vector; bare_array_type (Vector, 1)] ) ;
  add_unqualified ("ordered_probit_lpmf", ReturnType Real, [Int; Real; Vector]) ;
  add_unqualified
    ( "ordered_probit_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Real; Vector] ) ;
  add_unqualified
    ( "ordered_probit_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Real; bare_array_type (Vector, 1)] ) ;
  add_unqualified ("ordered_probit_rng", ReturnType Int, [Real; Vector]) ;
  add_binary "owens_t" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "pareto_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "pareto_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("pareto_rng", ReturnType (rng_return_type Real [t; u]), [t; u]) )
  ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_unqualified
            ( "pareto_type_2_ccdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_cdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_cdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_lccdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "pareto_type_2_lcdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
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
              add_unqualified
                ( "pareto_type_2_rng"
                , ReturnType (rng_return_type Real [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_unary_vectorized "Phi" ;
  add_unary_vectorized "Phi_approx" ;
  add_nullary "pi" ;
  add_unqualified ("plus", ReturnType Int, [Int]) ;
  add_unqualified ("plus", ReturnType Real, [Real]) ;
  add_unqualified ("plus", ReturnType Vector, [Vector]) ;
  add_unqualified ("plus", ReturnType RowVector, [RowVector]) ;
  add_unqualified ("plus", ReturnType Matrix, [Matrix]) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "poisson_ccdf_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ("poisson_cdf", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_unqualified
        ( "poisson_cdf_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ("poisson_log", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_unqualified
        ("poisson_lccdf", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_unqualified
        ("poisson_lcdf", ReturnType Real, [int_vector_types i; vector_types j]) ;
      add_unqualified
        ("poisson_lpmf", ReturnType Real, [int_vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified ("poisson_rng", ReturnType (rng_return_type Int [t]), [t])
  ) ;
  for i = 0 to int_vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ( "poisson_log_log"
        , ReturnType Real
        , [int_vector_types i; vector_types j] ) ;
      add_unqualified
        ( "poisson_log_lpmf"
        , ReturnType Real
        , [int_vector_types i; vector_types j] )
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("poisson_log_rng", ReturnType (rng_return_type Int [t]), [t]) ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Real; Vector] ) ;
  add_unqualified
    ( "poisson_log_glm_lpmf"
    , ReturnType Real
    , [bare_array_type (Int, 1); Matrix; Vector; Vector] ) ;
  add_nullary "positive_infinity" ;
  add_binary "pow" ;
  add_unqualified ("prod", ReturnType Int, [bare_array_type (Int, 1)]) ;
  add_unqualified ("prod", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_unqualified ("prod", ReturnType Real, [Vector]) ;
  add_unqualified ("prod", ReturnType Real, [RowVector]) ;
  add_unqualified ("prod", ReturnType Real, [Matrix]) ;
  add_unqualified ("quad_form", ReturnType Real, [Matrix; Vector]) ;
  add_unqualified ("quad_form", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("quad_form_sym", ReturnType Real, [Matrix; Vector]) ;
  add_unqualified ("quad_form_sym", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("quad_form_diag", ReturnType Matrix, [Matrix; Vector]) ;
  add_unqualified ("quad_form_diag", ReturnType Matrix, [Matrix; RowVector]) ;
  add_unqualified ("rank", ReturnType Int, [bare_array_type (Int, 1); Int]) ;
  add_unqualified ("rank", ReturnType Int, [bare_array_type (Real, 1); Int]) ;
  add_unqualified ("rank", ReturnType Int, [Vector; Int]) ;
  add_unqualified ("rank", ReturnType Int, [RowVector; Int]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      add_unqualified
        ("rayleigh_ccdf_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_cdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_cdf_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_log", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_lccdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_lcdf", ReturnType Real, [vector_types i; vector_types j]) ;
      add_unqualified
        ("rayleigh_lpdf", ReturnType Real, [vector_types i; vector_types j])
    done
  done ;
  for_all_vector_types (fun t ->
      add_unqualified
        ("rayleigh_rng", ReturnType (rng_return_type Real [t]), [t]) ) ;
  add_unqualified ("append_row", ReturnType Matrix, [Matrix; Matrix]) ;
  add_unqualified ("append_row", ReturnType Matrix, [RowVector; Matrix]) ;
  add_unqualified ("append_row", ReturnType Matrix, [Matrix; RowVector]) ;
  add_unqualified ("append_row", ReturnType Matrix, [RowVector; RowVector]) ;
  add_unqualified ("append_row", ReturnType Vector, [Vector; Vector]) ;
  add_unqualified ("append_row", ReturnType Vector, [Real; Vector]) ;
  add_unqualified ("append_row", ReturnType Vector, [Vector; Real]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ( "rep_array"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_types i; Int] ) ;
    add_unqualified
      ( "rep_array"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_types i; Int; Int] ) ;
    add_unqualified
      ( "rep_array"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_types i; Int; Int; Int] ) ;
    for j = 1 to 3 - 1 do
      add_unqualified
        ( "rep_array"
        , ReturnType (bare_array_type (bare_types i, j + 1))
        , [bare_array_type (bare_types i, j); Int] ) ;
      add_unqualified
        ( "rep_array"
        , ReturnType (bare_array_type (bare_types i, j + 2))
        , [bare_array_type (bare_types i, j); Int; Int] ) ;
      add_unqualified
        ( "rep_array"
        , ReturnType (bare_array_type (bare_types i, j + 3))
        , [bare_array_type (bare_types i, j); Int; Int; Int] )
    done
  done ;
  add_unqualified ("rep_matrix", ReturnType Matrix, [Real; Int; Int]) ;
  add_unqualified ("rep_matrix", ReturnType Matrix, [Vector; Int]) ;
  add_unqualified ("rep_matrix", ReturnType Matrix, [RowVector; Int]) ;
  add_unqualified ("rep_row_vector", ReturnType RowVector, [Real; Int]) ;
  add_unqualified ("rep_vector", ReturnType Vector, [Real; Int]) ;
  add_unqualified ("rising_factorial", ReturnType Real, [Real; Int]) ;
  add_unqualified ("rising_factorial", ReturnType Int, [Int; Int]) ;
  add_unary_vectorized "round" ;
  add_unqualified ("row", ReturnType RowVector, [Matrix; Int]) ;
  add_unqualified ("rows", ReturnType Int, [Vector]) ;
  add_unqualified ("rows", ReturnType Int, [RowVector]) ;
  add_unqualified ("rows", ReturnType Int, [Matrix]) ;
  add_unqualified ("rows_dot_product", ReturnType Vector, [Vector; Vector]) ;
  add_unqualified
    ("rows_dot_product", ReturnType Vector, [RowVector; RowVector]) ;
  add_unqualified ("rows_dot_product", ReturnType Vector, [Matrix; Matrix]) ;
  add_unqualified ("rows_dot_self", ReturnType Vector, [Vector]) ;
  add_unqualified ("rows_dot_self", ReturnType Vector, [RowVector]) ;
  add_unqualified ("rows_dot_self", ReturnType Vector, [Matrix]) ;
  add_unqualified
    ("scale_matrix_exp_multiply", ReturnType Matrix, [Real; Matrix; Matrix]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "scaled_inv_chi_square_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "scaled_inv_chi_square_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ( "scaled_inv_chi_square_rng"
            , ReturnType (rng_return_type Real [t; u])
            , [t; u] ) ) ) ;
  add_unqualified ("sd", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_unqualified ("sd", ReturnType Real, [Vector]) ;
  add_unqualified ("sd", ReturnType Real, [RowVector]) ;
  add_unqualified ("sd", ReturnType Real, [Matrix]) ;
  add_unqualified ("segment", ReturnType RowVector, [RowVector; Int; Int]) ;
  add_unqualified ("segment", ReturnType Vector, [Vector; Int; Int]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ( "segment"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_array_type (bare_types i, 1); Int; Int] ) ;
    add_unqualified
      ( "segment"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_array_type (bare_types i, 2); Int; Int] ) ;
    add_unqualified
      ( "segment"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_array_type (bare_types i, 3); Int; Int] )
  done ;
  add_unary_vectorized "sin" ;
  add_unqualified ("singular_values", ReturnType Vector, [Matrix]) ;
  add_unary_vectorized "sinh" ;
  for i = 1 to 8 - 1 do
    add_unqualified ("size", ReturnType Int, [bare_array_type (Int, i)]) ;
    add_unqualified ("size", ReturnType Int, [bare_array_type (Real, i)]) ;
    add_unqualified ("size", ReturnType Int, [bare_array_type (Vector, i)]) ;
    add_unqualified ("size", ReturnType Int, [bare_array_type (RowVector, i)]) ;
    add_unqualified ("size", ReturnType Int, [bare_array_type (Matrix, i)])
  done ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_unqualified
            ( "skew_normal_ccdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_cdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_cdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_lccdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "skew_normal_lcdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
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
              add_unqualified
                ( "skew_normal_rng"
                , ReturnType (rng_return_type Real [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_unqualified ("softmax", ReturnType Vector, [Vector]) ;
  add_unqualified
    ( "sort_asc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Int, 1)] ) ;
  add_unqualified
    ( "sort_asc"
    , ReturnType (bare_array_type (Real, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_unqualified ("sort_asc", ReturnType Vector, [Vector]) ;
  add_unqualified ("sort_asc", ReturnType RowVector, [RowVector]) ;
  add_unqualified
    ( "sort_desc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Int, 1)] ) ;
  add_unqualified
    ( "sort_desc"
    , ReturnType (bare_array_type (Real, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_unqualified ("sort_desc", ReturnType Vector, [Vector]) ;
  add_unqualified ("sort_desc", ReturnType RowVector, [RowVector]) ;
  add_unqualified
    ( "sort_indices_asc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Int, 1)] ) ;
  add_unqualified
    ( "sort_indices_asc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_unqualified
    ("sort_indices_asc", ReturnType (bare_array_type (Int, 1)), [Vector]) ;
  add_unqualified
    ("sort_indices_asc", ReturnType (bare_array_type (Int, 1)), [RowVector]) ;
  add_unqualified
    ( "sort_indices_desc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Int, 1)] ) ;
  add_unqualified
    ( "sort_indices_desc"
    , ReturnType (bare_array_type (Int, 1))
    , [bare_array_type (Real, 1)] ) ;
  add_unqualified
    ("sort_indices_desc", ReturnType (bare_array_type (Int, 1)), [Vector]) ;
  add_unqualified
    ("sort_indices_desc", ReturnType (bare_array_type (Int, 1)), [RowVector]) ;
  add_unqualified ("squared_distance", ReturnType Real, [Real; Real]) ;
  add_unqualified ("squared_distance", ReturnType Real, [Vector; Vector]) ;
  add_unqualified ("squared_distance", ReturnType Real, [RowVector; RowVector]) ;
  add_unqualified ("squared_distance", ReturnType Real, [Vector; RowVector]) ;
  add_unqualified ("squared_distance", ReturnType Real, [RowVector; Vector]) ;
  add_unary_vectorized "sqrt" ;
  add_nullary "sqrt2" ;
  add_unary_vectorized "square" ;
  for i = 0 to vector_types_size - 1 do
    add_unqualified ("std_normal_log", ReturnType Real, [vector_types i]) ;
    add_unqualified ("std_normal_lpdf", ReturnType Real, [vector_types i])
  done ;
  add_unary "step" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          add_unqualified
            ( "student_t_ccdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_cdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_cdf_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_log"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_lccdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
            ( "student_t_lcdf"
            , ReturnType Real
            , [vector_types i; vector_types j; vector_types k; vector_types l]
            ) ;
          add_unqualified
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
              add_unqualified
                ( "student_t_rng"
                , ReturnType (rng_return_type Real [t; u; v])
                , [t; u; v] ) ) ) ) ;
  add_unqualified ("sub_col", ReturnType Vector, [Matrix; Int; Int; Int]) ;
  add_unqualified ("sub_row", ReturnType RowVector, [Matrix; Int; Int; Int]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ("subtract", ReturnType (bare_types i), [bare_types i; bare_types i])
  done ;
  add_unqualified ("subtract", ReturnType Vector, [Vector; Real]) ;
  add_unqualified ("subtract", ReturnType RowVector, [RowVector; Real]) ;
  add_unqualified ("subtract", ReturnType Matrix, [Matrix; Real]) ;
  add_unqualified ("subtract", ReturnType Vector, [Real; Vector]) ;
  add_unqualified ("subtract", ReturnType RowVector, [Real; RowVector]) ;
  add_unqualified ("subtract", ReturnType Matrix, [Real; Matrix]) ;
  add_unqualified ("sum", ReturnType Int, [bare_array_type (Int, 1)]) ;
  add_unqualified ("sum", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_unqualified ("sum", ReturnType Real, [Vector]) ;
  add_unqualified ("sum", ReturnType Real, [RowVector]) ;
  add_unqualified ("sum", ReturnType Real, [Matrix]) ;
  add_unqualified ("tail", ReturnType RowVector, [RowVector; Int]) ;
  add_unqualified ("tail", ReturnType Vector, [Vector; Int]) ;
  for i = 0 to bare_types_size - 1 do
    add_unqualified
      ( "tail"
      , ReturnType (bare_array_type (bare_types i, 1))
      , [bare_array_type (bare_types i, 1); Int] ) ;
    add_unqualified
      ( "tail"
      , ReturnType (bare_array_type (bare_types i, 2))
      , [bare_array_type (bare_types i, 2); Int] ) ;
    add_unqualified
      ( "tail"
      , ReturnType (bare_array_type (bare_types i, 3))
      , [bare_array_type (bare_types i, 3); Int] )
  done ;
  add_unary_vectorized "tan" ;
  add_unary_vectorized "tanh" (* ; add_nullary ("target") *) ;
  add_unqualified ("tcrossprod", ReturnType Matrix, [Matrix]) ;
  add_unary_vectorized "tgamma" ;
  add_unqualified
    ("to_array_1d", ReturnType (bare_array_type (Real, 1)), [Matrix]) ;
  add_unqualified
    ("to_array_1d", ReturnType (bare_array_type (Real, 1)), [Vector]) ;
  add_unqualified
    ("to_array_1d", ReturnType (bare_array_type (Real, 1)), [RowVector]) ;
  for i = 1 to 10 - 1 do
    add_unqualified
      ( "to_array_1d"
      , ReturnType (bare_array_type (Real, 1))
      , [bare_array_type (Real, i)] ) ;
    add_unqualified
      ( "to_array_1d"
      , ReturnType (bare_array_type (Int, 1))
      , [bare_array_type (Int, i)] )
  done ;
  add_unqualified
    ("to_array_2d", ReturnType (bare_array_type (Real, 2)), [Matrix]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [Matrix]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [Matrix; Int; Int]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [Matrix; Int; Int; Int]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [Vector]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [Vector; Int; Int]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [Vector; Int; Int; Int]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [RowVector]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [RowVector; Int; Int]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [RowVector; Int; Int; Int]) ;
  add_unqualified
    ("to_matrix", ReturnType Matrix, [bare_array_type (Real, 1); Int; Int]) ;
  add_unqualified
    ("to_matrix", ReturnType Matrix, [bare_array_type (Real, 1); Int; Int; Int]) ;
  add_unqualified
    ("to_matrix", ReturnType Matrix, [bare_array_type (Int, 1); Int; Int]) ;
  add_unqualified
    ("to_matrix", ReturnType Matrix, [bare_array_type (Int, 1); Int; Int; Int]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [bare_array_type (Real, 2)]) ;
  add_unqualified ("to_matrix", ReturnType Matrix, [bare_array_type (Int, 2)]) ;
  add_unqualified ("to_row_vector", ReturnType RowVector, [Matrix]) ;
  add_unqualified ("to_row_vector", ReturnType RowVector, [Vector]) ;
  add_unqualified ("to_row_vector", ReturnType RowVector, [RowVector]) ;
  add_unqualified
    ("to_row_vector", ReturnType RowVector, [bare_array_type (Real, 1)]) ;
  add_unqualified
    ("to_row_vector", ReturnType RowVector, [bare_array_type (Int, 1)]) ;
  add_unqualified ("to_vector", ReturnType Vector, [Matrix]) ;
  add_unqualified ("to_vector", ReturnType Vector, [Vector]) ;
  add_unqualified ("to_vector", ReturnType Vector, [RowVector]) ;
  add_unqualified ("to_vector", ReturnType Vector, [bare_array_type (Real, 1)]) ;
  add_unqualified ("to_vector", ReturnType Vector, [bare_array_type (Int, 1)]) ;
  add_unqualified ("trace", ReturnType Real, [Matrix]) ;
  add_unqualified
    ("trace_gen_quad_form", ReturnType Real, [Matrix; Matrix; Matrix]) ;
  add_unqualified ("trace_quad_form", ReturnType Real, [Matrix; Vector]) ;
  add_unqualified ("trace_quad_form", ReturnType Real, [Matrix; Matrix]) ;
  add_unqualified ("transpose", ReturnType RowVector, [Vector]) ;
  add_unqualified ("transpose", ReturnType Vector, [RowVector]) ;
  add_unqualified ("transpose", ReturnType Matrix, [Matrix]) ;
  add_unary_vectorized "trunc" ;
  add_unary_vectorized "trigamma" ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "uniform_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "uniform_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("uniform_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  add_unqualified ("variance", ReturnType Real, [bare_array_type (Real, 1)]) ;
  add_unqualified ("variance", ReturnType Real, [Vector]) ;
  add_unqualified ("variance", ReturnType Real, [RowVector]) ;
  add_unqualified ("variance", ReturnType Real, [Matrix]) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "von_mises_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "von_mises_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("von_mises_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        add_unqualified
          ( "weibull_ccdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_cdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_cdf_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_log"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_lccdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_lcdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] ) ;
        add_unqualified
          ( "weibull_lpdf"
          , ReturnType Real
          , [vector_types i; vector_types j; vector_types k] )
      done
    done
  done ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_unqualified
            ("weibull_rng", ReturnType (rng_return_type Real [t; u]), [t; u])
      ) ) ;
  for i = 0 to vector_types_size - 1 do
    for j = 0 to vector_types_size - 1 do
      for k = 0 to vector_types_size - 1 do
        for l = 0 to vector_types_size - 1 do
          for m = 0 to vector_types_size - 1 do
            add_unqualified
              ( "wiener_log"
              , ReturnType Real
              , [ vector_types i
                ; vector_types j
                ; vector_types k
                ; vector_types l
                ; vector_types m ] ) ;
            add_unqualified
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
  add_unqualified ("wishart_log", ReturnType Real, [Matrix; Real; Matrix]) ;
  add_unqualified ("wishart_lpdf", ReturnType Real, [Matrix; Real; Matrix]) ;
  add_unqualified ("wishart_rng", ReturnType Matrix, [Real; Matrix])

let try_get_primitive_return_type name argtypes =
  let namematches = Hashtbl.find_all primitive_signatures name in
  let filteredmatches =
    List.filter
      (fun x -> check_compatible_arguments_mod_conv name (snd x) argtypes)
      namematches
  in
  if List.length filteredmatches = 0 then None
    (* We return the least return type in case there are multiple options (due to implicit Int-Real conversion), where Int<Real *)
  else
    Some
      (List.hd (List.sort compare_returntype (List.map fst filteredmatches)))

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

let _ = Hashtbl.add operator_names "UPlus" "plus"

let _ = Hashtbl.add operator_names "Transpose" "transpose"

let _ = Hashtbl.add operator_names "Conditional" "if_else"

let _ = Hashtbl.add operator_names "PlusAssign" "assign_add"

let _ = Hashtbl.add operator_names "MinusAssign" "assign_subtract"

let _ = Hashtbl.add operator_names "TimesAssign" "assign_multiply"

let _ = Hashtbl.add operator_names "DivideAssign" "assign_divide"

let _ = Hashtbl.add operator_names "EltTimesAssign" "assign_elt_times"

let _ = Hashtbl.add operator_names "EltDivideAssign" "assign_elt_divide"

let try_get_operator_return_type op_name argtypes =
  if op_name = "Assign" || op_name = "ArrowAssign" then
    match argtypes with
    | [(_, ut1); (_, ut2)] ->
        if check_of_same_type_mod_array_conv "" ut1 ut2 then Some Void
        else None
    | _ -> None
  else
    let rec try_recursive_find = function
      | [] -> None
      | name :: names -> (
        match try_get_primitive_return_type name argtypes with
        | None -> try_recursive_find names
        | Some ut -> Some ut )
    in
    try_recursive_find (Hashtbl.find_all operator_names op_name)
