// included from constructor for function_signatures () in src/stan/lang/ast.hpp

std::vector<bare_expr_type> bare_types
bare_types.push_back (Int)
bare_types.push_back (Real)
bare_types.push_back (Vector)
bare_types.push_back (RowVector)
bare_types.push_back (Matrix)

std::vector<bare_expr_type> vector_types
vector_types.push_back (Real)  // scalar
vector_types.push_back (bare_array_type (Real, 1))  // std vector
vector_types.push_back (Vector)  // Eigen vector
vector_types.push_back (RowVector)  // Eigen row vector

std::vector<bare_expr_type> int_vector_types
int_vector_types.push_back (Int)  // scalar
int_vector_types.push_back (bare_array_type (Int))  // std vector

std::vector<bare_expr_type> primitive_types
primitive_types.push_back (Int)
primitive_types.push_back (Real)

std::vector<bare_expr_type> all_vector_types
all_vector_types.push_back (bare_expr_type (Real))  // scalar
all_vector_types.push_back (bare_expr_type (bare_array_type (Real)))  // std vector
all_vector_types.push_back (bare_expr_type (Vector))  // Eigen vector
all_vector_types.push_back (bare_expr_type (RowVector))  // Eigen row vector
all_vector_types.push_back (bare_expr_type (Int))  // scalar
all_vector_types.push_back (bare_expr_type (bare_array_type (Int)))  // std vector

;  Hashtbl.add primitives_signatures ("abs", bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("abs", bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures_unary_vectorized ("acos")
;  Hashtbl.add primitives_signatures_unary_vectorized ("acosh")
for i = 0 to bare_types.size () do   (
  ;  Hashtbl.add primitives_signatures (";  Hashtbl.add primitives_signatures", bare_typesi, bare_typesi, bare_typesi)
 ) done
;  Hashtbl.add primitives_signatures (";  Hashtbl.add primitives_signatures", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures (";  Hashtbl.add primitives_signatures", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures (";  Hashtbl.add primitives_signatures", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures (";  Hashtbl.add primitives_signatures", bare_expr_type (Vector), bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures (";  Hashtbl.add primitives_signatures", bare_expr_type (RowVector), bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures (";  Hashtbl.add primitives_signatures", bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
for i = 0 to bare_types.size () do   (
  ;  Hashtbl.add primitives_signatures (";  Hashtbl.add primitives_signatures", bare_typesi, bare_typesi)
 ) done
for i = 1 to 8 do   (
  ;  Hashtbl.add primitives_signatures ("append_array", bare_expr_type (bare_array_type (Int, i)), bare_expr_type (bare_array_type (Int, i)), bare_expr_type (bare_array_type (Int, i)))
  ;  Hashtbl.add primitives_signatures ("append_array", bare_expr_type (bare_array_type (Real, i)), bare_expr_type (bare_array_type (Real, i)), bare_expr_type (bare_array_type (Real, i)))
  ;  Hashtbl.add primitives_signatures ("append_array", bare_expr_type (bare_array_type (Vector, i)), bare_expr_type (bare_array_type (Vector, i)), bare_expr_type (bare_array_type (Vector, i)))
  ;  Hashtbl.add primitives_signatures ("append_array", bare_expr_type (bare_array_type (RowVector, i)), bare_expr_type (bare_array_type (RowVector, i)), bare_expr_type (bare_array_type (RowVector, i)))
  ;  Hashtbl.add primitives_signatures ("append_array", bare_expr_type (bare_array_type (Matrix, i)), bare_expr_type (bare_array_type (Matrix, i)), bare_expr_type (bare_array_type (Matrix, i)))
 ) done
;  Hashtbl.add primitives_signatures_unary_vectorized ("asin")
;  Hashtbl.add primitives_signatures_unary_vectorized ("asinh")
;  Hashtbl.add primitives_signatures_unary_vectorized ("atan")
;  Hashtbl.add primitives_signatures_binary ("atan2")
;  Hashtbl.add primitives_signatures_unary_vectorized ("atanh")
for i = 0 to int_vector_types.size () do 
  for j = 0 to vector_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("bernoulli_ccdf_log", bare_expr_type (Real), int_vector_typesi,
	vector_typesj)
    ;  Hashtbl.add primitives_signatures ("bernoulli_cdf", bare_expr_type (Real), int_vector_typesi,
	vector_typesj)
    ;  Hashtbl.add primitives_signatures ("bernoulli_cdf_log", bare_expr_type (Real), int_vector_typesi,
	vector_typesj)
    ;  Hashtbl.add primitives_signatures ("bernoulli_log", bare_expr_type (Real), int_vector_typesi,
	vector_typesj)
    ;  Hashtbl.add primitives_signatures ("bernoulli_lccdf", bare_expr_type (Real), int_vector_typesi,
	vector_typesj)
    ;  Hashtbl.add primitives_signatures ("bernoulli_lcdf", bare_expr_type (Real), int_vector_typesi,
	vector_typesj)
    ;  Hashtbl.add primitives_signatures ("bernoulli_lpmf", bare_expr_type (Real), int_vector_typesi,
	vector_typesj)
  ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  ;  Hashtbl.add primitives_signatures ("bernoulli_rng", e (t), t)
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  ;  Hashtbl.add primitives_signatures ("bernoulli_logit_rng", e (t), t)
 ) done
for i = 0 to int_vector_types.size () do 
  for j = 0 to vector_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("bernoulli_logit_log", bare_expr_type (Real), int_vector_typesi,
	vector_typesj)
    ;  Hashtbl.add primitives_signatures ("bernoulli_logit_lpmf", bare_expr_type (Real), int_vector_typesi,
	vector_typesj)
  ) done
;  Hashtbl.add primitives_signatures ("bernoulli_logit_glm_lpmf",
    bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)),
    bare_expr_type (Matrix),
    bare_expr_type (Real),
    bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("bernoulli_logit_glm_lpmf",
    bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)),
    bare_expr_type (Matrix),
    bare_expr_type (Vector),
    bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("bessel_first_kind", bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("bessel_second_kind", bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Real))
for i = 0 to int_vector_types.size () do 
  for j = 0 to int_vector_types.size () do 
    for k = 0 to vector_types.size () do 
      for l = 0 to vector_types.size () do   (
        ;  Hashtbl.add primitives_signatures ("beta_binomial_ccdf_log", bare_expr_type (Real),
            int_vector_typesi, int_vector_typesj,
	    vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("beta_binomial_cdf", bare_expr_type (Real),
            int_vector_typesi, int_vector_typesj,
	    vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("beta_binomial_cdf_log", bare_expr_type (Real),
            int_vector_typesi, int_vector_typesj,
	    vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("beta_binomial_log", bare_expr_type (Real),
            int_vector_typesi, int_vector_typesj,
	    vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("beta_binomial_lccdf", bare_expr_type (Real), int_vector_typesi,
	    int_vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("beta_binomial_lcdf", bare_expr_type (Real),
            int_vector_typesi, int_vector_typesj,
	    vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("beta_binomial_lpmf", bare_expr_type (Real),
            int_vector_typesi, int_vector_typesj,
	    vector_typesk, vector_typesl)
      ) done
Core_kernel.List.map (int_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    Core_kernel.List.map (all_vector_types () )  (fun v -> 
      ;  Hashtbl.add primitives_signatures ("beta_binomial_rng", e (t, u, v), t, u, v)
    ) done
  ) done
 ) done
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("beta_ccdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("beta_cdf", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
      ;  Hashtbl.add primitives_signatures ("beta_cdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("beta_log", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
      ;  Hashtbl.add primitives_signatures ("beta_lccdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("beta_lcdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("beta_lpdf", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("beta_rng", e (t, u), t, u)
  ) done
 ) done
Core_kernel.List.map (vector_types () )  (fun t -> 
  Core_kernel.List.map (vector_types () )  (fun u -> 
    Core_kernel.List.map (all_vector_types () )  (fun v -> 
      ;  Hashtbl.add primitives_signatures ("beta_proportion_ccdf_log", bare_expr_type (Real), t, u, v)
      ;  Hashtbl.add primitives_signatures ("beta_proportion_cdf_log", bare_expr_type (Real), t, u, v)
      ;  Hashtbl.add primitives_signatures ("beta_proportion_log", bare_expr_type (Real), t, u, v)
      ;  Hashtbl.add primitives_signatures ("beta_proportion_lccdf", bare_expr_type (Real), t, u, v)
      ;  Hashtbl.add primitives_signatures ("beta_proportion_lcdf", bare_expr_type (Real), t, u, v)
      ;  Hashtbl.add primitives_signatures ("beta_proportion_lpdf", bare_expr_type (Real), t, u, v)
    ) done
  ) done
 ) done
Core_kernel.List.map (vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("beta_proportion_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("binary_log_loss", bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Real))
for i = 0 to int_vector_types.size () do   (
  for j = 0 to int_vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("binomial_ccdf_log", bare_expr_type (Real),
          int_vector_typesi, int_vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("binomial_cdf", bare_expr_type (Real),
          int_vector_typesi, int_vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("binomial_cdf_log", bare_expr_type (Real),
          int_vector_typesi, int_vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("binomial_log", bare_expr_type (Real),
          int_vector_typesi, int_vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("binomial_lccdf", bare_expr_type (Real),
          int_vector_typesi, int_vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("binomial_lcdf", bare_expr_type (Real),
          int_vector_typesi, int_vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("binomial_lpmf", bare_expr_type (Real),
          int_vector_typesi, int_vector_typesj, vector_typesk)
    ) done
  ) done
) done
Core_kernel.List.map (int_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("binomial_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures_binary ("binomial_coefficient_log")
for i = 0 to int_vector_types.size () do   (
  for j = 0 to int_vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("binomial_logit_log", bare_expr_type (Real),
          int_vector_typesi, int_vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("binomial_logit_lpmf", bare_expr_type (Real),
          int_vector_typesi, int_vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("block", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
for i = 0 to int_vector_types.size () do   (
  ;  Hashtbl.add primitives_signatures ("categorical_log", bare_expr_type (Real), int_vector_typesi, bare_expr_type (Vector))
  ;  Hashtbl.add primitives_signatures ("categorical_logit_log", bare_expr_type (Real), int_vector_typesi,
      bare_expr_type (Vector))
  ;  Hashtbl.add primitives_signatures ("categorical_lpmf", bare_expr_type (Real), int_vector_typesi, bare_expr_type (Vector))
  ;  Hashtbl.add primitives_signatures ("categorical_logit_lpmf", bare_expr_type (Real), int_vector_typesi,
      bare_expr_type (Vector))
 ) done
;  Hashtbl.add primitives_signatures ("categorical_rng", bare_expr_type (Int), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("categorical_logit_rng", bare_expr_type (Int), bare_expr_type (Vector))
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("cauchy_ccdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("cauchy_cdf", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
      ;  Hashtbl.add primitives_signatures ("cauchy_cdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("cauchy_log", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
      ;  Hashtbl.add primitives_signatures ("cauchy_lccdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("cauchy_lcdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("cauchy_lpdf", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("cauchy_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("append_col", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("append_col", bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("append_col", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("append_col", bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("append_col", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("append_col", bare_expr_type (RowVector), bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("append_col", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures_unary_vectorized ("cbrt")
;  Hashtbl.add primitives_signatures_unary_vectorized ("ceil")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("chi_square_ccdf_log", bare_expr_type (Real), vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("chi_square_cdf", bare_expr_type (Real), vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("chi_square_cdf_log", bare_expr_type (Real), vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("chi_square_log", bare_expr_type (Real), vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("chi_square_lccdf", bare_expr_type (Real), vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("chi_square_lcdf", bare_expr_type (Real), vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("chi_square_lpdf", bare_expr_type (Real), vector_typesi,
        vector_typesj)
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  ;  Hashtbl.add primitives_signatures ("chi_square_rng", e (t), t)
 ) done
;  Hashtbl.add primitives_signatures ("cholesky_decompose", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("choose", bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("col", bare_expr_type (Vector), bare_expr_type (Matrix), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("cols", bare_expr_type (Int), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("cols", bare_expr_type (Int), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("cols", bare_expr_type (Int), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("columns_dot_product", bare_expr_type (RowVector), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("columns_dot_product", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("columns_dot_product", bare_expr_type (RowVector), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("columns_dot_self", bare_expr_type (RowVector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("columns_dot_self", bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("columns_dot_self", bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_unary_vectorized ("cos")
;  Hashtbl.add primitives_signatures_unary_vectorized ("cosh")
;  Hashtbl.add primitives_signatures ("cov_exp_quad", bare_expr_type (Matrix), bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("cov_exp_quad", bare_expr_type (Matrix), bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("cov_exp_quad", bare_expr_type (Matrix), bare_expr_type (bare_array_type (RowVector, 1)), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("cov_exp_quad", bare_expr_type (Matrix),
    bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (bare_array_type (Real, 1)),bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("cov_exp_quad", bare_expr_type (Matrix), bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("cov_exp_quad", bare_expr_type (Matrix), bare_expr_type (bare_array_type (RowVector, 1)), bare_expr_type (bare_array_type (RowVector, 1)), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("crossprod", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("csr_matrix_times_vector", bare_expr_type (Vector), bare_expr_type (Int), bare_expr_type (Int),
    bare_expr_type (Vector), bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("csr_to_dense_matrix", bare_expr_type (Matrix), bare_expr_type (Int), bare_expr_type (Int),
    bare_expr_type (Vector), bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("csr_extract_w", bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("csr_extract_v", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("csr_extract_u", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("cumulative_sum", bare_expr_type (bare_array_type (Real, 1)),
    bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("cumulative_sum", bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("cumulative_sum", bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("determinant", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("diag_matrix", bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("diag_post_multiply", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("diag_post_multiply", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("diag_pre_multiply", bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("diag_pre_multiply", bare_expr_type (Matrix), bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("diagonal", bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_unary_vectorized ("digamma")

;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Matrix))

for i = 0 to 8 do   (
  ;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Int, i + 1)))
  ;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Real, i + 1)))
  ;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Vector, i + 1)))
  ;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (RowVector, i + 1)))
  ;  Hashtbl.add primitives_signatures ("dims", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Matrix, i + 1)))
 ) done

;  Hashtbl.add primitives_signatures ("dirichlet_log", bare_expr_type (Real), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("dirichlet_lpdf", bare_expr_type (Real), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("dirichlet_rng", bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("distance", bare_expr_type (Real), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("distance", bare_expr_type (Real), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("distance", bare_expr_type (Real), bare_expr_type (Vector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("distance", bare_expr_type (Real), bare_expr_type (RowVector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("divide", bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("divide", bare_expr_type (Real), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("divide", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("divide", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("divide", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("dot_product", bare_expr_type (Real), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("dot_product", bare_expr_type (Real), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("dot_product", bare_expr_type (Real), bare_expr_type (Vector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("dot_product", bare_expr_type (Real), bare_expr_type (RowVector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("dot_product", bare_expr_type (Real), bare_expr_type (bare_array_type (Real, 1)),
    bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("dot_self", bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("dot_self", bare_expr_type (Real), bare_expr_type (RowVector))
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("double_exponential_ccdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("double_exponential_cdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("double_exponential_cdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("double_exponential_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("double_exponential_lccdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("double_exponential_lcdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("double_exponential_lpdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("double_exponential_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures_nullary ("e")
;  Hashtbl.add primitives_signatures ("eigenvalues_sym", bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("eigenvectors_sym", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("qr_Q", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("qr_R", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("qr_thin_Q", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("qr_thin_R", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("elt_divide", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("elt_divide", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("elt_divide", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("elt_divide", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("elt_divide", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("elt_divide", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("elt_divide", bare_expr_type (Vector), bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("elt_divide", bare_expr_type (RowVector), bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("elt_divide", bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("elt_multiply", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("elt_multiply", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("elt_multiply", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_unary_vectorized ("erf")
;  Hashtbl.add primitives_signatures_unary_vectorized ("erfc")
;  Hashtbl.add primitives_signatures_unary_vectorized ("exp")
;  Hashtbl.add primitives_signatures_unary_vectorized ("exp2")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      for l = 0 to vector_types.size () do   (
        ;  Hashtbl.add primitives_signatures ("exp_mod_normal_ccdf_log", bare_expr_type (Real), vector_typesi,
	    vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("exp_mod_normal_cdf", bare_expr_type (Real), vector_typesi,
	    vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("exp_mod_normal_cdf_log", bare_expr_type (Real), vector_typesi,
	    vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("exp_mod_normal_log", bare_expr_type (Real), vector_typesi,
	    vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("exp_mod_normal_lccdf", bare_expr_type (Real), vector_typesi,
	    vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("exp_mod_normal_lcdf", bare_expr_type (Real), vector_typesi,
	    vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("exp_mod_normal_lpdf", bare_expr_type (Real), vector_typesi,
	    vector_typesj, vector_typesk, vector_typesl)
      ) done
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    Core_kernel.List.map (all_vector_types () )  (fun v -> 
      ;  Hashtbl.add primitives_signatures ("exp_mod_normal_rng", e (t, u, v), t, u, v)
    ) done
  ) done
 ) done
;  Hashtbl.add primitives_signatures_unary_vectorized ("expm1")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("exponential_ccdf_log", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("exponential_cdf", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("exponential_cdf_log", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("exponential_log", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("exponential_lccdf", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("exponential_lcdf", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("exponential_lpdf", bare_expr_type (Real), vector_typesi, vector_typesj)
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  ;  Hashtbl.add primitives_signatures ("exponential_rng", e (t), t)
 ) done
;  Hashtbl.add primitives_signatures_unary_vectorized ("fabs")
;  Hashtbl.add primitives_signatures ("falling_factorial", bare_expr_type (Real), bare_expr_type (Real), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("falling_factorial", bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures_binary ("fdim")
;  Hashtbl.add primitives_signatures_unary_vectorized ("floor")
;  Hashtbl.add primitives_signatures_ternary ("fma")
;  Hashtbl.add primitives_signatures_binary ("fmax")
;  Hashtbl.add primitives_signatures_binary ("fmin")
;  Hashtbl.add primitives_signatures_binary ("fmod")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("frechet_ccdf_log", bare_expr_type (Real), vector_typesi,
          vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("frechet_cdf", bare_expr_type (Real), vector_typesi,
          vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("frechet_cdf_log", bare_expr_type (Real), vector_typesi,
          vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("frechet_log", bare_expr_type (Real), vector_typesi,
          vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("frechet_lccdf", bare_expr_type (Real), vector_typesi,
          vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("frechet_lcdf", bare_expr_type (Real), vector_typesi,
          vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("frechet_lpdf", bare_expr_type (Real), vector_typesi,
          vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("frechet_rng", e (t, u), t, u)
  ) done
 ) done
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("gamma_ccdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gamma_cdf", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gamma_cdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gamma_log", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gamma_lccdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gamma_lcdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gamma_lpdf", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
    ) done
  ) done
 ) done
;  Hashtbl.add primitives_signatures_binary ("gamma_p")
;  Hashtbl.add primitives_signatures_binary ("gamma_q")
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("gamma_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("gaussian_dlm_obs_log", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix),
    bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("gaussian_dlm_obs_log", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix),
    bare_expr_type (Vector), bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("gaussian_dlm_obs_lpdf", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix),
    bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("gaussian_dlm_obs_lpdf", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix),
    bare_expr_type (Vector), bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_nullary ("get_lp")  // special handling in term_grammar_def
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("gumbel_ccdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gumbel_cdf", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gumbel_cdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gumbel_log", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gumbel_lccdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gumbel_lcdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("gumbel_lpdf", bare_expr_type (Real), vector_typesi, vector_typesj,
	  vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("gumbel_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("head", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("head", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Int))
for i = 0 to bare_types.size () do   (
  ;  Hashtbl.add primitives_signatures ("head", bare_expr_type (bare_array_type (bare_typesi, 1)),
      bare_expr_type (bare_array_type (bare_typesi, 1)), bare_expr_type (Int))
  ;  Hashtbl.add primitives_signatures ("head", bare_expr_type (bare_array_type (bare_typesi, 2)),
      bare_expr_type (bare_array_type (bare_typesi, 2)), bare_expr_type (Int))
  ;  Hashtbl.add primitives_signatures ("head", bare_expr_type (bare_array_type (bare_typesi, 3)),
      bare_expr_type (bare_array_type (bare_typesi, 3)), bare_expr_type (Int))
 ) done
;  Hashtbl.add primitives_signatures ("hypergeometric_log", bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("hypergeometric_lpmf", bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("hypergeometric_rng", bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures_binary ("hypot")
;  Hashtbl.add primitives_signatures ("if_else", bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("inc_beta", bare_expr_type (Real), bare_expr_type (Real), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("int_step", bare_expr_type (Int), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("int_step", bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures_unary_vectorized ("inv")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("inv_chi_square_ccdf_log", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("inv_chi_square_cdf", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("inv_chi_square_cdf_log", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("inv_chi_square_log", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("inv_chi_square_lccdf", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("inv_chi_square_lcdf", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("inv_chi_square_lpdf", bare_expr_type (Real), vector_typesi, vector_typesj)
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  ;  Hashtbl.add primitives_signatures ("inv_chi_square_rng", e (t), t)
 ) done
;  Hashtbl.add primitives_signatures_unary_vectorized ("inv_cloglog")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("inv_gamma_ccdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("inv_gamma_cdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("inv_gamma_cdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("inv_gamma_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("inv_gamma_lccdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("inv_gamma_lcdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("inv_gamma_lpdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("inv_gamma_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures_unary_vectorized ("inv_logit")
;  Hashtbl.add primitives_signatures_unary_vectorized ("inv_Phi")
;  Hashtbl.add primitives_signatures_unary_vectorized ("inv_sqrt")
;  Hashtbl.add primitives_signatures_unary_vectorized ("inv_square")
;  Hashtbl.add primitives_signatures ("inv_wishart_log", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("inv_wishart_lpdf", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("inv_wishart_rng", bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("inverse", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("inverse_spd", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("is_inf", bare_expr_type (Int), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("is_nan", bare_expr_type (Int), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures_binary ("lbeta")
;  Hashtbl.add primitives_signatures_binary ("lchoose")
;  Hashtbl.add primitives_signatures_unary_vectorized ("lgamma")
;  Hashtbl.add primitives_signatures ("lkj_corr_cholesky_log", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("lkj_corr_cholesky_lpdf", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("lkj_corr_cholesky_rng", bare_expr_type (Matrix), bare_expr_type (Int), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("lkj_corr_log", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("lkj_corr_lpdf", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("lkj_corr_rng", bare_expr_type (Matrix), bare_expr_type (Int), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("lkj_cov_log", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("lmgamma", bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures_binary ("lmultiply")
;  Hashtbl.add primitives_signatures_unary_vectorized ("log")
;  Hashtbl.add primitives_signatures_nullary ("log10")
;  Hashtbl.add primitives_signatures_unary_vectorized ("log10")
;  Hashtbl.add primitives_signatures_unary_vectorized ("log1m")
;  Hashtbl.add primitives_signatures_unary_vectorized ("log1m_exp")
;  Hashtbl.add primitives_signatures_unary_vectorized ("log1m_inv_logit")
;  Hashtbl.add primitives_signatures_unary_vectorized ("log1p")
;  Hashtbl.add primitives_signatures_unary_vectorized ("log1p_exp")
;  Hashtbl.add primitives_signatures_nullary ("log2")
;  Hashtbl.add primitives_signatures_unary_vectorized ("log2")
;  Hashtbl.add primitives_signatures ("log_determinant", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_binary ("log_diff_exp")
;  Hashtbl.add primitives_signatures_binary ("log_falling_factorial")
;  Hashtbl.add primitives_signatures_ternary ("log_mix")    // ;  Hashtbl.add primitives_signaturess fn over double, double, double
for i = 1 to vector_types.size () do   (
  for j = 1 to vector_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("log_mix", bare_expr_type (Real), bare_expr_type (vector_typesi), bare_expr_type (vector_typesj))
  ) done
  ;  Hashtbl.add primitives_signatures ("log_mix", bare_expr_type (Real), bare_expr_type (vector_typesi), bare_expr_type (bare_array_type (Vector, 1)))
  ;  Hashtbl.add primitives_signatures ("log_mix", bare_expr_type (Real), bare_expr_type (vector_typesi), bare_expr_type (bare_array_type (RowVector, 1)))
 ) done
;  Hashtbl.add primitives_signatures_binary ("log_rising_factorial")
;  Hashtbl.add primitives_signatures_unary_vectorized ("log_inv_logit")
;  Hashtbl.add primitives_signatures ("log_softmax", bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("log_sum_exp", bare_expr_type (Real), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("log_sum_exp", bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("log_sum_exp", bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("log_sum_exp", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_binary ("log_sum_exp")
for i = 0 to primitive_types.size () do   (
  ;  Hashtbl.add primitives_signatures ("logical_negation", bare_expr_type (Int), primitive_typesi)
  for j = 0 to primitive_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("logical_or", bare_expr_type (Int), primitive_typesi,
	primitive_typesj)
    ;  Hashtbl.add primitives_signatures ("logical_and", bare_expr_type (Int), primitive_typesi,
	primitive_typesj)
    ;  Hashtbl.add primitives_signatures ("logical_eq", bare_expr_type (Int), primitive_typesi,
	primitive_typesj)
    ;  Hashtbl.add primitives_signatures ("logical_neq", bare_expr_type (Int), primitive_typesi,
	primitive_typesj)
    ;  Hashtbl.add primitives_signatures ("logical_lt", bare_expr_type (Int), primitive_typesi,
	primitive_typesj)
    ;  Hashtbl.add primitives_signatures ("logical_lte", bare_expr_type (Int), primitive_typesi,
	primitive_typesj)
    ;  Hashtbl.add primitives_signatures ("logical_gt", bare_expr_type (Int), primitive_typesi,
	primitive_typesj)
    ;  Hashtbl.add primitives_signatures ("logical_gte", bare_expr_type (Int), primitive_typesi,
	primitive_typesj)
  ) done
 ) done
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("logistic_ccdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("logistic_cdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("logistic_cdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("logistic_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("logistic_lccdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("logistic_lcdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("logistic_lpdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("logistic_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures_unary_vectorized ("logit")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("lognormal_ccdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("lognormal_cdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("lognormal_cdf_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("lognormal_log", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("lognormal_lccdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("lognormal_lcdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("lognormal_lpdf", bare_expr_type (Real), vector_typesi,
	  vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("lognormal_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures_nullary ("machine_precision")
;  Hashtbl.add primitives_signatures ("matrix_exp", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("matrix_exp_multiply", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("max", bare_expr_type (Int), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("max", bare_expr_type (Real), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("max", bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("max", bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("max", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("max", bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("mdivide_left", bare_expr_type (Vector), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("mdivide_left", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("mdivide_left_spd", bare_expr_type (Vector), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("mdivide_left_spd", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("mdivide_left_tri_low", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("mdivide_left_tri_low", bare_expr_type (Vector), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("mdivide_right", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("mdivide_right_spd", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("mdivide_right_spd", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("mdivide_right", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("mdivide_right_tri_low", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("mdivide_right_tri_low", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("mean", bare_expr_type (Real), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("mean", bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("mean", bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("mean", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("min", bare_expr_type (Int), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("min", bare_expr_type (Real), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("min", bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("min", bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("min", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("min", bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("minus", bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("minus", bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("minus", bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("minus", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("modified_bessel_first_kind", bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("modified_bessel_second_kind", bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("modulus", bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("multi_gp_log", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("multi_gp_lpdf", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("multi_gp_cholesky_log", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("multi_gp_cholesky_lpdf", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Vector))
 (
  std::vector<bare_expr_type> eigen_vector_types
  eigen_vector_types.push_back (Vector)
  eigen_vector_types.push_back (bare_array_type (Vector))
  eigen_vector_types.push_back (RowVector)
  eigen_vector_types.push_back (bare_array_type (RowVector))
  for k = 0 to 4 do   (
    for l = 0 to 4 do   (
      ;  Hashtbl.add primitives_signatures ("multi_normal_cholesky_log", bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesk),
          bare_expr_type (eigen_vector_typesl), bare_expr_type (Matrix))

      ;  Hashtbl.add primitives_signatures ("multi_normal_cholesky_lpdf", bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesk),
          bare_expr_type (eigen_vector_typesl), bare_expr_type (Matrix))

      ;  Hashtbl.add primitives_signatures ("multi_normal_log", bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesk),
          bare_expr_type (eigen_vector_typesl), bare_expr_type (Matrix))

      ;  Hashtbl.add primitives_signatures ("multi_normal_lpdf", bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesk),
          bare_expr_type (eigen_vector_typesl), bare_expr_type (Matrix))

      ;  Hashtbl.add primitives_signatures ("multi_normal_prec_log", bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesk),
          bare_expr_type (eigen_vector_typesl), bare_expr_type (Matrix))

      ;  Hashtbl.add primitives_signatures ("multi_normal_prec_lpdf", bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesk),
          bare_expr_type (eigen_vector_typesl), bare_expr_type (Matrix))

      ;  Hashtbl.add primitives_signatures ("multi_student_t_log", bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesk), bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesl), bare_expr_type (Matrix))

      ;  Hashtbl.add primitives_signatures ("multi_student_t_lpdf", bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesk), bare_expr_type (Real),
          bare_expr_type (eigen_vector_typesl), bare_expr_type (Matrix))
    ) done
  ) done
) done
;  Hashtbl.add primitives_signatures ("multi_normal_rng", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multi_normal_rng", bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multi_normal_rng", bare_expr_type (Vector), bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multi_normal_rng", bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (bare_array_type (RowVector, 1)), bare_expr_type (Matrix))

;  Hashtbl.add primitives_signatures ("multi_normal_cholesky_rng", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multi_normal_cholesky_rng", bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multi_normal_cholesky_rng", bare_expr_type (Vector), bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multi_normal_cholesky_rng", bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (bare_array_type (RowVector, 1)), bare_expr_type (Matrix))

;  Hashtbl.add primitives_signatures ("multi_student_t_rng", bare_expr_type (Vector), bare_expr_type (Real), bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multi_student_t_rng", bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (Real), bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multi_student_t_rng", bare_expr_type (Vector), bare_expr_type (Real), bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multi_student_t_rng", bare_expr_type (bare_array_type (Vector, 1)), bare_expr_type (Real), bare_expr_type (bare_array_type (RowVector, 1)), bare_expr_type (Matrix))

;  Hashtbl.add primitives_signatures ("multinomial_log", bare_expr_type (Real), bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("multinomial_lpmf", bare_expr_type (Real), bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("multinomial_rng", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (Real), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (Real), bare_expr_type (RowVector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (Vector), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (Vector), bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (RowVector), bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("multiply", bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_binary ("multiply_log")
;  Hashtbl.add primitives_signatures ("multiply_lower_tri_self_transpose", bare_expr_type (Matrix), bare_expr_type (Matrix))
for i = 0 to int_vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("neg_binomial_ccdf_log", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_cdf", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_cdf_log", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_log", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_lccdf", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_lcdf", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_lpmf", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)

      ;  Hashtbl.add primitives_signatures ("neg_binomial_2_ccdf_log", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_2_cdf", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_2_cdf_log", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_2_log", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_2_lccdf", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_2_lcdf", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_2_lpmf", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)

      ;  Hashtbl.add primitives_signatures ("neg_binomial_2_log_log", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("neg_binomial_2_log_lpmf", bare_expr_type (Real),
          int_vector_typesi, vector_typesj, vector_typesk)
    ) done
  ) done
) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("neg_binomial_rng", e (t, u), t, u)
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("neg_binomial_2_rng", e (t, u), t, u)
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("neg_binomial_2_log_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("neg_binomial_2_log_glm_lpmf",
    bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)),
    bare_expr_type (Matrix),
    bare_expr_type (Real),
    bare_expr_type (Vector),
    bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("neg_binomial_2_log_glm_lpmf",
    bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)),
    bare_expr_type (Matrix),
    bare_expr_type (Vector),
    bare_expr_type (Vector),
    bare_expr_type (Real))
;  Hashtbl.add primitives_signatures_nullary ("negative_infinity")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("normal_ccdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("normal_cdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("normal_cdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("normal_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("normal_lccdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("normal_lcdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("normal_lpdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("normal_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("normal_id_glm_lpdf",
    bare_expr_type (Real),
    bare_expr_type (Vector),
    bare_expr_type (Matrix),
    bare_expr_type (Real),
    bare_expr_type (Vector),
    bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("normal_id_glm_lpdf",
    bare_expr_type (Real),
    bare_expr_type (Vector),
    bare_expr_type (Matrix),
    bare_expr_type (Vector),
    bare_expr_type (Vector),
    bare_expr_type (Real))
;  Hashtbl.add primitives_signatures_nullary ("not_a_number")
;  Hashtbl.add primitives_signatures ("num_elements", bare_expr_type (Int), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("num_elements", bare_expr_type (Int), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("num_elements", bare_expr_type (Int), bare_expr_type (RowVector))
for  (size_t i=1 i < 10 i++)  (
  ;  Hashtbl.add primitives_signatures ("num_elements", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (Int, i))))
  ;  Hashtbl.add primitives_signatures ("num_elements", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (Real, i))))
  ;  Hashtbl.add primitives_signatures ("num_elements", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (Matrix, i))))
  ;  Hashtbl.add primitives_signatures ("num_elements", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (RowVector, i))))
  ;  Hashtbl.add primitives_signatures ("num_elements", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (Vector, i))))
 ) done
;  Hashtbl.add primitives_signatures ("ordered_logistic_log", bare_expr_type (Real), bare_expr_type (Int),
    bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("ordered_logistic_log", bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector),
    bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("ordered_logistic_log", bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector),
    bare_expr_type (bare_array_type (Vector, 1)))

;  Hashtbl.add primitives_signatures ("ordered_logistic_lpmf", bare_expr_type (Real), bare_expr_type (Int),
    bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("ordered_logistic_lpmf", bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector),
    bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("ordered_logistic_lpmf", bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector),
    bare_expr_type (bare_array_type (Vector, 1)))

;  Hashtbl.add primitives_signatures ("ordered_logistic_rng", bare_expr_type (Int), bare_expr_type (Real),
    bare_expr_type (Vector))

;  Hashtbl.add primitives_signatures ("ordered_probit_log", bare_expr_type (Real), bare_expr_type (Int),
    bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("ordered_probit_log", bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector),
    bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("ordered_probit_log", bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector),
    bare_expr_type (bare_array_type (Vector, 1)))

;  Hashtbl.add primitives_signatures ("ordered_probit_lpmf", bare_expr_type (Real), bare_expr_type (Int),
    bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("ordered_probit_lpmf", bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Real),
    bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("ordered_probit_lpmf", bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Real),
    bare_expr_type (bare_array_type (Vector, 1)))

;  Hashtbl.add primitives_signatures ("ordered_probit_rng", bare_expr_type (Int), bare_expr_type (Real),
    bare_expr_type (Vector))


;  Hashtbl.add primitives_signatures_binary ("owens_t")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("pareto_ccdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("pareto_cdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("pareto_cdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("pareto_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("pareto_lccdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("pareto_lcdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("pareto_lpdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("pareto_rng", e (t, u), t, u)
  ) done
 ) done
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      for l = 0 to vector_types.size () do   (
        ;  Hashtbl.add primitives_signatures ("pareto_type_2_ccdf_log", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("pareto_type_2_cdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("pareto_type_2_cdf_log", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("pareto_type_2_log", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("pareto_type_2_lccdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("pareto_type_2_lcdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("pareto_type_2_lpdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
      ) done
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    Core_kernel.List.map (all_vector_types () )  (fun v -> 
      ;  Hashtbl.add primitives_signatures ("pareto_type_2_rng", e (t, u, v), t, u, v)
    ) done
  ) done
 ) done
;  Hashtbl.add primitives_signatures_unary_vectorized ("Phi")
;  Hashtbl.add primitives_signatures_unary_vectorized ("Phi_approx")
;  Hashtbl.add primitives_signatures_nullary ("pi")
for i = 0 to int_vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("poisson_ccdf_log", bare_expr_type (Real), int_vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("poisson_cdf", bare_expr_type (Real), int_vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("poisson_cdf_log", bare_expr_type (Real), int_vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("poisson_log", bare_expr_type (Real), int_vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("poisson_lccdf", bare_expr_type (Real), int_vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("poisson_lcdf", bare_expr_type (Real), int_vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("poisson_lpmf", bare_expr_type (Real), int_vector_typesi,
        vector_typesj)
  ) done
) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  ;  Hashtbl.add primitives_signatures ("poisson_rng", e (t), t)
 ) done
for i = 0 to int_vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("poisson_log_log", bare_expr_type (Real), int_vector_typesi,
        vector_typesj)
    ;  Hashtbl.add primitives_signatures ("poisson_log_lpmf", bare_expr_type (Real), int_vector_typesi,
        vector_typesj)
  ) done
) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  ;  Hashtbl.add primitives_signatures ("poisson_log_rng", e (t), t)
 ) done
;  Hashtbl.add primitives_signatures ("poisson_log_glm_lpmf",
    bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)),
    bare_expr_type (Matrix),
    bare_expr_type (Real),
    bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("poisson_log_glm_lpmf",
    bare_expr_type (Real),
    bare_expr_type (bare_array_type (Int, 1)),
    bare_expr_type (Matrix),
    bare_expr_type (Vector),
    bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures_nullary ("positive_infinity")
;  Hashtbl.add primitives_signatures_binary ("pow")
;  Hashtbl.add primitives_signatures ("prod", bare_expr_type (Int), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("prod", bare_expr_type (Real), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("prod", bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("prod", bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("prod", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("quad_form", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("quad_form", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("quad_form_sym", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("quad_form_sym", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("quad_form_diag", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("quad_form_diag", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("rank", bare_expr_type (Int), bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rank", bare_expr_type (Int), bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rank", bare_expr_type (Int), bare_expr_type (Vector), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rank", bare_expr_type (Int), bare_expr_type (RowVector), bare_expr_type (Int))
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    ;  Hashtbl.add primitives_signatures ("rayleigh_ccdf_log", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("rayleigh_cdf", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("rayleigh_cdf_log", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("rayleigh_log", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("rayleigh_lccdf", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("rayleigh_lcdf", bare_expr_type (Real), vector_typesi, vector_typesj)
    ;  Hashtbl.add primitives_signatures ("rayleigh_lpdf", bare_expr_type (Real), vector_typesi, vector_typesj)
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  ;  Hashtbl.add primitives_signatures ("rayleigh_rng", e (t), t)
 ) done
;  Hashtbl.add primitives_signatures ("append_row", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("append_row", bare_expr_type (Matrix), bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("append_row", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("append_row", bare_expr_type (Matrix), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("append_row", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("append_row", bare_expr_type (Vector), bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("append_row", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Real))
for i = 0 to bare_types.size () do   (
  ;  Hashtbl.add primitives_signatures ("rep_array", bare_expr_type (bare_array_type (bare_typesi, 1)), bare_typesi, bare_expr_type (Int))
  ;  Hashtbl.add primitives_signatures ("rep_array", bare_expr_type (bare_array_type (bare_typesi, 2)), bare_typesi, bare_expr_type (Int), bare_expr_type (Int))
  ;  Hashtbl.add primitives_signatures ("rep_array", bare_expr_type (bare_array_type (bare_typesi, 3)), bare_typesi,
      bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
  for  (size_t j = 1 j <= 3 ++j)  (
    ;  Hashtbl.add primitives_signatures ("rep_array", bare_expr_type (bare_array_type (bare_typesi, j + 1)),
        bare_expr_type (bare_array_type (bare_typesi, j)),  bare_expr_type (Int))
    ;  Hashtbl.add primitives_signatures ("rep_array", bare_expr_type (bare_array_type (bare_typesi, j + 2)),
        bare_expr_type (bare_array_type (bare_typesi, j)),  bare_expr_type (Int), bare_expr_type (Int))
    ;  Hashtbl.add primitives_signatures ("rep_array", bare_expr_type (bare_array_type (bare_typesi, j + 3)),
        bare_expr_type (bare_array_type (bare_typesi, j)),  bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("rep_matrix", bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rep_matrix", bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rep_matrix", bare_expr_type (Matrix), bare_expr_type (RowVector), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rep_row_vector", bare_expr_type (RowVector), bare_expr_type (Real), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rep_vector", bare_expr_type (Vector), bare_expr_type (Real), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rising_factorial", bare_expr_type (Real), bare_expr_type (Real), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rising_factorial", bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures_unary_vectorized ("round")
;  Hashtbl.add primitives_signatures ("row", bare_expr_type (RowVector), bare_expr_type (Matrix), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("rows", bare_expr_type (Int), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("rows", bare_expr_type (Int), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("rows", bare_expr_type (Int), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("rows_dot_product", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("rows_dot_product", bare_expr_type (Vector), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("rows_dot_product", bare_expr_type (Vector), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("rows_dot_self", bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("rows_dot_self", bare_expr_type (Vector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("rows_dot_self", bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("scale_matrix_exp_multiply", bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix))
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("scaled_inv_chi_square_ccdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("scaled_inv_chi_square_cdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("scaled_inv_chi_square_cdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("scaled_inv_chi_square_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("scaled_inv_chi_square_lccdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("scaled_inv_chi_square_lcdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("scaled_inv_chi_square_lpdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("scaled_inv_chi_square_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("sd", bare_expr_type (Real), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("sd", bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("sd", bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("sd", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("segment", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("segment", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Int), bare_expr_type (Int))
for i = 0 to bare_types.size () do   (
  ;  Hashtbl.add primitives_signatures ("segment", bare_expr_type (bare_array_type (bare_typesi, 1)),
      bare_expr_type (bare_array_type (bare_typesi, 1)), bare_expr_type (Int), bare_expr_type (Int))
  ;  Hashtbl.add primitives_signatures ("segment", bare_expr_type (bare_array_type (bare_typesi, 2)),
      bare_expr_type (bare_array_type (bare_typesi, 2)), bare_expr_type (Int), bare_expr_type (Int))
  ;  Hashtbl.add primitives_signatures ("segment", bare_expr_type (bare_array_type (bare_typesi, 3)),
      bare_expr_type (bare_array_type (bare_typesi, 3)), bare_expr_type (Int), bare_expr_type (Int))
 ) done
;  Hashtbl.add primitives_signatures_unary_vectorized ("sin")
;  Hashtbl.add primitives_signatures ("singular_values", bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_unary_vectorized ("sinh")
// size () is polymorphic over arrays, so start i at 1
for i = 1 to 8 do   (
  ;  Hashtbl.add primitives_signatures ("size", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (Int, i))))
  ;  Hashtbl.add primitives_signatures ("size", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (Real, i))))
  ;  Hashtbl.add primitives_signatures ("size", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (Vector, i))))
  ;  Hashtbl.add primitives_signatures ("size", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (RowVector, i))))
  ;  Hashtbl.add primitives_signatures ("size", bare_expr_type (Int), bare_expr_type (bare_array_type (bare_array_type (Matrix, i))))
 ) done
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      for l = 0 to vector_types.size () do   (
        ;  Hashtbl.add primitives_signatures ("skew_normal_ccdf_log", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("skew_normal_cdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("skew_normal_cdf_log", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("skew_normal_log", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("skew_normal_lccdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("skew_normal_lcdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("skew_normal_lpdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
      ) done
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    Core_kernel.List.map (all_vector_types () )  (fun v -> 
      ;  Hashtbl.add primitives_signatures ("skew_normal_rng", e (t, u, v), t, u, v)
    ) done
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("softmax", bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("sort_asc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("sort_asc", bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("sort_asc", bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("sort_asc", bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("sort_desc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("sort_desc", bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("sort_desc", bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("sort_desc", bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("sort_indices_asc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("sort_indices_asc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("sort_indices_asc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("sort_indices_asc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("sort_indices_desc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("sort_indices_desc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("sort_indices_desc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("sort_indices_desc", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("squared_distance", bare_expr_type (Real), bare_expr_type (Real), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("squared_distance", bare_expr_type (Real), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("squared_distance", bare_expr_type (Real), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("squared_distance", bare_expr_type (Real), bare_expr_type (Vector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("squared_distance", bare_expr_type (Real), bare_expr_type (RowVector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures_unary_vectorized ("sqrt")
;  Hashtbl.add primitives_signatures_nullary ("sqrt2")
;  Hashtbl.add primitives_signatures_unary_vectorized ("square")
for i = 0 to vector_types.size () do 
 (
  ;  Hashtbl.add primitives_signatures ("std_normal_log", bare_expr_type (Real), vector_typesi)
  ;  Hashtbl.add primitives_signatures ("std_normal_lpdf", bare_expr_type (Real), vector_typesi)
) done
;  Hashtbl.add primitives_signatures_unary ("step")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      for l = 0 to vector_types.size () do   (
        ;  Hashtbl.add primitives_signatures ("student_t_ccdf_log", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("student_t_cdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("student_t_cdf_log", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("student_t_log", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("student_t_lccdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("student_t_lcdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
        ;  Hashtbl.add primitives_signatures ("student_t_lpdf", bare_expr_type (Real), vector_typesi,
            vector_typesj, vector_typesk, vector_typesl)
      ) done
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    Core_kernel.List.map (all_vector_types () )  (fun v -> 
      ;  Hashtbl.add primitives_signatures ("student_t_rng", e (t, u, v), t, u, v)
    ) done
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("sub_col", bare_expr_type (Vector), bare_expr_type (Matrix), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("sub_row", bare_expr_type (RowVector), bare_expr_type (Matrix), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("subtract", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("subtract", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("subtract", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("subtract", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("subtract", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("subtract", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Real))
;  Hashtbl.add primitives_signatures ("subtract", bare_expr_type (Vector), bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("subtract", bare_expr_type (RowVector), bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("subtract", bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("sum", bare_expr_type (Int), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("sum", bare_expr_type (Real), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("sum", bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("sum", bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("sum", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("tail", bare_expr_type (RowVector), bare_expr_type (RowVector), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("tail", bare_expr_type (Vector), bare_expr_type (Vector), bare_expr_type (Int))
for i = 0 to bare_types.size () do   (
  ;  Hashtbl.add primitives_signatures ("tail", bare_expr_type (bare_array_type (bare_typesi, 1)),
      bare_expr_type (bare_array_type (bare_typesi, 1)), bare_expr_type (Int))
  ;  Hashtbl.add primitives_signatures ("tail", bare_expr_type (bare_array_type (bare_typesi, 2)),
      bare_expr_type (bare_array_type (bare_typesi, 2)), bare_expr_type (Int))
  ;  Hashtbl.add primitives_signatures ("tail", bare_expr_type (bare_array_type (bare_typesi, 3)),
      bare_expr_type (bare_array_type (bare_typesi, 3)), bare_expr_type (Int))
 ) done
;  Hashtbl.add primitives_signatures_unary_vectorized ("tan")
;  Hashtbl.add primitives_signatures_unary_vectorized ("tanh")
;  Hashtbl.add primitives_signatures_nullary ("target")  // converted to "get_lp" in term_grammar semantics
;  Hashtbl.add primitives_signatures ("tcrossprod", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_unary_vectorized ("tgamma")
;  Hashtbl.add primitives_signatures ("to_array_1d", bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("to_array_1d", bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("to_array_1d", bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (RowVector))
for  (size_t i=1 i < 10 i++)  (
  ;  Hashtbl.add primitives_signatures ("to_array_1d", bare_expr_type (bare_array_type (Real, 1)),
      bare_expr_type (bare_array_type (bare_array_type (Real, i))))
  ;  Hashtbl.add primitives_signatures ("to_array_1d", bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (bare_array_type (bare_array_type (Int, i))))
 ) done
;  Hashtbl.add primitives_signatures ("to_array_2d", bare_expr_type (bare_array_type (Real, 2)), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (Vector), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (RowVector), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (RowVector), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (bare_array_type (Real, 1)), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (bare_array_type (Int, 1)), bare_expr_type (Int), bare_expr_type (Int), bare_expr_type (Int))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (bare_array_type (Real, 2)))
;  Hashtbl.add primitives_signatures ("to_matrix", bare_expr_type (Matrix), bare_expr_type (bare_array_type (Int, 2)))
;  Hashtbl.add primitives_signatures ("to_row_vector", bare_expr_type (RowVector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("to_row_vector", bare_expr_type (RowVector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("to_row_vector", bare_expr_type (RowVector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("to_row_vector", bare_expr_type (RowVector), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("to_row_vector", bare_expr_type (RowVector), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("to_vector", bare_expr_type (Vector), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("to_vector", bare_expr_type (Vector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("to_vector", bare_expr_type (Vector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("to_vector", bare_expr_type (Vector), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("to_vector", bare_expr_type (Vector), bare_expr_type (bare_array_type (Int, 1)))
;  Hashtbl.add primitives_signatures ("trace", bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("trace_gen_quad_form", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("trace_quad_form", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("trace_quad_form", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("transpose", bare_expr_type (RowVector), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("transpose", bare_expr_type (Vector), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("transpose", bare_expr_type (Matrix), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures_unary_vectorized ("trunc")
;  Hashtbl.add primitives_signatures_unary_vectorized ("trigamma")
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("uniform_ccdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("uniform_cdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("uniform_cdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("uniform_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("uniform_lccdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("uniform_lcdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("uniform_lpdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("uniform_rng", e (t, u), t, u)
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("variance", bare_expr_type (Real), bare_expr_type (bare_array_type (Real, 1)))
;  Hashtbl.add primitives_signatures ("variance", bare_expr_type (Real), bare_expr_type (Vector))
;  Hashtbl.add primitives_signatures ("variance", bare_expr_type (Real), bare_expr_type (RowVector))
;  Hashtbl.add primitives_signatures ("variance", bare_expr_type (Real), bare_expr_type (Matrix))
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("von_mises_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("von_mises_lpdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
    ) done
  ) done
) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("von_mises_rng", e (t, u), t, u)
  ) done
 ) done
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      ;  Hashtbl.add primitives_signatures ("weibull_ccdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("weibull_cdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("weibull_cdf_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("weibull_log", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("weibull_lccdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("weibull_lcdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
      ;  Hashtbl.add primitives_signatures ("weibull_lpdf", bare_expr_type (Real),
          vector_typesi, vector_typesj, vector_typesk)
    ) done
  ) done
 ) done
Core_kernel.List.map (all_vector_types () )  (fun t -> 
  Core_kernel.List.map (all_vector_types () )  (fun u -> 
    ;  Hashtbl.add primitives_signatures ("weibull_rng", e (t, u), t, u)
  ) done
 ) done
for i = 0 to vector_types.size () do   (
  for j = 0 to vector_types.size () do   (
    for k = 0 to vector_types.size () do   (
      for l = 0 to vector_types.size () do   (
        for m = 0 to vector_types.size () do   (
          ;  Hashtbl.add primitives_signatures ("wiener_log", bare_expr_type (Real), vector_typesi,
              vector_typesj,vector_typesk, vector_typesl,
              vector_typesm)
          ;  Hashtbl.add primitives_signatures ("wiener_lpdf", bare_expr_type (Real), vector_typesi,
              vector_typesj,vector_typesk, vector_typesl,
              vector_typesm)
        ) done
      ) done
    ) done
  ) done
 ) done
;  Hashtbl.add primitives_signatures ("wishart_log", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("wishart_lpdf", bare_expr_type (Real), bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
;  Hashtbl.add primitives_signatures ("wishart_rng", bare_expr_type (Matrix), bare_expr_type (Real), bare_expr_type (Matrix))
