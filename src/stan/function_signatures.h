;
for_all_vector_types  (  fun t -> 
     add_plain ("bernoulli_rng",  (rng_return_type) (t), t) 
 ) 
for_all_vector_types  (  fun t -> 
    add_plain ("bernoulli_logit_rng",  (rng_return_type) (t), t) 
 )
; for i = 0 to int_vector_types_size do  
  for j = 0 to vector_types_size do    (
      add_plain ("bernoulli_logit_log",  ((ReturnType Real)), int_vector_types i ,
	vector_types j ) 
      ; add_plain ("bernoulli_logit_lpmf",  ((ReturnType Real)), int_vector_types i ,
	vector_types j ) 
  ) done
  ; add_plain ("bernoulli_logit_glm_lpmf",
     ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),
     ((ReturnType Matrix)),
     ((ReturnType Real)),
     ((ReturnType Vector))) 
  ; add_plain ("bernoulli_logit_glm_lpmf",
     ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),
     ((ReturnType Matrix)),
     ((ReturnType Vector)),
     ((ReturnType Vector))) 
  ; add_plain ("bessel_first_kind",  ((ReturnType Real)),  (ReturnType Int),  ((ReturnType Real))) 
  ; add_plain ("bessel_second_kind",  ((ReturnType Real)),  (ReturnType Int),  ((ReturnType Real))) 
; for i = 0 to int_vector_types_size do  
  for j = 0 to int_vector_types_size do  
     for k = 0 to vector_types_size do  
       for l = 0 to vector_types_size do    (
           add_plain ("beta_binomial_ccdf_log",  ((ReturnType Real)),
            int_vector_types i , int_vector_types j ,
	    vector_types k , vector_types l ) 
          ; add_plain ("beta_binomial_cdf",  ((ReturnType Real)),
            int_vector_types i , int_vector_types j ,
	    vector_types k , vector_types l ) 
          ; add_plain ("beta_binomial_cdf_log",  ((ReturnType Real)),
            int_vector_types i , int_vector_types j ,
	    vector_types k , vector_types l ) 
          ; add_plain ("beta_binomial_log",  ((ReturnType Real)),
            int_vector_types i , int_vector_types j ,
	    vector_types k , vector_types l ) 
          ; add_plain ("beta_binomial_lccdf",  ((ReturnType Real)), int_vector_types i ,
	    int_vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("beta_binomial_lcdf",  ((ReturnType Real)),
            int_vector_types i , int_vector_types j ,
	    vector_types k , vector_types l ) 
          ; add_plain ("beta_binomial_lpmf",  ((ReturnType Real)),
            int_vector_types i , int_vector_types j ,
	    vector_types k , vector_types l ) 
      ) done ;
Core_kernel.List.map  int_vector_types  (   fun t -> 
  for_all_vector_types  (  fun u -> 
    for_all_vector_types  (   fun v -> 
         add_plain ("beta_binomial_rng",  (rng_return_type) (t, u, v), t, u, v) 
    ) 
  ) 
 ) 
; for i = 0 to vector_types_size do    (
  for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("beta_ccdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("beta_cdf",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
        ; add_plain ("beta_cdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("beta_log",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
        ; add_plain ("beta_lccdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("beta_lcdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("beta_lpdf",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  (   fun t -> 
  for_all_vector_types  (   fun u -> 
       add_plain ("beta_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 )  ;
Core_kernel.List.map  vector_types  (   fun t -> 
  Core_kernel.List.map  vector_types  (   fun u -> 
    for_all_vector_types  (   fun v -> 
         add_plain ("beta_proportion_ccdf_log",  ((ReturnType Real)), t, u, v) 
        ; add_plain ("beta_proportion_cdf_log",  ((ReturnType Real)), t, u, v) 
        ; add_plain ("beta_proportion_log",  ((ReturnType Real)), t, u, v) 
        ; add_plain ("beta_proportion_lccdf",  ((ReturnType Real)), t, u, v) 
        ; add_plain ("beta_proportion_lcdf",  ((ReturnType Real)), t, u, v) 
        ; add_plain ("beta_proportion_lpdf",  ((ReturnType Real)), t, u, v) 
    ) 
  ) 
 ) ;
Core_kernel.List.map  vector_types  (   fun t -> 
  for_all_vector_types  (   fun u -> 
       add_plain ("beta_proportion_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_plain ("binary_log_loss",  ((ReturnType Real)),  (ReturnType Int),  ((ReturnType Real))) 
; for i = 0 to int_vector_types_size do    (
   for j = 0 to int_vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("binomial_ccdf_log",  ((ReturnType Real)),
          int_vector_types i , int_vector_types j , vector_types k ) 
        ; add_plain ("binomial_cdf",  ((ReturnType Real)),
          int_vector_types i , int_vector_types j , vector_types k ) 
        ; add_plain ("binomial_cdf_log",  ((ReturnType Real)),
          int_vector_types i , int_vector_types j , vector_types k ) 
        ; add_plain ("binomial_log",  ((ReturnType Real)),
          int_vector_types i , int_vector_types j , vector_types k ) 
        ; add_plain ("binomial_lccdf",  ((ReturnType Real)),
          int_vector_types i , int_vector_types j , vector_types k ) 
        ; add_plain ("binomial_lcdf",  ((ReturnType Real)),
          int_vector_types i , int_vector_types j , vector_types k ) 
        ; add_plain ("binomial_lpmf",  ((ReturnType Real)),
          int_vector_types i , int_vector_types j , vector_types k ) 
    ) done
  ) done
) done ;
Core_kernel.List.map  int_vector_types  (   fun t -> 
  for_all_vector_types  (   fun u -> 
      add_plain ("binomial_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_binary ("binomial_coefficient_log") 
; for i = 0 to int_vector_types_size do    (
   for j = 0 to int_vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("binomial_logit_log",  ((ReturnType Real)),
          int_vector_types i , int_vector_types j , vector_types k ) 
        ; add_plain ("binomial_logit_lpmf",  ((ReturnType Real)),
          int_vector_types i , int_vector_types j , vector_types k ) 
    ) done
  ) done
 ) done
  ; add_plain ("block",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
; for i = 0 to int_vector_types_size do    (
     add_plain ("categorical_log",  ((ReturnType Real)), int_vector_types i ,  ((ReturnType Vector))) 
    ; add_plain ("categorical_logit_log",  ((ReturnType Real)), int_vector_types i ,
       ((ReturnType Vector))) 
    ; add_plain ("categorical_lpmf",  ((ReturnType Real)), int_vector_types i ,  ((ReturnType Vector))) 
    ; add_plain ("categorical_logit_lpmf",  ((ReturnType Real)), int_vector_types i ,
       ((ReturnType Vector))) 
 ) done
  ; add_plain ("categorical_rng",  (ReturnType Int),  ((ReturnType Vector))) 
  ; add_plain ("categorical_logit_rng",  (ReturnType Int),  ((ReturnType Vector))) 
; for i = 0 to vector_types_size do    (
  for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("cauchy_ccdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("cauchy_cdf",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
        ; add_plain ("cauchy_cdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("cauchy_log",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
        ; add_plain ("cauchy_lccdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("cauchy_lcdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("cauchy_lpdf",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
      add_plain ("cauchy_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) ;
   add_plain ("append_col",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("append_col",  ((ReturnType Matrix)),  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("append_col",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("append_col",  ((ReturnType Matrix)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("append_col",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("append_col",  ((ReturnType RowVector)),  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("append_col",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Real))) 
  ; add_unary_vectorized ("cbrt") 
  ; add_unary_vectorized ("ceil") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
       add_plain ("chi_square_ccdf_log",  ((ReturnType Real)), vector_types i ,
        vector_types j ) 
      ; add_plain ("chi_square_cdf",  ((ReturnType Real)), vector_types i ,
        vector_types j ) 
      ; add_plain ("chi_square_cdf_log",  ((ReturnType Real)), vector_types i ,
        vector_types j ) 
      ; add_plain ("chi_square_log",  ((ReturnType Real)), vector_types i ,
        vector_types j ) 
      ; add_plain ("chi_square_lccdf",  ((ReturnType Real)), vector_types i ,
        vector_types j ) 
      ; add_plain ("chi_square_lcdf",  ((ReturnType Real)), vector_types i ,
        vector_types j ) 
      ; add_plain ("chi_square_lpdf",  ((ReturnType Real)), vector_types i ,
        vector_types j ) 
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
     add_plain ("chi_square_rng",  (rng_return_type) (t), t) 
 ) done
  ; add_plain ("cholesky_decompose",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("choose",  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("col",  ((ReturnType Vector)),  ((ReturnType Matrix)),  (ReturnType Int)) 
  ; add_plain ("cols",  (ReturnType Int),  ((ReturnType Vector))) 
  ; add_plain ("cols",  (ReturnType Int),  ((ReturnType RowVector))) 
  ; add_plain ("cols",  (ReturnType Int),  ((ReturnType Matrix))) 
  ; add_plain ("columns_dot_product",  ((ReturnType RowVector)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("columns_dot_product",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("columns_dot_product",  ((ReturnType RowVector)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("columns_dot_self",  ((ReturnType RowVector)),  ((ReturnType Vector))) 
  ; add_plain ("columns_dot_self",  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("columns_dot_self",  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_unary_vectorized ("cos") 
  ; add_unary_vectorized ("cosh") 
  ; add_plain ("cov_exp_quad",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType Real), 1)),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("cov_exp_quad",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType Vector), 1)),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("cov_exp_quad",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType RowVector), 1)),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("cov_exp_quad",  ((ReturnType Matrix)),
     (bare_array_type ((ReturnType Real), 1)),  (bare_array_type ((ReturnType Real), 1)), ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("cov_exp_quad",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType Vector), 1)),  (bare_array_type ((ReturnType Vector), 1)),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("cov_exp_quad",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType RowVector), 1)),  (bare_array_type ((ReturnType RowVector), 1)),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("crossprod",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("csr_matrix_times_vector",  ((ReturnType Vector)),  (ReturnType Int),  (ReturnType Int),
     ((ReturnType Vector)),  (bare_array_type (ReturnType Int, 1)),  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector))) 
  ; add_plain ("csr_to_dense_matrix",  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int),
     ((ReturnType Vector)),  (bare_array_type (ReturnType Int, 1)),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("csr_extract_w",  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("csr_extract_v",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Matrix))) 
  ; add_plain ("csr_extract_u",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Matrix))) 
  ; add_plain ("cumulative_sum",  (bare_array_type ((ReturnType Real), 1)),
     (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("cumulative_sum",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("cumulative_sum",  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("determinant",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("diag_matrix",  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("diag_post_multiply",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("diag_post_multiply",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType RowVector))) 
  ; add_plain ("diag_pre_multiply",  ((ReturnType Matrix)),  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("diag_pre_multiply",  ((ReturnType Matrix)),  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("diagonal",  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_unary_vectorized ("digamma") 

  ; add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  (ReturnType Int)) 
  ; add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Real))) 
  ; add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector))) 
  ; add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType RowVector))) 
  ; add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Matrix))) 

; for i = 0 to 8 do    (
     add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type (ReturnType Int, i + 1))) 
    ; add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type ((ReturnType Real), i + 1))) 
    ; add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type ((ReturnType Vector), i + 1))) 
    ; add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type ((ReturnType RowVector), i + 1))) 
    ; add_plain ("dims",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type ((ReturnType Matrix), i + 1))) 
 ) done

  ; add_plain ("dirichlet_log",  ((ReturnType Real)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("dirichlet_lpdf",  ((ReturnType Real)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("dirichlet_rng",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("distance",  ((ReturnType Real)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("distance",  ((ReturnType Real)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("distance",  ((ReturnType Real)),  ((ReturnType Vector)),  ((ReturnType RowVector))) 
  ; add_plain ("distance",  ((ReturnType Real)),  ((ReturnType RowVector)),  ((ReturnType Vector))) 
  ; add_plain ("divide",  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("divide",  ((ReturnType Real)),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("divide",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Real))) 
  ; add_plain ("divide",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Real))) 
  ; add_plain ("divide",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Real))) 
  ; add_plain ("dot_product",  ((ReturnType Real)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("dot_product",  ((ReturnType Real)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("dot_product",  ((ReturnType Real)),  ((ReturnType Vector)),  ((ReturnType RowVector))) 
  ; add_plain ("dot_product",  ((ReturnType Real)),  ((ReturnType RowVector)),  ((ReturnType Vector))) 
  ; add_plain ("dot_product",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1)),
     (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("dot_self",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("dot_self",  ((ReturnType Real)),  ((ReturnType RowVector))) 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("double_exponential_ccdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("double_exponential_cdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("double_exponential_cdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("double_exponential_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("double_exponential_lccdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("double_exponential_lcdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("double_exponential_lpdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
    ) done
  ) done
 ) done
; for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("double_exponential_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_nullary ("e") 
  ; add_plain ("eigenvalues_sym",  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("eigenvectors_sym",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("qr_Q",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("qr_R",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("qr_thin_Q",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("qr_thin_R",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("elt_divide",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("elt_divide",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("elt_divide",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("elt_divide",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Real))) 
  ; add_plain ("elt_divide",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Real))) 
  ; add_plain ("elt_divide",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Real))) 
  ; add_plain ("elt_divide",  ((ReturnType Vector)),  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("elt_divide",  ((ReturnType RowVector)),  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("elt_divide",  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("elt_multiply",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("elt_multiply",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("elt_multiply",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_unary_vectorized ("erf") 
  ; add_unary_vectorized ("erfc") 
  ; add_unary_vectorized ("exp") 
  ; add_unary_vectorized ("exp2") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
       for l = 0 to vector_types_size do    (
           add_plain ("exp_mod_normal_ccdf_log",  ((ReturnType Real)), vector_types i ,
	    vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("exp_mod_normal_cdf",  ((ReturnType Real)), vector_types i ,
	    vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("exp_mod_normal_cdf_log",  ((ReturnType Real)), vector_types i ,
	    vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("exp_mod_normal_log",  ((ReturnType Real)), vector_types i ,
	    vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("exp_mod_normal_lccdf",  ((ReturnType Real)), vector_types i ,
	    vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("exp_mod_normal_lcdf",  ((ReturnType Real)), vector_types i ,
	    vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("exp_mod_normal_lpdf",  ((ReturnType Real)), vector_types i ,
	    vector_types j , vector_types k , vector_types l ) 
      ) done
    ) done
  ) done
 ) done
; for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
    for_all_vector_types  ( fun v -> 
         add_plain ("exp_mod_normal_rng",  (rng_return_type) (t, u, v), t, u, v) 
    ) 
  ) 
 ) 
  ; add_unary_vectorized ("expm1") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
       add_plain ("exponential_ccdf_log",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("exponential_cdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("exponential_cdf_log",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("exponential_log",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("exponential_lccdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("exponential_lcdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("exponential_lpdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
  ) done
 ) done
; for_all_vector_types  ( fun t -> 
     add_plain ("exponential_rng",  (rng_return_type) (t), t) 
 ) 
  ; add_unary_vectorized ("fabs") 
  ; add_plain ("falling_factorial",  ((ReturnType Real)),  ((ReturnType Real)),  (ReturnType Int)) 
  ; add_plain ("falling_factorial",  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_binary ("fdim") 
  ; add_unary_vectorized ("floor") 
  ; add_ternary ("fma") 
  ; add_binary ("fmax") 
  ; add_binary ("fmin") 
  ; add_binary ("fmod") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("frechet_ccdf_log",  ((ReturnType Real)), vector_types i ,
          vector_types j , vector_types k ) 
        ; add_plain ("frechet_cdf",  ((ReturnType Real)), vector_types i ,
          vector_types j , vector_types k ) 
        ; add_plain ("frechet_cdf_log",  ((ReturnType Real)), vector_types i ,
          vector_types j , vector_types k ) 
        ; add_plain ("frechet_log",  ((ReturnType Real)), vector_types i ,
          vector_types j , vector_types k ) 
        ; add_plain ("frechet_lccdf",  ((ReturnType Real)), vector_types i ,
          vector_types j , vector_types k ) 
        ; add_plain ("frechet_lcdf",  ((ReturnType Real)), vector_types i ,
          vector_types j , vector_types k ) 
        ; add_plain ("frechet_lpdf",  ((ReturnType Real)), vector_types i ,
          vector_types j , vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("frechet_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("gamma_ccdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("gamma_cdf",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
        ; add_plain ("gamma_cdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("gamma_log",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
        ; add_plain ("gamma_lccdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("gamma_lcdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("gamma_lpdf",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
    ) done
  ) done
 ) done
  ; add_binary ("gamma_p") 
  ; add_binary ("gamma_q") 
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("gamma_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_plain ("gaussian_dlm_obs_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),
     ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("gaussian_dlm_obs_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),
     ((ReturnType Vector)),  ((ReturnType Matrix)),  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("gaussian_dlm_obs_lpdf",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),
     ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("gaussian_dlm_obs_lpdf",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),
     ((ReturnType Vector)),  ((ReturnType Matrix)),  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  (* ; add_nullary ("get_lp")   *)
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("gumbel_ccdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("gumbel_cdf",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
        ; add_plain ("gumbel_cdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("gumbel_log",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
        ; add_plain ("gumbel_lccdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("gumbel_lcdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("gumbel_lpdf",  ((ReturnType Real)), vector_types i , vector_types j ,
	  vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("gumbel_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_plain ("head",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  (ReturnType Int)) 
  ; add_plain ("head",  ((ReturnType Vector)),  ((ReturnType Vector)),  (ReturnType Int)) 
; for i = 0 to bare_types_size do    (
     add_plain ("head",  (bare_array_type (bare_types i , 1)),
       (bare_array_type (bare_types i , 1)),  (ReturnType Int)) 
    ; add_plain ("head",  (bare_array_type (bare_types i , 2)),
       (bare_array_type (bare_types i , 2)),  (ReturnType Int)) 
    ; add_plain ("head",  (bare_array_type (bare_types i , 3)),
       (bare_array_type (bare_types i , 3)),  (ReturnType Int)) 
 ) done
  ; add_plain ("hypergeometric_log",  ((ReturnType Real)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("hypergeometric_lpmf",  ((ReturnType Real)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("hypergeometric_rng",  (ReturnType Int),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_binary ("hypot") 
  ; add_plain ("if_else",  ((ReturnType Real)),  (ReturnType Int),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("inc_beta",  ((ReturnType Real)),  ((ReturnType Real)),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("int_step",  (ReturnType Int),  ((ReturnType Real))) 
  ; add_plain ("int_step",  (ReturnType Int),  (ReturnType Int)) 
  ; add_unary_vectorized ("inv") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
       add_plain ("inv_chi_square_ccdf_log",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("inv_chi_square_cdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("inv_chi_square_cdf_log",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("inv_chi_square_log",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("inv_chi_square_lccdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("inv_chi_square_lcdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("inv_chi_square_lpdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
  ) done
 ) done
; for_all_vector_types  ( fun t -> 
    add_plain ("inv_chi_square_rng",  (rng_return_type) (t), t) 
 ) 
  ; add_unary_vectorized ("inv_cloglog") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("inv_gamma_ccdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("inv_gamma_cdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("inv_gamma_cdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("inv_gamma_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("inv_gamma_lccdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("inv_gamma_lcdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("inv_gamma_lpdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("inv_gamma_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_unary_vectorized ("inv_logit") 
  ; add_unary_vectorized ("inv_Phi") 
  ; add_unary_vectorized ("inv_sqrt") 
  ; add_unary_vectorized ("inv_square") 
  ; add_plain ("inv_wishart_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("inv_wishart_lpdf",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("inv_wishart_rng",  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("inverse",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("inverse_spd",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("is_inf",  (ReturnType Int),  ((ReturnType Real))) 
  ; add_plain ("is_nan",  (ReturnType Int),  ((ReturnType Real))) 
  ; add_binary ("lbeta") 
  ; add_binary ("lchoose") 
  ; add_unary_vectorized ("lgamma") 
  ; add_plain ("lkj_corr_cholesky_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real))) 
  ; add_plain ("lkj_corr_cholesky_lpdf",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real))) 
  ; add_plain ("lkj_corr_cholesky_rng",  ((ReturnType Matrix)),  (ReturnType Int),  ((ReturnType Real))) 
  ; add_plain ("lkj_corr_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real))) 
  ; add_plain ("lkj_corr_lpdf",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real))) 
  ; add_plain ("lkj_corr_rng",  ((ReturnType Matrix)),  (ReturnType Int),  ((ReturnType Real))) 
  ; add_plain ("lkj_cov_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Real))) 
  ; add_plain ("lmgamma",  ((ReturnType Real)),  (ReturnType Int),  ((ReturnType Real))) 
  ; add_binary ("lmultiply") 
  ; add_unary_vectorized ("log") 
  ; add_nullary ("log10") 
  ; add_unary_vectorized ("log10") 
  ; add_unary_vectorized ("log1m") 
  ; add_unary_vectorized ("log1m_exp") 
  ; add_unary_vectorized ("log1m_inv_logit") 
  ; add_unary_vectorized ("log1p") 
  ; add_unary_vectorized ("log1p_exp") 
  ; add_nullary ("log2") 
  ; add_unary_vectorized ("log2") 
  ; add_plain ("log_determinant",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_binary ("log_diff_exp") 
  ; add_binary ("log_falling_factorial") 
   add_ternary ("log_mix")   
; for i = 1 to vector_types_size do    (
   for j = 1 to vector_types_size do    (
      add_plain ("log_mix",  ((ReturnType Real)),  (vector_types i ),  (vector_types j )) 
  ) done
    ; add_plain ("log_mix",  ((ReturnType Real)),  (vector_types i ),  (bare_array_type ((ReturnType Vector), 1))) 
    ; add_plain ("log_mix",  ((ReturnType Real)),  (vector_types i ),  (bare_array_type ((ReturnType RowVector), 1))) 
 ) done
  ; add_binary ("log_rising_factorial") 
  ; add_unary_vectorized ("log_inv_logit") 
  ; add_plain ("log_softmax",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("log_sum_exp",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("log_sum_exp",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("log_sum_exp",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("log_sum_exp",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_binary ("log_sum_exp") 
; for i = 0 to primitive_types.size () do    (
     add_plain ("logical_negation",  (ReturnType Int), primitive_types i ) 
  ; for j = 0 to primitive_types.size () do    (
       add_plain ("logical_or",  (ReturnType Int), primitive_types i ,
	primitive_types j ) 
      ; add_plain ("logical_and",  (ReturnType Int), primitive_types i ,
	primitive_types j ) 
      ; add_plain ("logical_eq",  (ReturnType Int), primitive_types i ,
	primitive_types j ) 
      ; add_plain ("logical_neq",  (ReturnType Int), primitive_types i ,
	primitive_types j ) 
      ; add_plain ("logical_lt",  (ReturnType Int), primitive_types i ,
	primitive_types j ) 
      ; add_plain ("logical_lte",  (ReturnType Int), primitive_types i ,
	primitive_types j ) 
      ; add_plain ("logical_gt",  (ReturnType Int), primitive_types i ,
	primitive_types j ) 
      ; add_plain ("logical_gte",  (ReturnType Int), primitive_types i ,
	primitive_types j ) 
  ) done
 ) done
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("logistic_ccdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("logistic_cdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("logistic_cdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("logistic_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("logistic_lccdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("logistic_lcdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("logistic_lpdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
      add_plain ("logistic_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_unary_vectorized ("logit") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("lognormal_ccdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("lognormal_cdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("lognormal_cdf_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("lognormal_log",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("lognormal_lccdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("lognormal_lcdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
        ; add_plain ("lognormal_lpdf",  ((ReturnType Real)), vector_types i ,
	  vector_types j , vector_types k ) 
    ) done
  ) done
 ) done
; for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("lognormal_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_nullary ("machine_precision") 
  ; add_plain ("matrix_exp",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("matrix_exp_multiply",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("max",  (ReturnType Int),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("max",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("max",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("max",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("max",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("max",  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("mdivide_left",  ((ReturnType Vector)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("mdivide_left",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("mdivide_left_spd",  ((ReturnType Vector)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("mdivide_left_spd",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("mdivide_left_tri_low",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("mdivide_left_tri_low",  ((ReturnType Vector)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("mdivide_right",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("mdivide_right_spd",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("mdivide_right_spd",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("mdivide_right",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("mdivide_right_tri_low",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("mdivide_right_tri_low",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("mean",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("mean",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("mean",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("mean",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("min",  (ReturnType Int),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("min",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("min",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("min",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("min",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("min",  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("minus",  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("minus",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("minus",  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("minus",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("modified_bessel_first_kind",  ((ReturnType Real)),  (ReturnType Int),  ((ReturnType Real))) 
  ; add_plain ("modified_bessel_second_kind",  ((ReturnType Real)),  (ReturnType Int),  ((ReturnType Real))) 
  ; add_plain ("modulus",  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("multi_gp_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("multi_gp_lpdf",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("multi_gp_cholesky_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("multi_gp_cholesky_lpdf",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
 (
  std::vector<> eigen_vector_types 
  eigen_vector_types.push_back ((ReturnType Vector)) 
  eigen_vector_types.push_back (bare_array_type ((ReturnType Vector))) 
  eigen_vector_types.push_back ((ReturnType RowVector)) 
  eigen_vector_types.push_back (bare_array_type ((ReturnType RowVector))) 
  ; for k = 0 to 4 do    (
     for l = 0 to 4 do    (
         add_plain ("multi_normal_cholesky_log",  ((ReturnType Real)),
           (eigen_vector_types k ),
           (eigen_vector_types l ),  ((ReturnType Matrix))) 

        ; add_plain ("multi_normal_cholesky_lpdf",  ((ReturnType Real)),
           (eigen_vector_types k ),
           (eigen_vector_types l ),  ((ReturnType Matrix))) 

        ; add_plain ("multi_normal_log",  ((ReturnType Real)),
           (eigen_vector_types k ),
           (eigen_vector_types l ),  ((ReturnType Matrix))) 

        ; add_plain ("multi_normal_lpdf",  ((ReturnType Real)),
           (eigen_vector_types k ),
           (eigen_vector_types l ),  ((ReturnType Matrix))) 

        ; add_plain ("multi_normal_prec_log",  ((ReturnType Real)),
           (eigen_vector_types k ),
           (eigen_vector_types l ),  ((ReturnType Matrix))) 

        ; add_plain ("multi_normal_prec_lpdf",  ((ReturnType Real)),
           (eigen_vector_types k ),
           (eigen_vector_types l ),  ((ReturnType Matrix))) 

        ; add_plain ("multi_student_t_log",  ((ReturnType Real)),
           (eigen_vector_types k ),  ((ReturnType Real)),
           (eigen_vector_types l ),  ((ReturnType Matrix))) 

        ; add_plain ("multi_student_t_lpdf",  ((ReturnType Real)),
           (eigen_vector_types k ),  ((ReturnType Real)),
           (eigen_vector_types l ),  ((ReturnType Matrix))) 
    ) done
  ) done
) done
  ; add_plain ("multi_normal_rng",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("multi_normal_rng",  (bare_array_type ((ReturnType Vector), 1)),  (bare_array_type ((ReturnType Vector), 1)),  ((ReturnType Matrix))) 
  ; add_plain ("multi_normal_rng",  ((ReturnType Vector)),  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("multi_normal_rng",  (bare_array_type ((ReturnType Vector), 1)),  (bare_array_type ((ReturnType RowVector), 1)),  ((ReturnType Matrix))) 

  ; add_plain ("multi_normal_cholesky_rng",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("multi_normal_cholesky_rng",  (bare_array_type ((ReturnType Vector), 1)),  (bare_array_type ((ReturnType Vector), 1)),  ((ReturnType Matrix))) 
  ; add_plain ("multi_normal_cholesky_rng",  ((ReturnType Vector)),  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("multi_normal_cholesky_rng",  (bare_array_type ((ReturnType Vector), 1)),  (bare_array_type ((ReturnType RowVector), 1)),  ((ReturnType Matrix))) 

  ; add_plain ("multi_student_t_rng",  ((ReturnType Vector)),  ((ReturnType Real)),  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("multi_student_t_rng",  (bare_array_type ((ReturnType Vector), 1)),  ((ReturnType Real)),  (bare_array_type ((ReturnType Vector), 1)),  ((ReturnType Matrix))) 
  ; add_plain ("multi_student_t_rng",  ((ReturnType Vector)),  ((ReturnType Real)),  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("multi_student_t_rng",  (bare_array_type ((ReturnType Vector), 1)),  ((ReturnType Real)),  (bare_array_type ((ReturnType RowVector), 1)),  ((ReturnType Matrix))) 

  ; add_plain ("multinomial_log",  ((ReturnType Real)),  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector))) 
  ; add_plain ("multinomial_lpmf",  ((ReturnType Real)),  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector))) 
  ; add_plain ("multinomial_rng",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector)),  (ReturnType Int)) 
  ; add_plain ("multiply",  ((ReturnType Real)),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("multiply",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Real))) 
  ; add_plain ("multiply",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Real))) 
  ; add_plain ("multiply",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Real))) 
  ; add_plain ("multiply",  ((ReturnType Real)),  ((ReturnType RowVector)),  ((ReturnType Vector))) 
  ; add_plain ("multiply",  ((ReturnType Matrix)),  ((ReturnType Vector)),  ((ReturnType RowVector))) 
  ; add_plain ("multiply",  ((ReturnType Vector)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("multiply",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("multiply",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("multiply",  ((ReturnType Vector)),  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("multiply",  ((ReturnType RowVector)),  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("multiply",  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_binary ("multiply_log") 
  ; add_plain ("multiply_lower_tri_self_transpose",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
; for i = 0 to int_vector_types_size do    (
  for j = 0 to vector_types_size do    (
    for k = 0 to vector_types_size do    (
         add_plain ("neg_binomial_ccdf_log",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_cdf",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_cdf_log",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_log",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_lccdf",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_lcdf",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_lpmf",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 

        ; add_plain ("neg_binomial_2_ccdf_log",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_2_cdf",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_2_cdf_log",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_2_log",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_2_lccdf",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_2_lcdf",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_2_lpmf",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 

        ; add_plain ("neg_binomial_2_log_log",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("neg_binomial_2_log_lpmf",  ((ReturnType Real)),
          int_vector_types i , vector_types j , vector_types k ) 
    ) done
  ) done
) done
; for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("neg_binomial_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("neg_binomial_2_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("neg_binomial_2_log_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_plain ("neg_binomial_2_log_glm_lpmf",
     ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),
     ((ReturnType Matrix)),
     ((ReturnType Real)),
     ((ReturnType Vector)),
     ((ReturnType Real))) 
  ; add_plain ("neg_binomial_2_log_glm_lpmf",
     ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),
     ((ReturnType Matrix)),
     ((ReturnType Vector)),
     ((ReturnType Vector)),
     ((ReturnType Real))) 
  ; add_nullary ("negative_infinity") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("normal_ccdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("normal_cdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("normal_cdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("normal_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("normal_lccdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("normal_lcdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("normal_lpdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("normal_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_plain ("normal_id_glm_lpdf",
     ((ReturnType Real)),
     ((ReturnType Vector)),
     ((ReturnType Matrix)),
     ((ReturnType Real)),
     ((ReturnType Vector)),
     ((ReturnType Real))) 
  ; add_plain ("normal_id_glm_lpdf",
     ((ReturnType Real)),
     ((ReturnType Vector)),
     ((ReturnType Matrix)),
     ((ReturnType Vector)),
     ((ReturnType Vector)),
     ((ReturnType Real))) 
  ; add_nullary ("not_a_number") 
  ; add_plain ("num_elements",  (ReturnType Int),  ((ReturnType Matrix))) 
  ; add_plain ("num_elements",  (ReturnType Int),  ((ReturnType Vector))) 
  ; add_plain ("num_elements",  (ReturnType Int),  ((ReturnType RowVector))) 
; for i=1  to 10 do (
    add_plain ("num_elements",  (ReturnType Int),  (bare_array_type (bare_array_type (ReturnType Int, i)))) 
    ; add_plain ("num_elements",  (ReturnType Int),  (bare_array_type (bare_array_type ((ReturnType Real), i)))) 
    ; add_plain ("num_elements",  (ReturnType Int),  (bare_array_type (bare_array_type ((ReturnType Matrix), i)))) 
    ; add_plain ("num_elements",  (ReturnType Int),  (bare_array_type (bare_array_type ((ReturnType RowVector), i)))) 
    ; add_plain ("num_elements",  (ReturnType Int),  (bare_array_type (bare_array_type ((ReturnType Vector), i)))) 
 ) done
  ; add_plain ("ordered_logistic_log",  ((ReturnType Real)),  (ReturnType Int),
     ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("ordered_logistic_log",  ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector)),
     ((ReturnType Vector))) 
  ; add_plain ("ordered_logistic_log",  ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector)),
     (bare_array_type ((ReturnType Vector), 1))) 

  ; add_plain ("ordered_logistic_lpmf",  ((ReturnType Real)),  (ReturnType Int),
     ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("ordered_logistic_lpmf",  ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector)),
     ((ReturnType Vector))) 
  ; add_plain ("ordered_logistic_lpmf",  ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector)),
     (bare_array_type ((ReturnType Vector), 1))) 

  ; add_plain ("ordered_logistic_rng",  (ReturnType Int),  ((ReturnType Real)),
     ((ReturnType Vector))) 

  ; add_plain ("ordered_probit_log",  ((ReturnType Real)),  (ReturnType Int),
     ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("ordered_probit_log",  ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector)),
     ((ReturnType Vector))) 
  ; add_plain ("ordered_probit_log",  ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector)),
     (bare_array_type ((ReturnType Vector), 1))) 

  ; add_plain ("ordered_probit_lpmf",  ((ReturnType Real)),  (ReturnType Int),
     ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("ordered_probit_lpmf",  ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),  ((ReturnType Real)),
     ((ReturnType Vector))) 
  ; add_plain ("ordered_probit_lpmf",  ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),  ((ReturnType Real)),
     (bare_array_type ((ReturnType Vector), 1))) 

  ; add_plain ("ordered_probit_rng",  (ReturnType Int),  ((ReturnType Real)),
     ((ReturnType Vector))) 


  ; add_binary ("owens_t") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("pareto_ccdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("pareto_cdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("pareto_cdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("pareto_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("pareto_lccdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("pareto_lcdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("pareto_lpdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("pareto_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
    for k = 0 to vector_types_size do    (
       for l = 0 to vector_types_size do    (
           add_plain ("pareto_type_2_ccdf_log",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("pareto_type_2_cdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("pareto_type_2_cdf_log",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("pareto_type_2_log",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("pareto_type_2_lccdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("pareto_type_2_lcdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("pareto_type_2_lpdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
      ) done
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
    for_all_vector_types  ( fun v -> 
        add_plain ("pareto_type_2_rng",  (rng_return_type) (t, u, v), t, u, v) 
    ) 
  ) 
 ) 
  ; add_unary_vectorized ("Phi") 
  ; add_unary_vectorized ("Phi_approx") 
  ; add_nullary ("pi") 
; for i = 0 to int_vector_types_size do    (
   for j = 0 to vector_types_size do    (
       add_plain ("poisson_ccdf_log",  ((ReturnType Real)), int_vector_types i ,
        vector_types j ) 
      ; add_plain ("poisson_cdf",  ((ReturnType Real)), int_vector_types i ,
        vector_types j ) 
      ; add_plain ("poisson_cdf_log",  ((ReturnType Real)), int_vector_types i ,
        vector_types j ) 
      ; add_plain ("poisson_log",  ((ReturnType Real)), int_vector_types i ,
        vector_types j ) 
      ; add_plain ("poisson_lccdf",  ((ReturnType Real)), int_vector_types i ,
        vector_types j ) 
      ; add_plain ("poisson_lcdf",  ((ReturnType Real)), int_vector_types i ,
        vector_types j ) 
      ; add_plain ("poisson_lpmf",  ((ReturnType Real)), int_vector_types i ,
        vector_types j ) 
  ) done
) done ;
for_all_vector_types  ( fun t -> 
    add_plain ("poisson_rng",  (rng_return_type) (t), t) 
 ) 
; for i = 0 to int_vector_types_size do    (
   for j = 0 to vector_types_size do    (
       add_plain ("poisson_log_log",  ((ReturnType Real)), int_vector_types i ,
        vector_types j ) 
      ; add_plain ("poisson_log_lpmf",  ((ReturnType Real)), int_vector_types i ,
        vector_types j ) 
  ) done
) done ;
for_all_vector_types  ( fun t -> 
     add_plain ("poisson_log_rng",  (rng_return_type) (t), t) 
 ) 
  ; add_plain ("poisson_log_glm_lpmf",
     ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),
     ((ReturnType Matrix)),
     ((ReturnType Real)),
     ((ReturnType Vector))) 
  ; add_plain ("poisson_log_glm_lpmf",
     ((ReturnType Real)),
     (bare_array_type (ReturnType Int, 1)),
     ((ReturnType Matrix)),
     ((ReturnType Vector)),
     ((ReturnType Vector))) 
  ; add_nullary ("positive_infinity") 
  ; add_binary ("pow") 
  ; add_plain ("prod",  (ReturnType Int),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("prod",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("prod",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("prod",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("prod",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("quad_; form",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("quad_; form",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("quad_; form_sym",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("quad_; form_sym",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("quad_; form_diag",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("quad_; form_diag",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType RowVector))) 
  ; add_plain ("rank",  (ReturnType Int),  (bare_array_type (ReturnType Int, 1)),  (ReturnType Int)) 
  ; add_plain ("rank",  (ReturnType Int),  (bare_array_type ((ReturnType Real), 1)),  (ReturnType Int)) 
  ; add_plain ("rank",  (ReturnType Int),  ((ReturnType Vector)),  (ReturnType Int)) 
  ; add_plain ("rank",  (ReturnType Int),  ((ReturnType RowVector)),  (ReturnType Int)) 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
       add_plain ("rayleigh_ccdf_log",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("rayleigh_cdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("rayleigh_cdf_log",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("rayleigh_log",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("rayleigh_lccdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("rayleigh_lcdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
      ; add_plain ("rayleigh_lpdf",  ((ReturnType Real)), vector_types i , vector_types j ) 
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
    add_plain ("rayleigh_rng",  (rng_return_type) (t), t) 
 ) 
  ; add_plain ("append_row",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("append_row",  ((ReturnType Matrix)),  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("append_row",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType RowVector))) 
  ; add_plain ("append_row",  ((ReturnType Matrix)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("append_row",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("append_row",  ((ReturnType Vector)),  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("append_row",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Real))) 
; for i = 0 to bare_types_size do    (
     add_plain ("rep_array",  (bare_array_type (bare_types i , 1)), bare_types i ,  (ReturnType Int)) 
    ; add_plain ("rep_array",  (bare_array_type (bare_types i , 2)), bare_types i ,  (ReturnType Int),  (ReturnType Int)) 
    ; add_plain ("rep_array",  (bare_array_type (bare_types i , 3)), bare_types i ,
       (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; for   j = 1 to 3 do  (
       add_plain ("rep_array",  (bare_array_type (bare_types i , j + 1)),
         (bare_array_type (bare_types i , j)),   (ReturnType Int)) 
      ; add_plain ("rep_array",  (bare_array_type (bare_types i , j + 2)),
         (bare_array_type (bare_types i , j)),   (ReturnType Int),  (ReturnType Int)) 
      ; add_plain ("rep_array",  (bare_array_type (bare_types i , j + 3)),
         (bare_array_type (bare_types i , j)),   (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ) done
 ) done
  ; add_plain ("rep_matrix",  ((ReturnType Matrix)),  ((ReturnType Real)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("rep_matrix",  ((ReturnType Matrix)),  ((ReturnType Vector)),  (ReturnType Int)) 
  ; add_plain ("rep_matrix",  ((ReturnType Matrix)),  ((ReturnType RowVector)),  (ReturnType Int)) 
  ; add_plain ("rep_row_vector",  ((ReturnType RowVector)),  ((ReturnType Real)),  (ReturnType Int)) 
  ; add_plain ("rep_vector",  ((ReturnType Vector)),  ((ReturnType Real)),  (ReturnType Int)) 
  ; add_plain ("rising_factorial",  ((ReturnType Real)),  ((ReturnType Real)),  (ReturnType Int)) 
  ; add_plain ("rising_factorial",  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_unary_vectorized ("round") 
  ; add_plain ("row",  ((ReturnType RowVector)),  ((ReturnType Matrix)),  (ReturnType Int)) 
  ; add_plain ("rows",  (ReturnType Int),  ((ReturnType Vector))) 
  ; add_plain ("rows",  (ReturnType Int),  ((ReturnType RowVector))) 
  ; add_plain ("rows",  (ReturnType Int),  ((ReturnType Matrix))) 
  ; add_plain ("rows_dot_product",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("rows_dot_product",  ((ReturnType Vector)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("rows_dot_product",  ((ReturnType Vector)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("rows_dot_self",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("rows_dot_self",  ((ReturnType Vector)),  ((ReturnType RowVector))) 
  ; add_plain ("rows_dot_self",  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("scale_matrix_exp_multiply",  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("scaled_inv_chi_square_ccdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("scaled_inv_chi_square_cdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("scaled_inv_chi_square_cdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("scaled_inv_chi_square_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("scaled_inv_chi_square_lccdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("scaled_inv_chi_square_lcdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("scaled_inv_chi_square_lpdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("scaled_inv_chi_square_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_plain ("sd",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("sd",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("sd",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("sd",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("segment",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("segment",  ((ReturnType Vector)),  ((ReturnType Vector)),  (ReturnType Int),  (ReturnType Int)) 
; for i = 0 to bare_types_size do    (
     add_plain ("segment",  (bare_array_type (bare_types i , 1)),
       (bare_array_type (bare_types i , 1)),  (ReturnType Int),  (ReturnType Int)) 
    ; add_plain ("segment",  (bare_array_type (bare_types i , 2)),
       (bare_array_type (bare_types i , 2)),  (ReturnType Int),  (ReturnType Int)) 
    ; add_plain ("segment",  (bare_array_type (bare_types i , 3)),
       (bare_array_type (bare_types i , 3)),  (ReturnType Int),  (ReturnType Int)) 
 ) done
  ; add_unary_vectorized ("sin") 
  ; add_plain ("singular_values",  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_unary_vectorized ("sinh") 
// size () is polymorphic over arrays, so start i at 1
; for i = 1 to 8 do    (
     add_plain ("size",  (ReturnType Int),  (bare_array_type (bare_array_type (ReturnType Int, i)))) 
    ; add_plain ("size",  (ReturnType Int),  (bare_array_type (bare_array_type ((ReturnType Real), i)))) 
    ; add_plain ("size",  (ReturnType Int),  (bare_array_type (bare_array_type ((ReturnType Vector), i)))) 
    ; add_plain ("size",  (ReturnType Int),  (bare_array_type (bare_array_type ((ReturnType RowVector), i)))) 
    ; add_plain ("size",  (ReturnType Int),  (bare_array_type (bare_array_type ((ReturnType Matrix), i)))) 
 ) done
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
       for l = 0 to vector_types_size do    (
           add_plain ("skew_normal_ccdf_log",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("skew_normal_cdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("skew_normal_cdf_log",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("skew_normal_log",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("skew_normal_lccdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("skew_normal_lcdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("skew_normal_lpdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
      ) done
    ) done
  ) done
 ) done
; for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
    for_all_vector_types  ( fun v -> 
         add_plain ("skew_normal_rng",  (rng_return_type) (t, u, v), t, u, v) 
    ) 
  ) 
 ) 
  ; add_plain ("softmax",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("sort_asc",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("sort_asc",  (bare_array_type ((ReturnType Real), 1)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("sort_asc",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("sort_asc",  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("sort_desc",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("sort_desc",  (bare_array_type ((ReturnType Real), 1)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("sort_desc",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("sort_desc",  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("sort_indices_asc",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("sort_indices_asc",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("sort_indices_asc",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector))) 
  ; add_plain ("sort_indices_asc",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType RowVector))) 
  ; add_plain ("sort_indices_desc",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("sort_indices_desc",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("sort_indices_desc",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType Vector))) 
  ; add_plain ("sort_indices_desc",  (bare_array_type (ReturnType Int, 1)),  ((ReturnType RowVector))) 
  ; add_plain ("squared_distance",  ((ReturnType Real)),  ((ReturnType Real)),  ((ReturnType Real))) 
  ; add_plain ("squared_distance",  ((ReturnType Real)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("squared_distance",  ((ReturnType Real)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("squared_distance",  ((ReturnType Real)),  ((ReturnType Vector)),  ((ReturnType RowVector))) 
  ; add_plain ("squared_distance",  ((ReturnType Real)),  ((ReturnType RowVector)),  ((ReturnType Vector))) 
  ; add_unary_vectorized ("sqrt") 
  ; add_nullary ("sqrt2") 
  ; add_unary_vectorized ("square") 
; for i = 0 to vector_types_size do  
 (
     add_plain ("std_normal_log",  ((ReturnType Real)), vector_types i ) 
    ; add_plain ("std_normal_lpdf",  ((ReturnType Real)), vector_types i ) 
) done
  ; add_unary ("step") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
       for l = 0 to vector_types_size do    (
           add_plain ("student_t_ccdf_log",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("student_t_cdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("student_t_cdf_log",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("student_t_log",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("student_t_lccdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("student_t_lcdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
          ; add_plain ("student_t_lpdf",  ((ReturnType Real)), vector_types i ,
            vector_types j , vector_types k , vector_types l ) 
      ) done
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
    for_all_vector_types  ( fun v -> 
         add_plain ("student_t_rng",  (rng_return_type) (t, u, v), t, u, v) 
    ) 
  ) 
 ) 
  ; add_plain ("sub_col",  ((ReturnType Vector)),  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("sub_row",  ((ReturnType RowVector)),  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("subtract",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("subtract",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("subtract",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("subtract",  ((ReturnType Vector)),  ((ReturnType Vector)),  ((ReturnType Real))) 
  ; add_plain ("subtract",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  ((ReturnType Real))) 
  ; add_plain ("subtract",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Real))) 
  ; add_plain ("subtract",  ((ReturnType Vector)),  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("subtract",  ((ReturnType RowVector)),  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("subtract",  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("sum",  (ReturnType Int),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("sum",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("sum",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("sum",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("sum",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("tail",  ((ReturnType RowVector)),  ((ReturnType RowVector)),  (ReturnType Int)) 
  ; add_plain ("tail",  ((ReturnType Vector)),  ((ReturnType Vector)),  (ReturnType Int)) 
; for i = 0 to bare_types_size do    (
     add_plain ("tail",  (bare_array_type (bare_types i , 1)),
       (bare_array_type (bare_types i , 1)),  (ReturnType Int)) 
    ; add_plain ("tail",  (bare_array_type (bare_types i , 2)),
       (bare_array_type (bare_types i , 2)),  (ReturnType Int)) 
    ; add_plain ("tail",  (bare_array_type (bare_types i , 3)),
       (bare_array_type (bare_types i , 3)),  (ReturnType Int)) 
 ) done
  ; add_unary_vectorized ("tan") 
  ; add_unary_vectorized ("tanh") 
  (* ; add_nullary ("target") *)
  ; add_plain ("tcrossprod",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_unary_vectorized ("tgamma") 
  ; add_plain ("to_array_1d",  (bare_array_type ((ReturnType Real), 1)),  ((ReturnType Matrix))) 
  ; add_plain ("to_array_1d",  (bare_array_type ((ReturnType Real), 1)),  ((ReturnType Vector))) 
  ; add_plain ("to_array_1d",  (bare_array_type ((ReturnType Real), 1)),  ((ReturnType RowVector))) 
; for   i=1 to 10 do  (
     add_plain ("to_array_1d",  (bare_array_type ((ReturnType Real), 1)),
       (bare_array_type (bare_array_type ((ReturnType Real), i)))) 
    ; add_plain ("to_array_1d",  (bare_array_type (ReturnType Int, 1)),  (bare_array_type (bare_array_type (ReturnType Int, i)))) 
 ) done
  ; add_plain ("to_array_2d",  (bare_array_type ((ReturnType Real), 2)),  ((ReturnType Matrix))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Matrix)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Vector)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType Vector)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType RowVector))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType RowVector)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  ((ReturnType RowVector)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType Real), 1)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType Real), 1)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type (ReturnType Int, 1)),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type (ReturnType Int, 1)),  (ReturnType Int),  (ReturnType Int),  (ReturnType Int)) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type ((ReturnType Real), 2))) 
  ; add_plain ("to_matrix",  ((ReturnType Matrix)),  (bare_array_type (ReturnType Int, 2))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  ((ReturnType Matrix))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  ((ReturnType Vector))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  ((ReturnType RowVector))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("to_row_vector",  ((ReturnType RowVector)),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  ((ReturnType Matrix))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  ((ReturnType Vector))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  ((ReturnType RowVector))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("to_vector",  ((ReturnType Vector)),  (bare_array_type (ReturnType Int, 1))) 
  ; add_plain ("trace",  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("trace_gen_quad_; form",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("trace_quad_; form",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Vector))) 
  ; add_plain ("trace_quad_; form",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_plain ("transpose",  ((ReturnType RowVector)),  ((ReturnType Vector))) 
  ; add_plain ("transpose",  ((ReturnType Vector)),  ((ReturnType RowVector))) 
  ; add_plain ("transpose",  ((ReturnType Matrix)),  ((ReturnType Matrix))) 
  ; add_unary_vectorized ("trunc") 
  ; add_unary_vectorized ("trigamma") 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("uni; form_ccdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_cdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_cdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_lccdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_lcdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("uni; form_lpdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("uni; form_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
  ; add_plain ("variance",  ((ReturnType Real)),  (bare_array_type ((ReturnType Real), 1))) 
  ; add_plain ("variance",  ((ReturnType Real)),  ((ReturnType Vector))) 
  ; add_plain ("variance",  ((ReturnType Real)),  ((ReturnType RowVector))) 
  ; add_plain ("variance",  ((ReturnType Real)),  ((ReturnType Matrix))) 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("von_mises_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("von_mises_lpdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
    ) done
  ) done
) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
       add_plain ("von_mises_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
         add_plain ("weibull_ccdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_cdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_cdf_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_log",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_lccdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_lcdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
        ; add_plain ("weibull_lpdf",  ((ReturnType Real)),
          vector_types i , vector_types j , vector_types k ) 
    ) done
  ) done
 ) done ;
for_all_vector_types  ( fun t -> 
  for_all_vector_types  ( fun u -> 
      add_plain ("weibull_rng",  (rng_return_type) (t, u), t, u) 
  ) 
 ) 
; for i = 0 to vector_types_size do    (
   for j = 0 to vector_types_size do    (
     for k = 0 to vector_types_size do    (
       for l = 0 to vector_types_size do    (
         for m = 0 to vector_types_size do    (
             add_plain ("wiener_log",  ((ReturnType Real)), vector_types i ,
              vector_types j ,vector_types k , vector_types l ,
              vector_types m ) 
            ; add_plain ("wiener_lpdf",  ((ReturnType Real)), vector_types i ,
              vector_types j ,vector_types k , vector_types l ,
              vector_types m ) 
        ) done
      ) done
    ) done
  ) done
 ) done
  ; add_plain ("wishart_log",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("wishart_lpdf",  ((ReturnType Real)),  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 
  ; add_plain ("wishart_rng",  ((ReturnType Matrix)),  ((ReturnType Real)),  ((ReturnType Matrix))) 

