(* Here, we define type checking ; for the Stan Math library *)

(* This is ugly. An ideal treatment of function overloading works by carrying around*
   a LAZY set of types ; for each expression. However, that's awkward in OCaml.
   Perhaps an argument ; for Haskell after all?
   OCaml does have lazy lists. Perhaps those could be used ; for this purpose?
   Or implement our own lazy sets?
   
*)

(* TODO: do appropriate checking ; for higher order functions here *)

(* TODO: first load whole math library into try_get_primitive_return_type --
we are using a predicate here because the functions are overloaded so heavily  *)

open Syntax

(* A semantic error reported by the toplevel *)
let semantic_error ?loc msg =
  Zoo.error ~kind:"Semantic error" ?loc (Scanf.format_from_string msg "")

let primitive_names =
  [ "abs"
  ; "abs"
  ; "acos"
  ; "acosh"
  ; "add"
  ; "add"
  ; "add"
  ; "add"
  ; "add"
  ; "add"
  ; "add"
  ; "add"
  ; "append_array"
  ; "append_array"
  ; "append_array"
  ; "append_array"
  ; "append_array"
  ; "asin"
  ; "asinh"
  ; "atan"
  ; "atan2"
  ; "atanh"
  ; "bernoulli_ccdf_log"
  ; "bernoulli_cdf"
  ; "bernoulli_cdf_log"
  ; "bernoulli_log"
  ; "bernoulli_lccdf"
  ; "bernoulli_lcdf"
  ; "bernoulli_lpmf"
  ; "bernoulli_rng"
  ; "bernoulli_logit_rng"
  ; "bernoulli_logit_log"
  ; "bernoulli_logit_lpmf"
  ; "bernoulli_logit_glm_lpmf"
  ; "bernoulli_logit_glm_lpmf"
  ; "bessel_first_kind"
  ; "bessel_second_kind"
  ; "beta_binomial_ccdf_log"
  ; "beta_binomial_cdf"
  ; "beta_binomial_cdf_log"
  ; "beta_binomial_log"
  ; "beta_binomial_lccdf"
  ; "beta_binomial_lcdf"
  ; "beta_binomial_lpmf"
  ; "beta_binomial_rng"
  ; "beta_ccdf_log"
  ; "beta_cdf"
  ; "beta_cdf_log"
  ; "beta_log"
  ; "beta_lccdf"
  ; "beta_lcdf"
  ; "beta_lpdf"
  ; "beta_rng"
  ; "beta_proportion_ccdf_log"
  ; "beta_proportion_cdf_log"
  ; "beta_proportion_log"
  ; "beta_proportion_lccdf"
  ; "beta_proportion_lcdf"
  ; "beta_proportion_lpdf"
  ; "beta_proportion_rng"
  ; "binary_log_loss"
  ; "binomial_ccdf_log"
  ; "binomial_cdf"
  ; "binomial_cdf_log"
  ; "binomial_log"
  ; "binomial_lccdf"
  ; "binomial_lcdf"
  ; "binomial_lpmf"
  ; "binomial_rng"
  ; "binomial_coefficient_log"
  ; "binomial_logit_log"
  ; "binomial_logit_lpmf"
  ; "block"
  ; "categorical_log"
  ; "categorical_logit_log"
  ; "categorical_lpmf"
  ; "categorical_logit_lpmf"
  ; "categorical_rng"
  ; "categorical_logit_rng"
  ; "cauchy_ccdf_log"
  ; "cauchy_cdf"
  ; "cauchy_cdf_log"
  ; "cauchy_log"
  ; "cauchy_lccdf"
  ; "cauchy_lcdf"
  ; "cauchy_lpdf"
  ; "cauchy_rng"
  ; "append_col"
  ; "append_col"
  ; "append_col"
  ; "append_col"
  ; "append_col"
  ; "append_col"
  ; "append_col"
  ; "cbrt"
  ; "ceil"
  ; "chi_square_ccdf_log"
  ; "chi_square_cdf"
  ; "chi_square_cdf_log"
  ; "chi_square_log"
  ; "chi_square_lccdf"
  ; "chi_square_lcdf"
  ; "chi_square_lpdf"
  ; "chi_square_rng"
  ; "cholesky_decompose"
  ; "choose"
  ; "col"
  ; "cols"
  ; "cols"
  ; "cols"
  ; "columns_dot_product"
  ; "columns_dot_product"
  ; "columns_dot_product"
  ; "columns_dot_self"
  ; "columns_dot_self"
  ; "columns_dot_self"
  ; "cos"
  ; "cosh"
  ; "cov_exp_quad"
  ; "cov_exp_quad"
  ; "cov_exp_quad"
  ; "cov_exp_quad"
  ; "cov_exp_quad"
  ; "cov_exp_quad"
  ; "crossprod"
  ; "csr_matrix_times_vector"
  ; "csr_to_dense_matrix"
  ; "csr_extract_w"
  ; "csr_extract_v"
  ; "csr_extract_u"
  ; "cumulative_sum"
  ; "cumulative_sum"
  ; "cumulative_sum"
  ; "determinant"
  ; "diag_matrix"
  ; "diag_post_multiply"
  ; "diag_post_multiply"
  ; "diag_pre_multiply"
  ; "diag_pre_multiply"
  ; "diagonal"
  ; "digamma"
  ; "dims"
  ; "dims"
  ; "dims"
  ; "dims"
  ; "dims"
  ; "dims"
  ; "dims"
  ; "dims"
  ; "dims"
  ; "dims"
  ; "dirichlet_log"
  ; "dirichlet_lpdf"
  ; "dirichlet_rng"
  ; "distance"
  ; "distance"
  ; "distance"
  ; "distance"
  ; "divide"
  ; "divide"
  ; "divide"
  ; "divide"
  ; "divide"
  ; "dot_product"
  ; "dot_product"
  ; "dot_product"
  ; "dot_product"
  ; "dot_product"
  ; "dot_self"
  ; "dot_self"
  ; "double_exponential_ccdf_log"
  ; "double_exponential_cdf"
  ; "double_exponential_cdf_log"
  ; "double_exponential_log"
  ; "double_exponential_lccdf"
  ; "double_exponential_lcdf"
  ; "double_exponential_lpdf"
  ; "double_exponential_rng"
  ; "e"
  ; "eigenvalues_sym"
  ; "eigenvectors_sym"
  ; "qr_Q"
  ; "qr_R"
  ; "qr_thin_Q"
  ; "qr_thin_R"
  ; "elt_divide"
  ; "elt_divide"
  ; "elt_divide"
  ; "elt_divide"
  ; "elt_divide"
  ; "elt_divide"
  ; "elt_divide"
  ; "elt_divide"
  ; "elt_divide"
  ; "elt_multiply"
  ; "elt_multiply"
  ; "elt_multiply"
  ; "erf"
  ; "erfc"
  ; "exp"
  ; "exp2"
  ; "exp_mod_normal_ccdf_log"
  ; "exp_mod_normal_cdf"
  ; "exp_mod_normal_cdf_log"
  ; "exp_mod_normal_log"
  ; "exp_mod_normal_lccdf"
  ; "exp_mod_normal_lcdf"
  ; "exp_mod_normal_lpdf"
  ; "exp_mod_normal_rng"
  ; "expm1"
  ; "exponential_ccdf_log"
  ; "exponential_cdf"
  ; "exponential_cdf_log"
  ; "exponential_log"
  ; "exponential_lccdf"
  ; "exponential_lcdf"
  ; "exponential_lpdf"
  ; "exponential_rng"
  ; "fabs"
  ; "falling_factorial"
  ; "falling_factorial"
  ; "fdim"
  ; "floor"
  ; "fma"
  ; "fmax"
  ; "fmin"
  ; "fmod"
  ; "frechet_ccdf_log"
  ; "frechet_cdf"
  ; "frechet_cdf_log"
  ; "frechet_log"
  ; "frechet_lccdf"
  ; "frechet_lcdf"
  ; "frechet_lpdf"
  ; "frechet_rng"
  ; "gamma_ccdf_log"
  ; "gamma_cdf"
  ; "gamma_cdf_log"
  ; "gamma_log"
  ; "gamma_lccdf"
  ; "gamma_lcdf"
  ; "gamma_lpdf"
  ; "gamma_p"
  ; "gamma_q"
  ; "gamma_rng"
  ; "gaussian_dlm_obs_log"
  ; "gaussian_dlm_obs_log"
  ; "gaussian_dlm_obs_lpdf"
  ; "gaussian_dlm_obs_lpdf"
  ; "get_lp"
  ; "gumbel_ccdf_log"
  ; "gumbel_cdf"
  ; "gumbel_cdf_log"
  ; "gumbel_log"
  ; "gumbel_lccdf"
  ; "gumbel_lcdf"
  ; "gumbel_lpdf"
  ; "gumbel_rng"
  ; "head"
  ; "head"
  ; "head"
  ; "head"
  ; "head"
  ; "hypergeometric_log"
  ; "hypergeometric_lpmf"
  ; "hypergeometric_rng"
  ; "hypot"
  ; "if_else"
  ; "inc_beta"
  ; "int_step"
  ; "int_step"
  ; "inv"
  ; "inv_chi_square_ccdf_log"
  ; "inv_chi_square_cdf"
  ; "inv_chi_square_cdf_log"
  ; "inv_chi_square_log"
  ; "inv_chi_square_lccdf"
  ; "inv_chi_square_lcdf"
  ; "inv_chi_square_lpdf"
  ; "inv_chi_square_rng"
  ; "inv_cloglog"
  ; "inv_gamma_ccdf_log"
  ; "inv_gamma_cdf"
  ; "inv_gamma_cdf_log"
  ; "inv_gamma_log"
  ; "inv_gamma_lccdf"
  ; "inv_gamma_lcdf"
  ; "inv_gamma_lpdf"
  ; "inv_gamma_rng"
  ; "inv_logit"
  ; "inv_Phi"
  ; "inv_sqrt"
  ; "inv_square"
  ; "inv_wishart_log"
  ; "inv_wishart_lpdf"
  ; "inv_wishart_rng"
  ; "inverse"
  ; "inverse_spd"
  ; "is_inf"
  ; "is_nan"
  ; "lbeta"
  ; "lchoose"
  ; "lgamma"
  ; "lkj_corr_cholesky_log"
  ; "lkj_corr_cholesky_lpdf"
  ; "lkj_corr_cholesky_rng"
  ; "lkj_corr_log"
  ; "lkj_corr_lpdf"
  ; "lkj_corr_rng"
  ; "lkj_cov_log"
  ; "lmgamma"
  ; "lmultiply"
  ; "log"
  ; "log10"
  ; "log10"
  ; "log1m"
  ; "log1m_exp"
  ; "log1m_inv_logit"
  ; "log1p"
  ; "log1p_exp"
  ; "log2"
  ; "log2"
  ; "log_determinant"
  ; "log_diff_exp"
  ; "log_falling_factorial"
  ; "log_mix"
  ; "log_mix"
  ; "log_mix"
  ; "log_mix"
  ; "log_rising_factorial"
  ; "log_inv_logit"
  ; "log_softmax"
  ; "log_sum_exp"
  ; "log_sum_exp"
  ; "log_sum_exp"
  ; "log_sum_exp"
  ; "log_sum_exp"
  ; "logical_negation"
  ; "logical_or"
  ; "logical_and"
  ; "logical_eq"
  ; "logical_neq"
  ; "logical_lt"
  ; "logical_lte"
  ; "logical_gt"
  ; "logical_gte"
  ; "logistic_ccdf_log"
  ; "logistic_cdf"
  ; "logistic_cdf_log"
  ; "logistic_log"
  ; "logistic_lccdf"
  ; "logistic_lcdf"
  ; "logistic_lpdf"
  ; "logistic_rng"
  ; "logit"
  ; "lognormal_ccdf_log"
  ; "lognormal_cdf"
  ; "lognormal_cdf_log"
  ; "lognormal_log"
  ; "lognormal_lccdf"
  ; "lognormal_lcdf"
  ; "lognormal_lpdf"
  ; "lognormal_rng"
  ; "machine_precision"
  ; "matrix_exp"
  ; "matrix_exp_multiply"
  ; "max"
  ; "max"
  ; "max"
  ; "max"
  ; "max"
  ; "max"
  ; "mdivide_left"
  ; "mdivide_left"
  ; "mdivide_left_spd"
  ; "mdivide_left_spd"
  ; "mdivide_left_tri_low"
  ; "mdivide_left_tri_low"
  ; "mdivide_right"
  ; "mdivide_right_spd"
  ; "mdivide_right_spd"
  ; "mdivide_right"
  ; "mdivide_right_tri_low"
  ; "mdivide_right_tri_low"
  ; "mean"
  ; "mean"
  ; "mean"
  ; "mean"
  ; "min"
  ; "min"
  ; "min"
  ; "min"
  ; "min"
  ; "min"
  ; "minus"
  ; "minus"
  ; "minus"
  ; "minus"
  ; "modified_bessel_first_kind"
  ; "modified_bessel_second_kind"
  ; "modulus"
  ; "multi_gp_log"
  ; "multi_gp_lpdf"
  ; "multi_gp_cholesky_log"
  ; "multi_gp_cholesky_lpdf"
  ; "multi_normal_cholesky_log"
  ; "multi_normal_cholesky_lpdf"
  ; "multi_normal_log"
  ; "multi_normal_lpdf"
  ; "multi_normal_prec_log"
  ; "multi_normal_prec_lpdf"
  ; "multi_student_t_log"
  ; "multi_student_t_lpdf"
  ; "multi_normal_rng"
  ; "multi_normal_rng"
  ; "multi_normal_rng"
  ; "multi_normal_rng"
  ; "multi_normal_cholesky_rng"
  ; "multi_normal_cholesky_rng"
  ; "multi_normal_cholesky_rng"
  ; "multi_normal_cholesky_rng"
  ; "multi_student_t_rng"
  ; "multi_student_t_rng"
  ; "multi_student_t_rng"
  ; "multi_student_t_rng"
  ; "multinomial_log"
  ; "multinomial_lpmf"
  ; "multinomial_rng"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply"
  ; "multiply_log"
  ; "multiply_lower_tri_self_transpose"
  ; "neg_binomial_ccdf_log"
  ; "neg_binomial_cdf"
  ; "neg_binomial_cdf_log"
  ; "neg_binomial_log"
  ; "neg_binomial_lccdf"
  ; "neg_binomial_lcdf"
  ; "neg_binomial_lpmf"
  ; "neg_binomial_2_ccdf_log"
  ; "neg_binomial_2_cdf"
  ; "neg_binomial_2_cdf_log"
  ; "neg_binomial_2_log"
  ; "neg_binomial_2_lccdf"
  ; "neg_binomial_2_lcdf"
  ; "neg_binomial_2_lpmf"
  ; "neg_binomial_2_log_log"
  ; "neg_binomial_2_log_lpmf"
  ; "neg_binomial_rng"
  ; "neg_binomial_2_rng"
  ; "neg_binomial_2_log_rng"
  ; "neg_binomial_2_log_glm_lpmf"
  ; "neg_binomial_2_log_glm_lpmf"
  ; "negative_infinity"
  ; "normal_ccdf_log"
  ; "normal_cdf"
  ; "normal_cdf_log"
  ; "normal_log"
  ; "normal_lccdf"
  ; "normal_lcdf"
  ; "normal_lpdf"
  ; "normal_rng"
  ; "normal_id_glm_lpdf"
  ; "normal_id_glm_lpdf"
  ; "not_a_number"
  ; "num_elements"
  ; "num_elements"
  ; "num_elements"
  ; "num_elements"
  ; "num_elements"
  ; "num_elements"
  ; "num_elements"
  ; "num_elements"
  ; "ordered_logistic_log"
  ; "ordered_logistic_log"
  ; "ordered_logistic_log"
  ; "ordered_logistic_lpmf"
  ; "ordered_logistic_lpmf"
  ; "ordered_logistic_lpmf"
  ; "ordered_logistic_rng"
  ; "ordered_probit_log"
  ; "ordered_probit_log"
  ; "ordered_probit_log"
  ; "ordered_probit_lpmf"
  ; "ordered_probit_lpmf"
  ; "ordered_probit_lpmf"
  ; "ordered_probit_rng"
  ; "owens_t"
  ; "pareto_ccdf_log"
  ; "pareto_cdf"
  ; "pareto_cdf_log"
  ; "pareto_log"
  ; "pareto_lccdf"
  ; "pareto_lcdf"
  ; "pareto_lpdf"
  ; "pareto_rng"
  ; "pareto_type_2_ccdf_log"
  ; "pareto_type_2_cdf"
  ; "pareto_type_2_cdf_log"
  ; "pareto_type_2_log"
  ; "pareto_type_2_lccdf"
  ; "pareto_type_2_lcdf"
  ; "pareto_type_2_lpdf"
  ; "pareto_type_2_rng"
  ; "Phi"
  ; "Phi_approx"
  ; "pi"
  ; "poisson_ccdf_log"
  ; "poisson_cdf"
  ; "poisson_cdf_log"
  ; "poisson_log"
  ; "poisson_lccdf"
  ; "poisson_lcdf"
  ; "poisson_lpmf"
  ; "poisson_rng"
  ; "poisson_log_log"
  ; "poisson_log_lpmf"
  ; "poisson_log_rng"
  ; "poisson_log_glm_lpmf"
  ; "poisson_log_glm_lpmf"
  ; "positive_infinity"
  ; "pow"
  ; "prod"
  ; "prod"
  ; "prod"
  ; "prod"
  ; "prod"
  ; "quad_; form"
  ; "quad_; form"
  ; "quad_; form_sym"
  ; "quad_; form_sym"
  ; "quad_; form_diag"
  ; "quad_; form_diag"
  ; "rank"
  ; "rank"
  ; "rank"
  ; "rank"
  ; "rayleigh_ccdf_log"
  ; "rayleigh_cdf"
  ; "rayleigh_cdf_log"
  ; "rayleigh_log"
  ; "rayleigh_lccdf"
  ; "rayleigh_lcdf"
  ; "rayleigh_lpdf"
  ; "rayleigh_rng"
  ; "append_row"
  ; "append_row"
  ; "append_row"
  ; "append_row"
  ; "append_row"
  ; "append_row"
  ; "append_row"
  ; "rep_array"
  ; "rep_array"
  ; "rep_array"
  ; "rep_array"
  ; "rep_array"
  ; "rep_array"
  ; "rep_matrix"
  ; "rep_matrix"
  ; "rep_matrix"
  ; "rep_row_vector"
  ; "rep_vector"
  ; "rising_factorial"
  ; "rising_factorial"
  ; "round"
  ; "row"
  ; "rows"
  ; "rows"
  ; "rows"
  ; "rows_dot_product"
  ; "rows_dot_product"
  ; "rows_dot_product"
  ; "rows_dot_self"
  ; "rows_dot_self"
  ; "rows_dot_self"
  ; "scale_matrix_exp_multiply"
  ; "scaled_inv_chi_square_ccdf_log"
  ; "scaled_inv_chi_square_cdf"
  ; "scaled_inv_chi_square_cdf_log"
  ; "scaled_inv_chi_square_log"
  ; "scaled_inv_chi_square_lccdf"
  ; "scaled_inv_chi_square_lcdf"
  ; "scaled_inv_chi_square_lpdf"
  ; "scaled_inv_chi_square_rng"
  ; "sd"
  ; "sd"
  ; "sd"
  ; "sd"
  ; "segment"
  ; "segment"
  ; "segment"
  ; "segment"
  ; "segment"
  ; "sin"
  ; "singular_values"
  ; "sinh"
  ; "size"
  ; "size"
  ; "size"
  ; "size"
  ; "size"
  ; "skew_normal_ccdf_log"
  ; "skew_normal_cdf"
  ; "skew_normal_cdf_log"
  ; "skew_normal_log"
  ; "skew_normal_lccdf"
  ; "skew_normal_lcdf"
  ; "skew_normal_lpdf"
  ; "skew_normal_rng"
  ; "softmax"
  ; "sort_asc"
  ; "sort_asc"
  ; "sort_asc"
  ; "sort_asc"
  ; "sort_desc"
  ; "sort_desc"
  ; "sort_desc"
  ; "sort_desc"
  ; "sort_indices_asc"
  ; "sort_indices_asc"
  ; "sort_indices_asc"
  ; "sort_indices_asc"
  ; "sort_indices_desc"
  ; "sort_indices_desc"
  ; "sort_indices_desc"
  ; "sort_indices_desc"
  ; "squared_distance"
  ; "squared_distance"
  ; "squared_distance"
  ; "squared_distance"
  ; "squared_distance"
  ; "sqrt"
  ; "sqrt2"
  ; "square"
  ; "std_normal_log"
  ; "std_normal_lpdf"
  ; "step"
  ; "student_t_ccdf_log"
  ; "student_t_cdf"
  ; "student_t_cdf_log"
  ; "student_t_log"
  ; "student_t_lccdf"
  ; "student_t_lcdf"
  ; "student_t_lpdf"
  ; "student_t_rng"
  ; "sub_col"
  ; "sub_row"
  ; "subtract"
  ; "subtract"
  ; "subtract"
  ; "subtract"
  ; "subtract"
  ; "subtract"
  ; "subtract"
  ; "subtract"
  ; "subtract"
  ; "sum"
  ; "sum"
  ; "sum"
  ; "sum"
  ; "sum"
  ; "tail"
  ; "tail"
  ; "tail"
  ; "tail"
  ; "tail"
  ; "tan"
  ; "tanh"
  ; "target"
  ; "get_lp"
  ; "tcrossprod"
  ; "tgamma"
  ; "to_array_1d"
  ; "to_array_1d"
  ; "to_array_1d"
  ; "to_array_1d"
  ; "to_array_1d"
  ; "to_array_2d"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_matrix"
  ; "to_row_vector"
  ; "to_row_vector"
  ; "to_row_vector"
  ; "to_row_vector"
  ; "to_row_vector"
  ; "to_vector"
  ; "to_vector"
  ; "to_vector"
  ; "to_vector"
  ; "to_vector"
  ; "trace"
  ; "trace_gen_quad_; form"
  ; "trace_quad_; form"
  ; "trace_quad_; form"
  ; "transpose"
  ; "transpose"
  ; "transpose"
  ; "trunc"
  ; "trigamma"
  ; "uni; form_ccdf_log"
  ; "uni; form_cdf"
  ; "uni; form_cdf_log"
  ; "uni; form_log"
  ; "uni; form_lccdf"
  ; "uni; form_lcdf"
  ; "uni; form_lpdf"
  ; "uni; form_rng"
  ; "variance"
  ; "variance"
  ; "variance"
  ; "variance"
  ; "von_mises_log"
  ; "von_mises_lpdf"
  ; "von_mises_rng"
  ; "weibull_ccdf_log"
  ; "weibull_cdf"
  ; "weibull_cdf_log"
  ; "weibull_log"
  ; "weibull_lccdf"
  ; "weibull_lcdf"
  ; "weibull_lpdf"
  ; "weibull_rng"
  ; "wiener_log"
  ; "wiener_lpdf"
  ; "wishart_log"
  ; "wishart_lpdf"
  ; "wishart_rng" ]

let primitive_signatures = Hashtbl.create 3000

let bare_types = function
  | 0 -> ReturnType Int
  | 1 -> ReturnType Real
  | 2 -> ReturnType Vector
  | 3 -> ReturnType RowVector
  | 4 -> ReturnType Matrix
  | _ -> semantic_error "This should never happen. Please report a bug."

let bare_types_size = 4

let vector_types = function
  | 0 -> ReturnType Real
  | 1 -> ReturnType (Array Real)
  | 2 -> ReturnType Vector
  | 3 -> ReturnType RowVector
  | _ -> semantic_error "This should never happen. Please report a bug."

let vector_types_size = 3

let int_vector_types = function
  | 0 -> ReturnType Int
  | 1 -> ReturnType (Array Int)
  | _ -> semantic_error "This should never happen. Please report a bug."

let int_vector_types_size = 1

let primitive_types = function
  | 0 -> ReturnType Int
  | 1 -> ReturnType Real
  | _ -> semantic_error "This should never happen. Please report a bug."

let primitive_types_size = 1

let all_vector_types = function
  | 0 -> ReturnType Real
  | 1 -> ReturnType (Array Real)
  | 2 -> ReturnType Vector
  | 3 -> ReturnType RowVector
  | 4 -> ReturnType Int
  | 5 -> ReturnType (Array Int)
  | _ -> semantic_error "This should never happen. Please report a bug."

let all_vector_types_size = 5

let is_primitive = function
  | ReturnType Real -> true
  | ReturnType Int -> true
  | _ -> false

let rng_return_type t lt =
  if List.for_all is_primitive lt then ReturnType t else ReturnType (Array t)

(* TODO: add in multi-argument ones *)

let add_plain (name, rt, argts) =
  Hashtbl.add primitive_signatures name (rt, argts)

let add_nullary name = add_plain (name, ReturnType Real, [])

let _ = add_nullary "reject"

let add_unary name = add_plain (name, ReturnType Real, [ReturnType Real])

let add_unary_vectorized name =
  add_plain (name, ReturnType Real, [ReturnType Int]) ;
  add_plain (name, ReturnType Real, [ReturnType Real]) ;
  add_plain (name, ReturnType Vector, [ReturnType Vector]) ;
  add_plain (name, ReturnType RowVector, [ReturnType RowVector]) ;
  add_plain (name, ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain (name, ReturnType (Array Real), [ReturnType (Array Int)]) ;
  add_plain (name, ReturnType (Array Real), [ReturnType (Array Real)]) ;
  add_plain (name, ReturnType (Array Vector), [ReturnType (Array Vector)]) ;
  add_plain (name, ReturnType (Array RowVector), [ReturnType (Array RowVector)]) ;
  add_plain (name, ReturnType (Array Matrix), [ReturnType (Array Matrix)])

let add_binary name =
  add_plain (name, ReturnType Real, [ReturnType Real; ReturnType Real])

let add_ternary name =
  add_plain
    (name, ReturnType Real, [ReturnType Real; ReturnType Real; ReturnType Real])

let add_quaternary name =
  add_plain
    ( name
    , ReturnType Real
    , [ReturnType Real; ReturnType Real; ReturnType Real; ReturnType Real] )

let basic_bare_array_type = function
  | ReturnType Real -> ReturnType (Array Real)
  | ReturnType Int -> ReturnType (Array Int)
  | ReturnType Vector -> ReturnType (Array Vector)
  | ReturnType RowVector -> ReturnType (Array RowVector)
  | ReturnType Matrix -> ReturnType (Array Matrix)
  | _ -> semantic_error "This should never happen. Please report a bug."

let rec bare_array_type (t, i) =
  match i with
  | 0 -> t
  | j -> basic_bare_array_type (bare_array_type (t, j - 1))

let for_all_vector_types s =
  for i = 0 to 5 do
    s (all_vector_types i)
  done

let for_int_vector_types s =
  for i = 0 to 1 do
    s (int_vector_types i)
  done

let for_vector_types s =
  for i = 0 to 3 do
    s (vector_types i)
  done

let try_get_primitive_return_type name argtypes = None

(* TODO *)
let is_primitive_name name = Hashtbl.mem primitive_signatures name

let _ =
  add_plain ("abs", ReturnType Int, [ReturnType Int]) ;
  add_plain ("abs", ReturnType Int, [ReturnType Int]) ;
  add_plain ("abs", ReturnType Real, [ReturnType Real]) ;
  add_unary_vectorized "acos" ;
  add_unary_vectorized "acosh" ;
  for i = 0 to bare_types_size do
    add_plain ("add", bare_types i, [bare_types i; bare_types i])
  done ;
  add_plain ("add", ReturnType Vector, [ReturnType Vector; ReturnType Real]) ;
  add_plain
    ("add", ReturnType RowVector, [ReturnType RowVector; ReturnType Real]) ;
  add_plain ("add", ReturnType Matrix, [ReturnType Matrix; ReturnType Real]) ;
  add_plain ("add", ReturnType Vector, [ReturnType Real; ReturnType Vector]) ;
  add_plain
    ("add", ReturnType RowVector, [ReturnType Real; ReturnType RowVector]) ;
  add_plain ("add", ReturnType Matrix, [ReturnType Real; ReturnType Matrix]) ;
  for i = 0 to bare_types_size do
    add_plain ("add", bare_types i, [bare_types i])
  done ;
  for i = 1 to 8 do
    add_plain
      ( "append_array"
      , bare_array_type (ReturnType Int, i)
      , [ bare_array_type (ReturnType Int, i)
        ; bare_array_type (ReturnType Int, i) ] ) ;
    add_plain
      ( "append_array"
      , bare_array_type (ReturnType Real, i)
      , [ bare_array_type (ReturnType Real, i)
        ; bare_array_type (ReturnType Real, i) ] ) ;
    add_plain
      ( "append_array"
      , bare_array_type (ReturnType Vector, i)
      , [ bare_array_type (ReturnType Vector, i)
        ; bare_array_type (ReturnType Vector, i) ] ) ;
    add_plain
      ( "append_array"
      , bare_array_type (ReturnType RowVector, i)
      , [ bare_array_type (ReturnType RowVector, i)
        ; bare_array_type (ReturnType RowVector, i) ] ) ;
    add_plain
      ( "append_array"
      , bare_array_type (ReturnType Matrix, i)
      , [ bare_array_type (ReturnType Matrix, i)
        ; bare_array_type (ReturnType Matrix, i) ] )
  done ;
  add_unary_vectorized "asin" ;
  add_unary_vectorized "asinh" ;
  add_unary_vectorized "atan" ;
  add_binary "atan2" ;
  add_unary_vectorized "atanh" ;
  for i = 0 to int_vector_types_size do
    for j = 0 to vector_types_size do
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
      add_plain ("bernoulli_rng", rng_return_type Int [t], [t]) ) ;
  for_all_vector_types (fun t ->
      add_plain ("bernoulli_logit_rng", rng_return_type Int [t], [t]) ) ;
  for i = 0 to int_vector_types_size do
    for j = 0 to vector_types_size do
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
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Matrix
      ; ReturnType Real
      ; ReturnType Vector ] ) ;
  add_plain
    ( "bernoulli_logit_glm_lpmf"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Matrix
      ; ReturnType Vector
      ; ReturnType Vector ] ) ;
  add_plain
    ("bessel_first_kind", ReturnType Real, [ReturnType Int; ReturnType Real]) ;
  add_plain
    ("bessel_second_kind", ReturnType Real, [ReturnType Int; ReturnType Real]) ;
  for i = 0 to int_vector_types_size do
    for j = 0 to int_vector_types_size do
      for k = 0 to vector_types_size do
        for l = 0 to vector_types_size do
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
                ("beta_binomial_rng", rng_return_type Int [t; u; v], [t; u; v])
          ) ) ) ;
  for i = 0 to vector_types_size do
    for j = 0 to vector_types_size do
      for k = 0 to vector_types_size do
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
          add_plain ("beta_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
  for_vector_types (fun t ->
      for_vector_types (fun u ->
          for_all_vector_types (fun v ->
              add_plain ("beta_proportion_ccdf_log", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_cdf_log", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_log", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_lccdf", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_lcdf", ReturnType Real, [t; u; v]) ;
              add_plain ("beta_proportion_lpdf", ReturnType Real, [t; u; v]) )
      ) )
