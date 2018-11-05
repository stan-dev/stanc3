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

let bare_types_size = 5

let vector_types = function
  | 0 -> ReturnType Real
  | 1 -> ReturnType (Array Real)
  | 2 -> ReturnType Vector
  | 3 -> ReturnType RowVector
  | _ -> semantic_error "This should never happen. Please report a bug."

let vector_types_size = 4

let int_vector_types = function
  | 0 -> ReturnType Int
  | 1 -> ReturnType (Array Int)
  | _ -> semantic_error "This should never happen. Please report a bug."

let int_vector_types_size = 2

let primitive_types = function
  | 0 -> ReturnType Int
  | 1 -> ReturnType Real
  | _ -> semantic_error "This should never happen. Please report a bug."

let primitive_types_size = 2

let all_vector_types = function
  | 0 -> ReturnType Real
  | 1 -> ReturnType (Array Real)
  | 2 -> ReturnType Vector
  | 3 -> ReturnType RowVector
  | 4 -> ReturnType Int
  | 5 -> ReturnType (Array Int)
  | _ -> semantic_error "This should never happen. Please report a bug."

let all_vector_types_size = 6

let eigen_vector_types = function
  | 0 -> ReturnType Vector
  | 1 -> ReturnType (Array Vector)
  | 2 -> ReturnType RowVector
  | 3 -> ReturnType (Array RowVector)
  | _ -> semantic_error "This should never happen. Please report a bug."

let eigen_vector_types_size = 4

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

let try_get_primitive_return_type name argtypes = None

(* TODO *)
let is_primitive_name name = Hashtbl.mem primitive_signatures name

let _ =
  add_plain ("abs", ReturnType Int, [ReturnType Int]) ;
  add_plain ("abs", ReturnType Int, [ReturnType Int]) ;
  add_plain ("abs", ReturnType Real, [ReturnType Real]) ;
  add_unary_vectorized "acos" ;
  add_unary_vectorized "acosh" ;
  for i = 0 to bare_types_size - 1 do
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
  for i = 0 to bare_types_size - 1 do
    add_plain ("add", bare_types i, [bare_types i])
  done ;
  for i = 1 to 8 - 1 do
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
      add_plain ("bernoulli_rng", rng_return_type Int [t], [t]) ) ;
  for_all_vector_types (fun t ->
      add_plain ("bernoulli_logit_rng", rng_return_type Int [t], [t]) ) ;
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
                ("beta_binomial_rng", rng_return_type Int [t; u; v], [t; u; v])
          ) ) ) ;
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
      ) ) ;
  (* TODO: from here!! *)
  for_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain ("beta_proportion_rng", rng_return_type Real [t; u], [t; u])
      ) ) ;
  add_plain
    ("binary_log_loss", ReturnType Real, [ReturnType Int; ReturnType Real]) ;
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
          add_plain ("binomial_rng", rng_return_type Int [t; u], [t; u]) ) ) ;
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
  add_plain
    ( "block"
    , ReturnType Matrix
    , [ ReturnType Matrix
      ; ReturnType Int
      ; ReturnType Int
      ; ReturnType Int
      ; ReturnType Int ] ) ;
  for i = 0 to int_vector_types_size - 1 do
    add_plain
      ( "categorical_log"
      , ReturnType Real
      , [int_vector_types i; ReturnType Vector] ) ;
    add_plain
      ( "categorical_logit_log"
      , ReturnType Real
      , [int_vector_types i; ReturnType Vector] ) ;
    add_plain
      ( "categorical_lpmf"
      , ReturnType Real
      , [int_vector_types i; ReturnType Vector] ) ;
    add_plain
      ( "categorical_logit_lpmf"
      , ReturnType Real
      , [int_vector_types i; ReturnType Vector] )
  done ;
  add_plain ("categorical_rng", ReturnType Int, [ReturnType Vector]) ;
  add_plain ("categorical_logit_rng", ReturnType Int, [ReturnType Vector]) ;
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
          add_plain ("cauchy_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
  add_plain
    ("append_col", ReturnType Matrix, [ReturnType Matrix; ReturnType Matrix]) ;
  add_plain
    ("append_col", ReturnType Matrix, [ReturnType Vector; ReturnType Matrix]) ;
  add_plain
    ("append_col", ReturnType Matrix, [ReturnType Matrix; ReturnType Vector]) ;
  add_plain
    ("append_col", ReturnType Matrix, [ReturnType Vector; ReturnType Vector]) ;
  add_plain
    ( "append_col"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType RowVector] ) ;
  add_plain
    ( "append_col"
    , ReturnType RowVector
    , [ReturnType Real; ReturnType RowVector] ) ;
  add_plain
    ( "append_col"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType Real] ) ;
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
      add_plain ("chi_square_rng", rng_return_type Real [t], [t]) ) ;
  add_plain ("cholesky_decompose", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain ("choose", ReturnType Int, [ReturnType Int; ReturnType Int]) ;
  add_plain ("col", ReturnType Vector, [ReturnType Matrix; ReturnType Int]) ;
  add_plain ("cols", ReturnType Int, [ReturnType Vector]) ;
  add_plain ("cols", ReturnType Int, [ReturnType RowVector]) ;
  add_plain ("cols", ReturnType Int, [ReturnType Matrix]) ;
  add_plain
    ( "columns_dot_product"
    , ReturnType RowVector
    , [ReturnType Vector; ReturnType Vector] ) ;
  add_plain
    ( "columns_dot_product"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType RowVector] ) ;
  add_plain
    ( "columns_dot_product"
    , ReturnType RowVector
    , [ReturnType Matrix; ReturnType Matrix] ) ;
  add_plain ("columns_dot_self", ReturnType RowVector, [ReturnType Vector]) ;
  add_plain ("columns_dot_self", ReturnType RowVector, [ReturnType RowVector]) ;
  add_plain ("columns_dot_self", ReturnType RowVector, [ReturnType Matrix]) ;
  add_unary_vectorized "cos" ;
  add_unary_vectorized "cosh" ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (ReturnType Real, 1); ReturnType Real; ReturnType Real]
    ) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [bare_array_type (ReturnType Vector, 1); ReturnType Real; ReturnType Real]
    ) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [ bare_array_type (ReturnType RowVector, 1)
      ; ReturnType Real
      ; ReturnType Real ] ) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [ bare_array_type (ReturnType Real, 1)
      ; bare_array_type (ReturnType Real, 1)
      ; ReturnType Real
      ; ReturnType Real ] ) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [ bare_array_type (ReturnType Vector, 1)
      ; bare_array_type (ReturnType Vector, 1)
      ; ReturnType Real
      ; ReturnType Real ] ) ;
  add_plain
    ( "cov_exp_quad"
    , ReturnType Matrix
    , [ bare_array_type (ReturnType RowVector, 1)
      ; bare_array_type (ReturnType RowVector, 1)
      ; ReturnType Real
      ; ReturnType Real ] ) ;
  add_plain ("crossprod", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain
    ( "csr_matrix_times_vector"
    , ReturnType Vector
    , [ ReturnType Int
      ; ReturnType Int
      ; ReturnType Vector
      ; bare_array_type (ReturnType Int, 1)
      ; bare_array_type (ReturnType Int, 1)
      ; ReturnType Vector ] ) ;
  add_plain
    ( "csr_to_dense_matrix"
    , ReturnType Matrix
    , [ ReturnType Int
      ; ReturnType Int
      ; ReturnType Vector
      ; bare_array_type (ReturnType Int, 1)
      ; bare_array_type (ReturnType Int, 1) ] ) ;
  add_plain ("csr_extract_w", ReturnType Vector, [ReturnType Matrix]) ;
  add_plain
    ("csr_extract_v", bare_array_type (ReturnType Int, 1), [ReturnType Matrix]) ;
  add_plain
    ("csr_extract_u", bare_array_type (ReturnType Int, 1), [ReturnType Matrix]) ;
  add_plain
    ( "cumulative_sum"
    , bare_array_type (ReturnType Real, 1)
    , [bare_array_type (ReturnType Real, 1)] ) ;
  add_plain ("cumulative_sum", ReturnType Vector, [ReturnType Vector]) ;
  add_plain ("cumulative_sum", ReturnType RowVector, [ReturnType RowVector]) ;
  add_plain ("determinant", ReturnType Real, [ReturnType Matrix]) ;
  add_plain ("diag_matrix", ReturnType Matrix, [ReturnType Vector]) ;
  add_plain
    ( "diag_post_multiply"
    , ReturnType Matrix
    , [ReturnType Matrix; ReturnType Vector] ) ;
  add_plain
    ( "diag_post_multiply"
    , ReturnType Matrix
    , [ReturnType Matrix; ReturnType RowVector] ) ;
  add_plain
    ( "diag_pre_multiply"
    , ReturnType Matrix
    , [ReturnType Vector; ReturnType Matrix] ) ;
  add_plain
    ( "diag_pre_multiply"
    , ReturnType Matrix
    , [ReturnType RowVector; ReturnType Matrix] ) ;
  add_plain ("diagonal", ReturnType Vector, [ReturnType Matrix]) ;
  add_unary_vectorized "digamma" ;
  add_plain ("dims", bare_array_type (ReturnType Int, 1), [ReturnType Int]) ;
  add_plain ("dims", bare_array_type (ReturnType Int, 1), [ReturnType Real]) ;
  add_plain ("dims", bare_array_type (ReturnType Int, 1), [ReturnType Vector]) ;
  add_plain
    ("dims", bare_array_type (ReturnType Int, 1), [ReturnType RowVector]) ;
  add_plain ("dims", bare_array_type (ReturnType Int, 1), [ReturnType Matrix]) ;
  for i = 0 to 8 - 1 do
    add_plain
      ( "dims"
      , bare_array_type (ReturnType Int, 1)
      , [bare_array_type (ReturnType Int, i + 1)] ) ;
    add_plain
      ( "dims"
      , bare_array_type (ReturnType Int, 1)
      , [bare_array_type (ReturnType Real, i + 1)] ) ;
    add_plain
      ( "dims"
      , bare_array_type (ReturnType Int, 1)
      , [bare_array_type (ReturnType Vector, i + 1)] ) ;
    add_plain
      ( "dims"
      , bare_array_type (ReturnType Int, 1)
      , [bare_array_type (ReturnType RowVector, i + 1)] ) ;
    add_plain
      ( "dims"
      , bare_array_type (ReturnType Int, 1)
      , [bare_array_type (ReturnType Matrix, i + 1)] )
  done ;
  add_plain
    ("dirichlet_log", ReturnType Real, [ReturnType Vector; ReturnType Vector]) ;
  add_plain
    ("dirichlet_lpdf", ReturnType Real, [ReturnType Vector; ReturnType Vector]) ;
  add_plain ("dirichlet_rng", ReturnType Vector, [ReturnType Vector]) ;
  add_plain
    ("distance", ReturnType Real, [ReturnType Vector; ReturnType Vector]) ;
  add_plain
    ("distance", ReturnType Real, [ReturnType RowVector; ReturnType RowVector]) ;
  add_plain
    ("distance", ReturnType Real, [ReturnType Vector; ReturnType RowVector]) ;
  add_plain
    ("distance", ReturnType Real, [ReturnType RowVector; ReturnType Vector]) ;
  add_plain ("divide", ReturnType Int, [ReturnType Int; ReturnType Int]) ;
  add_plain ("divide", ReturnType Real, [ReturnType Real; ReturnType Real]) ;
  add_plain ("divide", ReturnType Vector, [ReturnType Vector; ReturnType Real]) ;
  add_plain
    ("divide", ReturnType RowVector, [ReturnType RowVector; ReturnType Real]) ;
  add_plain ("divide", ReturnType Matrix, [ReturnType Matrix; ReturnType Real]) ;
  add_plain
    ("dot_product", ReturnType Real, [ReturnType Vector; ReturnType Vector]) ;
  add_plain
    ( "dot_product"
    , ReturnType Real
    , [ReturnType RowVector; ReturnType RowVector] ) ;
  add_plain
    ("dot_product", ReturnType Real, [ReturnType Vector; ReturnType RowVector]) ;
  add_plain
    ("dot_product", ReturnType Real, [ReturnType RowVector; ReturnType Vector]) ;
  add_plain
    ( "dot_product"
    , ReturnType Real
    , [ bare_array_type (ReturnType Real, 1)
      ; bare_array_type (ReturnType Real, 1) ] ) ;
  add_plain ("dot_self", ReturnType Real, [ReturnType Vector]) ;
  add_plain ("dot_self", ReturnType Real, [ReturnType RowVector]) ;
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
            ("double_exponential_rng", rng_return_type Real [t; u], [t; u]) )
  ) ;
  add_nullary "e" ;
  add_plain ("eigenvalues_sym", ReturnType Vector, [ReturnType Matrix]) ;
  add_plain ("eigenvectors_sym", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain ("qr_Q", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain ("qr_R", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain ("qr_thin_Q", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain ("qr_thin_R", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain
    ("elt_divide", ReturnType Vector, [ReturnType Vector; ReturnType Vector]) ;
  add_plain
    ( "elt_divide"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType RowVector] ) ;
  add_plain
    ("elt_divide", ReturnType Matrix, [ReturnType Matrix; ReturnType Matrix]) ;
  add_plain
    ("elt_divide", ReturnType Vector, [ReturnType Vector; ReturnType Real]) ;
  add_plain
    ( "elt_divide"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType Real] ) ;
  add_plain
    ("elt_divide", ReturnType Matrix, [ReturnType Matrix; ReturnType Real]) ;
  add_plain
    ("elt_divide", ReturnType Vector, [ReturnType Real; ReturnType Vector]) ;
  add_plain
    ( "elt_divide"
    , ReturnType RowVector
    , [ReturnType Real; ReturnType RowVector] ) ;
  add_plain
    ("elt_divide", ReturnType Matrix, [ReturnType Real; ReturnType Matrix]) ;
  add_plain
    ("elt_multiply", ReturnType Vector, [ReturnType Vector; ReturnType Vector]) ;
  add_plain
    ( "elt_multiply"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType RowVector] ) ;
  add_plain
    ("elt_multiply", ReturnType Matrix, [ReturnType Matrix; ReturnType Matrix]) ;
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
                , rng_return_type Real [t; u; v]
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
      add_plain ("exponential_rng", rng_return_type Real [t], [t]) ) ;
  add_unary_vectorized "fabs" ;
  add_plain
    ("falling_factorial", ReturnType Real, [ReturnType Real; ReturnType Int]) ;
  add_plain
    ("falling_factorial", ReturnType Int, [ReturnType Int; ReturnType Int]) ;
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
          add_plain ("frechet_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
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
          add_plain ("gamma_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
  add_plain
    ( "gaussian_dlm_obs_log"
    , ReturnType Real
    , [ ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Vector
      ; ReturnType Matrix ] ) ;
  add_plain
    ( "gaussian_dlm_obs_log"
    , ReturnType Real
    , [ ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Vector
      ; ReturnType Matrix
      ; ReturnType Vector
      ; ReturnType Matrix ] ) ;
  add_plain
    ( "gaussian_dlm_obs_lpdf"
    , ReturnType Real
    , [ ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Vector
      ; ReturnType Matrix ] ) ;
  add_plain
    ( "gaussian_dlm_obs_lpdf"
    , ReturnType Real
    , [ ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Matrix
      ; ReturnType Vector
      ; ReturnType Matrix
      ; ReturnType Vector
      ; ReturnType Matrix ] )
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
          add_plain ("gumbel_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
  add_plain
    ("head", ReturnType RowVector, [ReturnType RowVector; ReturnType Int]) ;
  add_plain ("head", ReturnType Vector, [ReturnType Vector; ReturnType Int]) ;
  for i = 0 to bare_types_size - 1 do
    add_plain
      ( "head"
      , bare_array_type (bare_types i, 1)
      , [bare_array_type (bare_types i, 1); ReturnType Int] ) ;
    add_plain
      ( "head"
      , bare_array_type (bare_types i, 2)
      , [bare_array_type (bare_types i, 2); ReturnType Int] ) ;
    add_plain
      ( "head"
      , bare_array_type (bare_types i, 3)
      , [bare_array_type (bare_types i, 3); ReturnType Int] )
  done ;
  add_plain
    ( "hypergeometric_log"
    , ReturnType Real
    , [ReturnType Int; ReturnType Int; ReturnType Int; ReturnType Int] ) ;
  add_plain
    ( "hypergeometric_lpmf"
    , ReturnType Real
    , [ReturnType Int; ReturnType Int; ReturnType Int; ReturnType Int] ) ;
  add_plain
    ( "hypergeometric_rng"
    , ReturnType Int
    , [ReturnType Int; ReturnType Int; ReturnType Int] ) ;
  add_binary "hypot" ;
  add_plain
    ( "if_else"
    , ReturnType Real
    , [ReturnType Int; ReturnType Real; ReturnType Real] ) ;
  add_plain
    ( "inc_beta"
    , ReturnType Real
    , [ReturnType Real; ReturnType Real; ReturnType Real] ) ;
  add_plain ("int_step", ReturnType Int, [ReturnType Real]) ;
  add_plain ("int_step", ReturnType Int, [ReturnType Int]) ;
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
      add_plain ("inv_chi_square_rng", rng_return_type Real [t], [t]) ) ;
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
          add_plain ("inv_gamma_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
  add_unary_vectorized "inv_logit" ;
  add_unary_vectorized "inv_Phi" ;
  add_unary_vectorized "inv_sqrt" ;
  add_unary_vectorized "inv_square" ;
  add_plain
    ( "inv_wishart_log"
    , ReturnType Real
    , [ReturnType Matrix; ReturnType Real; ReturnType Matrix] ) ;
  add_plain
    ( "inv_wishart_lpdf"
    , ReturnType Real
    , [ReturnType Matrix; ReturnType Real; ReturnType Matrix] ) ;
  add_plain
    ("inv_wishart_rng", ReturnType Matrix, [ReturnType Real; ReturnType Matrix]) ;
  add_plain ("inverse", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain ("inverse_spd", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain ("is_inf", ReturnType Int, [ReturnType Real]) ;
  add_plain ("is_nan", ReturnType Int, [ReturnType Real]) ;
  add_binary "lbeta" ;
  add_binary "lchoose" ;
  add_unary_vectorized "lgamma" ;
  add_plain
    ( "lkj_corr_cholesky_log"
    , ReturnType Real
    , [ReturnType Matrix; ReturnType Real] ) ;
  add_plain
    ( "lkj_corr_cholesky_lpdf"
    , ReturnType Real
    , [ReturnType Matrix; ReturnType Real] ) ;
  add_plain
    ( "lkj_corr_cholesky_rng"
    , ReturnType Matrix
    , [ReturnType Int; ReturnType Real] ) ;
  add_plain
    ("lkj_corr_log", ReturnType Real, [ReturnType Matrix; ReturnType Real]) ;
  add_plain
    ("lkj_corr_lpdf", ReturnType Real, [ReturnType Matrix; ReturnType Real]) ;
  add_plain
    ("lkj_corr_rng", ReturnType Matrix, [ReturnType Int; ReturnType Real]) ;
  add_plain
    ( "lkj_cov_log"
    , ReturnType Real
    , [ReturnType Matrix; ReturnType Vector; ReturnType Vector; ReturnType Real]
    ) ;
  add_plain ("lmgamma", ReturnType Real, [ReturnType Int; ReturnType Real]) ;
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
  add_plain ("log_determinant", ReturnType Real, [ReturnType Matrix]) ;
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
      , [vector_types i; bare_array_type (ReturnType Vector, 1)] ) ;
    add_plain
      ( "log_mix"
      , ReturnType Real
      , [vector_types i; bare_array_type (ReturnType RowVector, 1)] )
  done ;
  add_binary "log_rising_factorial" ;
  add_unary_vectorized "log_inv_logit" ;
  add_plain ("log_softmax", ReturnType Vector, [ReturnType Vector]) ;
  add_plain
    ("log_sum_exp", ReturnType Real, [bare_array_type (ReturnType Real, 1)]) ;
  add_plain ("log_sum_exp", ReturnType Real, [ReturnType Vector]) ;
  add_plain ("log_sum_exp", ReturnType Real, [ReturnType RowVector]) ;
  add_plain ("log_sum_exp", ReturnType Real, [ReturnType Matrix]) ;
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
          add_plain ("logistic_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
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
          add_plain ("lognormal_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
  add_nullary "machine_precision" ;
  add_plain ("matrix_exp", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain
    ( "matrix_exp_multiply"
    , ReturnType Matrix
    , [ReturnType Matrix; ReturnType Matrix] ) ;
  add_plain ("max", ReturnType Int, [bare_array_type (ReturnType Int, 1)]) ;
  add_plain ("max", ReturnType Real, [bare_array_type (ReturnType Real, 1)]) ;
  add_plain ("max", ReturnType Real, [ReturnType Vector]) ;
  add_plain ("max", ReturnType Real, [ReturnType RowVector]) ;
  add_plain ("max", ReturnType Real, [ReturnType Matrix]) ;
  add_plain ("max", ReturnType Int, [ReturnType Int; ReturnType Int]) ;
  add_plain
    ("mdivide_left", ReturnType Vector, [ReturnType Matrix; ReturnType Vector]) ;
  add_plain
    ("mdivide_left", ReturnType Matrix, [ReturnType Matrix; ReturnType Matrix]) ;
  add_plain
    ( "mdivide_left_spd"
    , ReturnType Vector
    , [ReturnType Matrix; ReturnType Vector] ) ;
  add_plain
    ( "mdivide_left_spd"
    , ReturnType Matrix
    , [ReturnType Matrix; ReturnType Matrix] ) ;
  add_plain
    ( "mdivide_left_tri_low"
    , ReturnType Matrix
    , [ReturnType Matrix; ReturnType Matrix] ) ;
  add_plain
    ( "mdivide_left_tri_low"
    , ReturnType Vector
    , [ReturnType Matrix; ReturnType Vector] ) ;
  add_plain
    ( "mdivide_right"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType Matrix] ) ;
  add_plain
    ( "mdivide_right_spd"
    , ReturnType Matrix
    , [ReturnType Matrix; ReturnType Matrix] ) ;
  add_plain
    ( "mdivide_right_spd"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType Matrix] ) ;
  add_plain
    ("mdivide_right", ReturnType Matrix, [ReturnType Matrix; ReturnType Matrix]) ;
  add_plain
    ( "mdivide_right_tri_low"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType Matrix] ) ;
  add_plain
    ( "mdivide_right_tri_low"
    , ReturnType Matrix
    , [ReturnType Matrix; ReturnType Matrix] ) ;
  add_plain ("mean", ReturnType Real, [bare_array_type (ReturnType Real, 1)]) ;
  add_plain ("mean", ReturnType Real, [ReturnType Vector]) ;
  add_plain ("mean", ReturnType Real, [ReturnType RowVector]) ;
  add_plain ("mean", ReturnType Real, [ReturnType Matrix]) ;
  add_plain ("min", ReturnType Int, [bare_array_type (ReturnType Int, 1)]) ;
  add_plain ("min", ReturnType Real, [bare_array_type (ReturnType Real, 1)]) ;
  add_plain ("min", ReturnType Real, [ReturnType Vector]) ;
  add_plain ("min", ReturnType Real, [ReturnType RowVector]) ;
  add_plain ("min", ReturnType Real, [ReturnType Matrix]) ;
  add_plain ("min", ReturnType Int, [ReturnType Int; ReturnType Int]) ;
  add_plain ("minus", ReturnType Real, [ReturnType Real]) ;
  add_plain ("minus", ReturnType Vector, [ReturnType Vector]) ;
  add_plain ("minus", ReturnType RowVector, [ReturnType RowVector]) ;
  add_plain ("minus", ReturnType Matrix, [ReturnType Matrix]) ;
  add_plain
    ( "modified_bessel_first_kind"
    , ReturnType Real
    , [ReturnType Int; ReturnType Real] ) ;
  add_plain
    ( "modified_bessel_second_kind"
    , ReturnType Real
    , [ReturnType Int; ReturnType Real] ) ;
  add_plain ("modulus", ReturnType Int, [ReturnType Int; ReturnType Int]) ;
  add_plain
    ( "multi_gp_log"
    , ReturnType Real
    , [ReturnType Matrix; ReturnType Matrix; ReturnType Vector] ) ;
  add_plain
    ( "multi_gp_lpdf"
    , ReturnType Real
    , [ReturnType Matrix; ReturnType Matrix; ReturnType Vector] ) ;
  add_plain
    ( "multi_gp_cholesky_log"
    , ReturnType Real
    , [ReturnType Matrix; ReturnType Matrix; ReturnType Vector] ) ;
  add_plain
    ( "multi_gp_cholesky_lpdf"
    , ReturnType Real
    , [ReturnType Matrix; ReturnType Matrix; ReturnType Vector] ) ;
  for k = 0 to 4 - 1 do
    for l = 0 to 4 - 1 do
      add_plain
        ( "multi_normal_cholesky_log"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; ReturnType Matrix] ) ;
      add_plain
        ( "multi_normal_cholesky_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; ReturnType Matrix] ) ;
      add_plain
        ( "multi_normal_log"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; ReturnType Matrix] ) ;
      add_plain
        ( "multi_normal_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; ReturnType Matrix] ) ;
      add_plain
        ( "multi_normal_prec_log"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; ReturnType Matrix] ) ;
      add_plain
        ( "multi_normal_prec_lpdf"
        , ReturnType Real
        , [eigen_vector_types k; eigen_vector_types l; ReturnType Matrix] ) ;
      add_plain
        ( "multi_student_t_log"
        , ReturnType Real
        , [ eigen_vector_types k
          ; ReturnType Real
          ; eigen_vector_types l
          ; ReturnType Matrix ] ) ;
      add_plain
        ( "multi_student_t_lpdf"
        , ReturnType Real
        , [ eigen_vector_types k
          ; ReturnType Real
          ; eigen_vector_types l
          ; ReturnType Matrix ] )
    done
  done ;
  add_plain
    ( "multi_normal_rng"
    , ReturnType Vector
    , [ReturnType Vector; ReturnType Matrix] ) ;
  add_plain
    ( "multi_normal_rng"
    , bare_array_type (ReturnType Vector, 1)
    , [bare_array_type (ReturnType Vector, 1); ReturnType Matrix] ) ;
  add_plain
    ( "multi_normal_rng"
    , ReturnType Vector
    , [ReturnType RowVector; ReturnType Matrix] ) ;
  add_plain
    ( "multi_normal_rng"
    , bare_array_type (ReturnType Vector, 1)
    , [bare_array_type (ReturnType RowVector, 1); ReturnType Matrix] ) ;
  add_plain
    ( "multi_normal_cholesky_rng"
    , ReturnType Vector
    , [ReturnType Vector; ReturnType Matrix] ) ;
  add_plain
    ( "multi_normal_cholesky_rng"
    , bare_array_type (ReturnType Vector, 1)
    , [bare_array_type (ReturnType Vector, 1); ReturnType Matrix] ) ;
  add_plain
    ( "multi_normal_cholesky_rng"
    , ReturnType Vector
    , [ReturnType RowVector; ReturnType Matrix] ) ;
  add_plain
    ( "multi_normal_cholesky_rng"
    , bare_array_type (ReturnType Vector, 1)
    , [bare_array_type (ReturnType RowVector, 1); ReturnType Matrix] ) ;
  add_plain
    ( "multi_student_t_rng"
    , ReturnType Vector
    , [ReturnType Real; ReturnType Vector; ReturnType Matrix] ) ;
  add_plain
    ( "multi_student_t_rng"
    , bare_array_type (ReturnType Vector, 1)
    , [ ReturnType Real
      ; bare_array_type (ReturnType Vector, 1)
      ; ReturnType Matrix ] ) ;
  add_plain
    ( "multi_student_t_rng"
    , ReturnType Vector
    , [ReturnType Real; ReturnType RowVector; ReturnType Matrix] ) ;
  add_plain
    ( "multi_student_t_rng"
    , bare_array_type (ReturnType Vector, 1)
    , [ ReturnType Real
      ; bare_array_type (ReturnType RowVector, 1)
      ; ReturnType Matrix ] ) ;
  add_plain
    ( "multinomial_log"
    , ReturnType Real
    , [bare_array_type (ReturnType Int, 1); ReturnType Vector] ) ;
  add_plain
    ( "multinomial_lpmf"
    , ReturnType Real
    , [bare_array_type (ReturnType Int, 1); ReturnType Vector] ) ;
  add_plain
    ( "multinomial_rng"
    , bare_array_type (ReturnType Int, 1)
    , [ReturnType Vector; ReturnType Int] ) ;
  add_plain ("multiply", ReturnType Real, [ReturnType Real; ReturnType Real]) ;
  add_plain
    ("multiply", ReturnType Vector, [ReturnType Vector; ReturnType Real]) ;
  add_plain
    ("multiply", ReturnType RowVector, [ReturnType RowVector; ReturnType Real]) ;
  add_plain
    ("multiply", ReturnType Matrix, [ReturnType Matrix; ReturnType Real]) ;
  add_plain
    ("multiply", ReturnType Real, [ReturnType RowVector; ReturnType Vector]) ;
  add_plain
    ("multiply", ReturnType Matrix, [ReturnType Vector; ReturnType RowVector]) ;
  add_plain
    ("multiply", ReturnType Vector, [ReturnType Matrix; ReturnType Vector]) ;
  add_plain
    ( "multiply"
    , ReturnType RowVector
    , [ReturnType RowVector; ReturnType Matrix] ) ;
  add_plain
    ("multiply", ReturnType Matrix, [ReturnType Matrix; ReturnType Matrix]) ;
  add_plain
    ("multiply", ReturnType Vector, [ReturnType Real; ReturnType Vector]) ;
  add_plain
    ("multiply", ReturnType RowVector, [ReturnType Real; ReturnType RowVector]) ;
  add_plain
    ("multiply", ReturnType Matrix, [ReturnType Real; ReturnType Matrix]) ;
  add_binary "multiply_log" ;
  add_plain
    ( "multiply_lower_tri_self_transpose"
    , ReturnType Matrix
    , [ReturnType Matrix] ) ;
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
          add_plain ("neg_binomial_rng", rng_return_type Int [t; u], [t; u]) )
  ) ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain ("neg_binomial_2_rng", rng_return_type Int [t; u], [t; u])
      ) ) ;
  for_all_vector_types (fun t ->
      for_all_vector_types (fun u ->
          add_plain
            ("neg_binomial_2_log_rng", rng_return_type Int [t; u], [t; u]) ) ) ;
  add_plain
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Matrix
      ; ReturnType Real
      ; ReturnType Vector
      ; ReturnType Real ] ) ;
  add_plain
    ( "neg_binomial_2_log_glm_lpmf"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Matrix
      ; ReturnType Vector
      ; ReturnType Vector
      ; ReturnType Real ] ) ;
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
          add_plain ("normal_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
  add_plain
    ( "normal_id_glm_lpdf"
    , ReturnType Real
    , [ ReturnType Vector
      ; ReturnType Matrix
      ; ReturnType Real
      ; ReturnType Vector
      ; ReturnType Real ] ) ;
  add_plain
    ( "normal_id_glm_lpdf"
    , ReturnType Real
    , [ ReturnType Vector
      ; ReturnType Matrix
      ; ReturnType Vector
      ; ReturnType Vector
      ; ReturnType Real ] ) ;
  add_nullary "not_a_number" ;
  add_plain ("num_elements", ReturnType Int, [ReturnType Matrix]) ;
  add_plain ("num_elements", ReturnType Int, [ReturnType Vector]) ;
  add_plain ("num_elements", ReturnType Int, [ReturnType RowVector]) ;
  for i = 1 to 10 - 1 do
    add_plain
      ("num_elements", ReturnType Int, [bare_array_type (ReturnType Int, i)]) ;
    add_plain
      ("num_elements", ReturnType Int, [bare_array_type (ReturnType Real, i)]) ;
    add_plain
      ("num_elements", ReturnType Int, [bare_array_type (ReturnType Matrix, i)]) ;
    add_plain
      ( "num_elements"
      , ReturnType Int
      , [bare_array_type (ReturnType RowVector, i)] ) ;
    add_plain
      ("num_elements", ReturnType Int, [bare_array_type (ReturnType Vector, i)])
  done ;
  add_plain
    ( "ordered_logistic_log"
    , ReturnType Real
    , [ReturnType Int; ReturnType Real; ReturnType Vector] ) ;
  add_plain
    ( "ordered_logistic_log"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Vector
      ; ReturnType Vector ] ) ;
  add_plain
    ( "ordered_logistic_log"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Vector
      ; bare_array_type (ReturnType Vector, 1) ] ) ;
  add_plain
    ( "ordered_logistic_lpmf"
    , ReturnType Real
    , [ReturnType Int; ReturnType Real; ReturnType Vector] ) ;
  add_plain
    ( "ordered_logistic_lpmf"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Vector
      ; ReturnType Vector ] ) ;
  add_plain
    ( "ordered_logistic_lpmf"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Vector
      ; bare_array_type (ReturnType Vector, 1) ] ) ;
  add_plain
    ( "ordered_logistic_rng"
    , ReturnType Int
    , [ReturnType Real; ReturnType Vector] ) ;
  add_plain
    ( "ordered_probit_log"
    , ReturnType Real
    , [ReturnType Int; ReturnType Real; ReturnType Vector] ) ;
  add_plain
    ( "ordered_probit_log"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Vector
      ; ReturnType Vector ] ) ;
  add_plain
    ( "ordered_probit_log"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Vector
      ; bare_array_type (ReturnType Vector, 1) ] ) ;
  add_plain
    ( "ordered_probit_lpmf"
    , ReturnType Real
    , [ReturnType Int; ReturnType Real; ReturnType Vector] ) ;
  add_plain
    ( "ordered_probit_lpmf"
    , ReturnType Real
    , [bare_array_type (ReturnType Int, 1); ReturnType Real; ReturnType Vector]
    ) ;
  add_plain
    ( "ordered_probit_lpmf"
    , ReturnType Real
    , [ bare_array_type (ReturnType Int, 1)
      ; ReturnType Real
      ; bare_array_type (ReturnType Vector, 1) ] ) ;
  add_plain
    ("ordered_probit_rng", ReturnType Int, [ReturnType Real; ReturnType Vector]) ;
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
          add_plain ("pareto_rng", rng_return_type Real [t; u], [t; u]) ) ) ;
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
                ("pareto_type_2_rng", rng_return_type Real [t; u; v], [t; u; v])
          ) ) ) ;
  add_unary_vectorized "Phi" ;
  add_unary_vectorized "Phi_approx" ;
  add_nullary "pi" ;
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
      add_plain ("poisson_rng", rng_return_type Int [t], [t]) ) ;
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
  done
