Display all Stan math distributions exposed in the language
  $ stanc --dump-stan-math-distributions
  beta_binomial: lpmf, rng, ccdf, cdf
  beta: lpdf, rng, ccdf, cdf
  beta_proportion: lpdf, ccdf, cdf
  bernoulli: lpmf, rng, ccdf, cdf
  bernoulli_logit: lpmf, rng
  bernoulli_logit_glm: lpmf
  binomial: lpmf, rng, ccdf, cdf
  binomial_logit: lpmf
  binomial_logit_glm: lpmf
  categorical: lpmf
  categorical_logit: lpmf
  categorical_logit_glm: lpmf
  cauchy: lpdf, rng, ccdf, cdf
  chi_square: lpdf, rng, ccdf, cdf
  dirichlet: lpdf
  discrete_range: lpmf, rng, ccdf, cdf
  double_exponential: lpdf, rng, ccdf, cdf
  exp_mod_normal: lpdf, rng, ccdf, cdf
  exponential: lpdf, rng, ccdf, cdf
  frechet: lpdf, rng, ccdf, cdf
  gamma: lpdf, rng, ccdf, cdf
  gaussian_dlm_obs: lpdf
  gumbel: lpdf, rng, ccdf, cdf
  hmm_latent: rng
  hypergeometric: lpmf, rng
  inv_chi_square: lpdf, rng, ccdf, cdf
  inv_gamma: lpdf, rng, ccdf, cdf
  inv_wishart_cholesky: lpdf
  inv_wishart: lpdf
  lkj_corr: lpdf
  lkj_corr_cholesky: lpdf
  lkj_cov: lpdf
  logistic: lpdf, rng, ccdf, cdf
  loglogistic: lpdf, rng, cdf
  lognormal: lpdf, rng, ccdf, cdf
  multi_gp: lpdf
  multi_gp_cholesky: lpdf
  multinomial: lpmf
  multinomial_logit: lpmf
  multi_normal: lpdf
  multi_normal_cholesky: lpdf
  multi_normal_prec: lpdf
  multi_student_t: lpdf
  multi_student_t_cholesky: lpdf
  neg_binomial: lpmf, rng, ccdf, cdf
  neg_binomial_2: lpmf, rng, ccdf, cdf
  neg_binomial_2_log: lpmf, rng
  neg_binomial_2_log_glm: lpmf
  normal: lpdf, rng, ccdf, cdf
  normal_id_glm: lpdf
  ordered_logistic: lpmf
  ordered_logistic_glm: lpmf
  ordered_probit: lpmf
  pareto: lpdf, rng, ccdf, cdf
  pareto_type_2: lpdf, rng, ccdf, cdf
  poisson: lpmf, rng, ccdf, cdf
  poisson_log: lpmf, rng
  poisson_log_glm: lpmf
  rayleigh: lpdf, rng, ccdf, cdf
  scaled_inv_chi_square: lpdf, rng, ccdf, cdf
  skew_normal: lpdf, rng, ccdf, cdf
  skew_double_exponential: lpdf, rng, ccdf, cdf
  student_t: lpdf, rng, ccdf, cdf
  std_normal: lpdf, rng, ccdf, cdf
  uniform: lpdf, rng, ccdf, cdf
  von_mises: lpdf, rng, ccdf, cdf
  weibull: lpdf, rng, ccdf, cdf
  wiener: lpdf
  wishart_cholesky: lpdf
  wishart: lpdf
