Display all Stan math distributions exposed in the language
  $ stanc --dump-stan-math-distributions
  bernoulli: lpmf, rng, ccdf, cdf, log (deprecated)
  bernoulli_logit_glm: lpmf
  bernoulli_logit: lpmf
  bernoulli_logit: rng, log (deprecated)
  beta_binomial: rng, ccdf, cdf, log (deprecated)
  beta_binomial: lpmf
  beta: rng, ccdf, cdf, log (deprecated)
  beta: lpdf
  beta_proportion: rng, ccdf, cdf, log (deprecated)
  beta_proportion: lpdf
  binomial_logit: log (deprecated)
  binomial_logit: lpmf
  binomial: rng, ccdf, cdf, log (deprecated)
  binomial: lpmf
  categorical: lpmf, log (deprecated)
  categorical_logit: lpmf, log (deprecated)
  categorical_logit_glm: lpmf
  cauchy: lpdf, rng, ccdf, cdf, log (deprecated)
  chi_square: rng, ccdf, cdf, log (deprecated)
  chi_square: lpdf
  dirichlet: lpdf, log (deprecated)
  discrete_range: lpmf, rng, ccdf, cdf, log (deprecated)
  double_exponential: lpdf, rng, ccdf, cdf, log (deprecated)
  exp_mod_normal: lpdf, rng, ccdf, cdf, log (deprecated)
  exponential: lpdf, rng, ccdf, cdf, log (deprecated)
  frechet: lpdf, rng, ccdf, cdf, log (deprecated)
  gamma: rng, ccdf, cdf, log (deprecated)
  gamma: lpdf
  gaussian_dlm_obs: lpdf, log (deprecated)
  gumbel: lpdf, rng, ccdf, cdf, log (deprecated)
  hmm_latent: rng
  hypergeometric: lpmf, rng, log (deprecated)
  inv_chi_square: rng, ccdf, cdf, log (deprecated)
  inv_chi_square: lpdf
  inv_gamma: rng, ccdf, cdf, log (deprecated)
  inv_gamma: lpdf
  inv_wishart_cholesky: lpdf
  inv_wishart: lpdf, log (deprecated)
  lkj_corr: lpdf, log (deprecated)
  lkj_corr_cholesky: lpdf, log (deprecated)
  lkj_cov: lpdf, log (deprecated)
  logistic: lpdf, rng, ccdf, cdf, log (deprecated)
  loglogistic: lpdf, rng, cdf, log (deprecated)
  lognormal: lpdf, rng, ccdf, cdf, log (deprecated)
  multi_gp: lpdf, log (deprecated)
  multi_gp_cholesky: lpdf, log (deprecated)
  multinomial: lpmf, log (deprecated)
  multinomial_logit: lpmf, log (deprecated)
  multi_normal: lpdf, log (deprecated)
  multi_normal_cholesky: lpdf, log (deprecated)
  multi_normal_prec: lpdf, log (deprecated)
  multi_student_t: lpdf, log (deprecated)
  multi_student_t_cholesky: lpdf
  neg_binomial: rng, ccdf, cdf, log (deprecated)
  neg_binomial: lpmf
  neg_binomial_2: rng, ccdf, cdf, log (deprecated)
  neg_binomial_2: lpmf
  neg_binomial_2_log: lpmf, rng, log (deprecated)
  neg_binomial_2_log_glm: lpmf
  normal: lpdf, rng, ccdf, cdf, log (deprecated)
  normal_id_glm: lpdf
  ordered_logistic: lpmf, log (deprecated)
  ordered_logistic_glm: lpmf
  ordered_probit: lpmf, log (deprecated)
  pareto: lpdf, rng, ccdf, cdf, log (deprecated)
  pareto_type_2: lpdf, rng, ccdf, cdf, log (deprecated)
  poisson_log_glm: lpmf
  poisson_log: lpmf, rng, log (deprecated)
  poisson: rng, ccdf, cdf, log (deprecated)
  poisson: lpmf
  rayleigh: lpdf, rng, ccdf, cdf, log (deprecated)
  scaled_inv_chi_square: rng, ccdf, cdf, log (deprecated)
  scaled_inv_chi_square: lpdf
  skew_double_exponential: lpdf, rng, ccdf, cdf, log (deprecated)
  skew_normal: rng, ccdf, cdf, log (deprecated)
  skew_normal: lpdf
  std_normal: lpdf, rng, ccdf, cdf, log (deprecated)
  student_t: rng, ccdf, cdf, log (deprecated)
  student_t: lpdf
  uniform: lpdf, rng, ccdf, cdf, log (deprecated)
  von_mises: lpdf, rng, ccdf, cdf, log (deprecated)
  weibull: lpdf, rng, ccdf, cdf, log (deprecated)
  wiener: lpdf, log (deprecated)
  wishart_cholesky: lpdf
  wishart: lpdf, log (deprecated)
