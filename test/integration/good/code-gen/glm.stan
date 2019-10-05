data {
  int<lower=1> k;
  int<lower=0> n;
  matrix[n, k] X_d;
  vector[n] y_v;
  row_vector[n] X_rv_d;
  int y_vi[n];
  int y_s;
}

parameters {
  vector[k] beta;
  vector[k] cuts;
  real<lower=0> sigma;
  real alpha;
  real phi;
  matrix[n, k] X_p;
  row_vector[n] X_rv_p;
}

model {
  target += normal_id_glm_lpdf(y_v | X_d, alpha, beta, sigma);
  target += normal_id_glm_lpdf(y_v | X_p, alpha, beta, sigma);

  target += bernoulli_logit_glm_lpmf(y_vi| X_d, alpha, beta);
  target += bernoulli_logit_glm_lpmf(y_vi| X_p, alpha, beta);

  target += poisson_log_glm_lpmf(y_vi| X_d, alpha, beta);
  target += poisson_log_glm_lpmf(y_vi| X_p, alpha, beta);

  target += neg_binomial_2_log_glm_lpmf(y_vi| X_d, alpha, beta, phi);
  target += neg_binomial_2_log_glm_lpmf(y_vi| X_p, alpha, beta, phi);
}
