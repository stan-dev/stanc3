data {
  int<lower=1> k;
  int<lower=0> n;
  matrix[n, k] X_d;
  vector[n] y_v_d;
  row_vector[n] X_rv_d;
  int y_vi_d[n];
  int y_s_d;
}

parameters {
  vector[k] alpha_v;
  vector[k] beta;
  vector[k] cuts;
  real<lower=0> sigma;
  real alpha;
  real phi; 
  matrix[n, k] X_p;
  matrix[n, k] beta_m;
  row_vector[n] X_rv_p;
}

model {
  target += normal_id_glm_lpdf(y_v_d | X_d, alpha, beta, sigma);
  target += normal_id_glm_lpdf(y_v_d | X_p, alpha, beta, sigma);

  target += bernoulli_logit_glm_lpmf(y_vi_d| X_d, alpha, beta);
  target += bernoulli_logit_glm_lpmf(y_vi_d| X_p, alpha, beta);

  target += poisson_log_glm_lpmf(y_vi_d| X_d, alpha, beta);
  target += poisson_log_glm_lpmf(y_vi_d| X_p, alpha, beta);

  target += neg_binomial_2_log_glm_lpmf(y_vi_d| X_d, alpha, beta, phi);
  target += neg_binomial_2_log_glm_lpmf(y_vi_d| X_p, alpha, beta, phi);

  target += ordered_logistic_glm_lpmf(y_s_d| X_d, beta, cuts);
  target += ordered_logistic_glm_lpmf(y_s_d| X_p, beta, cuts);

  target += ordered_logistic_glm_lpmf(y_s_d| X_rv_d, beta, cuts);
  target += ordered_logistic_glm_lpmf(y_s_d| X_rv_p, beta, cuts);

  target += ordered_logistic_glm_lpmf(y_vi_d| X_d, beta, cuts);
  target += ordered_logistic_glm_lpmf(y_vi_d| X_p, beta, cuts);

  target += ordered_logistic_glm_lpmf(y_vi_d| X_rv_d, beta, cuts);
  target += ordered_logistic_glm_lpmf(y_vi_d| X_rv_p, beta, cuts);

  target += categorical_logit_glm_lpmf(y_s_d| X_d, alpha_v, beta_m);
  target += categorical_logit_glm_lpmf(y_s_d| X_p, alpha_v, beta_m);

  target += categorical_logit_glm_lpmf(y_s_d| X_rv_d, alpha_v, beta_m);
  target += categorical_logit_glm_lpmf(y_s_d| X_rv_p, alpha_v, beta_m);

  target += categorical_logit_glm_lpmf(y_vi_d| X_d, alpha_v, beta_m);
  target += categorical_logit_glm_lpmf(y_vi_d| X_p, alpha_v, beta_m);

  target += categorical_logit_glm_lpmf(y_vi_d| X_rv_d, alpha_v, beta_m);
  target += categorical_logit_glm_lpmf(y_vi_d| X_rv_p, alpha_v, beta_m);
}
