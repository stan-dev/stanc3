data {
  int<lower=1> k;
  int<lower=0> n;
  matrix[n, k] X_d;
  array[2] matrix[n, k] X_d_a;
  vector[n] y_v_d;
  row_vector[n] X_rv_d;
  array[n] int y_vi_d;
  array[n] int y2_vi_d;
  int y_s_d;
  real y_r_d;
}
transformed data {
  int<lower=1> k_td;
  int<lower=0> n_td;
  matrix[n, k] X_d_td;
  vector[n] y_v_d_td;
  row_vector[n] X_rv_d_td;
  array[n] int y_vi_d_td;
  array[n] int y2_vi_d_td;
  int y_s_d_td;
  real y_r_d_td;
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
  y_v_d ~ normal_id_glm(X_d, alpha, beta, sigma);
  
  target += normal_id_glm_lpdf(y_v_d | X_p, alpha, beta, sigma);
  y_v_d ~ normal_id_glm(X_p, alpha, beta, sigma);
  
  target += normal_id_glm_lpdf(y_r_d | X_d, alpha, beta, beta);
  y_r_d ~ normal_id_glm(X_d, alpha, beta, beta);
  
  target += normal_id_glm_lpdf(y_r_d | X_p, alpha, beta, beta);
  y_r_d ~ normal_id_glm(X_p, alpha, beta, beta);
  
  target += normal_id_glm_lpdf(y_v_d | X_rv_d, alpha, beta, beta);
  y_v_d ~ normal_id_glm(X_rv_d, alpha, beta, beta);
  
  target += normal_id_glm_lpdf(y_v_d | X_rv_p, alpha, beta, beta);
  y_v_d ~ normal_id_glm(X_rv_p, alpha, beta, beta);
  
  target += normal_id_glm_lpdf(y_v_d_td | X_d_td, alpha, beta, sigma);
  y_v_d_td ~ normal_id_glm(X_d_td, alpha, beta, sigma);
  
  target += normal_id_glm_lpdf(y_v_d_td | X_p, alpha, beta, sigma);
  y_v_d_td ~ normal_id_glm(X_p, alpha, beta, sigma);
  
  target += normal_id_glm_lpdf(y_r_d_td | X_d_td, alpha, beta, beta);
  y_r_d_td ~ normal_id_glm(X_d_td, alpha, beta, beta);
  
  target += normal_id_glm_lpdf(y_r_d_td | X_p, alpha, beta, beta);
  y_r_d_td ~ normal_id_glm(X_p, alpha, beta, beta);
  
  target += normal_id_glm_lpdf(y_v_d_td | X_rv_d_td, alpha, beta, beta);
  y_v_d_td ~ normal_id_glm(X_rv_d_td, alpha, beta, beta);
  
  target += normal_id_glm_lpdf(y_v_d_td | X_rv_p, alpha, beta, beta);
  y_v_d_td ~ normal_id_glm(X_rv_p, alpha, beta, beta);
  
  target += bernoulli_logit_glm_lpmf(y_vi_d | X_d, alpha, beta);
  y_vi_d ~ bernoulli_logit_glm(X_d, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_vi_d | X_p, alpha, beta);
  y_vi_d ~ bernoulli_logit_glm(X_p, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_vi_d | X_rv_d, alpha, beta);
  y_vi_d ~ bernoulli_logit_glm(X_rv_d, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_vi_d | X_rv_p, alpha, beta);
  y_vi_d ~ bernoulli_logit_glm(X_rv_p, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_s_d | X_d, alpha, beta);
  y_s_d ~ bernoulli_logit_glm(X_d, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_s_d | X_p, alpha, beta);
  y_s_d ~ bernoulli_logit_glm(X_p, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_vi_d_td | X_d_td, alpha, beta);
  y_vi_d_td ~ bernoulli_logit_glm(X_d_td, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_vi_d_td | X_p, alpha, beta);
  y_vi_d_td ~ bernoulli_logit_glm(X_p, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_vi_d_td | X_rv_d_td, alpha, beta);
  y_vi_d_td ~ bernoulli_logit_glm(X_rv_d_td, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_vi_d_td | X_rv_p, alpha, beta);
  y_vi_d_td ~ bernoulli_logit_glm(X_rv_p, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_s_d_td | X_d_td, alpha, beta);
  y_s_d_td ~ bernoulli_logit_glm(X_d_td, alpha, beta);
  
  target += bernoulli_logit_glm_lpmf(y_s_d_td | X_p, alpha, beta);
  y_s_d_td ~ bernoulli_logit_glm(X_p, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_vi_d | X_d, alpha, beta);
  y_vi_d ~ poisson_log_glm(X_d, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_vi_d | X_p, alpha, beta);
  y_vi_d ~ poisson_log_glm(X_p, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_s_d | X_d, alpha, beta);
  y_s_d ~ poisson_log_glm(X_d, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_s_d | X_p, alpha, beta);
  y_s_d ~ poisson_log_glm(X_p, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_vi_d | X_rv_d, alpha, beta);
  y_vi_d ~ poisson_log_glm(X_rv_d, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_vi_d | X_rv_p, alpha, beta);
  y_vi_d ~ poisson_log_glm(X_rv_p, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_vi_d_td | X_d_td, alpha, beta);
  y_vi_d_td ~ poisson_log_glm(X_d_td, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_vi_d_td | X_p, alpha, beta);
  y_vi_d_td ~ poisson_log_glm(X_p, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_s_d_td | X_d_td, alpha, beta);
  y_s_d_td ~ poisson_log_glm(X_d_td, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_s_d_td | X_p, alpha, beta);
  y_s_d_td ~ poisson_log_glm(X_p, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_vi_d_td | X_rv_d_td, alpha, beta);
  y_vi_d_td ~ poisson_log_glm(X_rv_d_td, alpha, beta);
  
  target += poisson_log_glm_lpmf(y_vi_d_td | X_rv_p, alpha, beta);
  y_vi_d_td ~ poisson_log_glm(X_rv_p, alpha, beta);
  
  target += neg_binomial_2_log_glm_lpmf(y_vi_d | X_d, alpha, beta, phi);
  y_vi_d ~ neg_binomial_2_log_glm(X_d, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_vi_d | X_p, alpha, beta, phi);
  y_vi_d ~ neg_binomial_2_log_glm(X_p, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_s_d | X_d, alpha, beta, phi);
  y_s_d ~ neg_binomial_2_log_glm(X_d, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_s_d | X_p, alpha, beta, phi);
  y_s_d ~ neg_binomial_2_log_glm(X_p, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_vi_d | X_rv_d, alpha, beta, phi);
  y_vi_d ~ neg_binomial_2_log_glm(X_rv_d, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_vi_d | X_rv_p, alpha, beta, phi);
  y_vi_d ~ neg_binomial_2_log_glm(X_rv_p, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_vi_d_td | X_d_td, alpha, beta, phi);
  y_vi_d_td ~ neg_binomial_2_log_glm(X_d_td, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_vi_d_td | X_p, alpha, beta, phi);
  y_vi_d_td ~ neg_binomial_2_log_glm(X_p, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_s_d_td | X_d_td, alpha, beta, phi);
  y_s_d_td ~ neg_binomial_2_log_glm(X_d_td, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_s_d_td | X_p, alpha, beta, phi);
  y_s_d_td ~ neg_binomial_2_log_glm(X_p, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_vi_d_td | X_rv_d_td, alpha, beta, phi);
  y_vi_d_td ~ neg_binomial_2_log_glm(X_rv_d_td, alpha, beta, phi);
  
  target += neg_binomial_2_log_glm_lpmf(y_vi_d_td | X_rv_p, alpha, beta, phi);
  y_vi_d_td ~ neg_binomial_2_log_glm(X_rv_p, alpha, beta, phi);
  
  target += ordered_logistic_glm_lpmf(y_s_d | X_d, beta, cuts);
  y_s_d ~ ordered_logistic_glm(X_d, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_s_d | X_p, beta, cuts);
  y_s_d ~ ordered_logistic_glm(X_p, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_s_d_td | X_d_td, beta, cuts);
  y_s_d_td ~ ordered_logistic_glm(X_d_td, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_s_d_td | X_p, beta, cuts);
  y_s_d_td ~ ordered_logistic_glm(X_p, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_s_d | X_d, beta, cuts);
  y_s_d ~ ordered_logistic_glm(X_d, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_s_d | X_p, beta, cuts);
  y_s_d ~ ordered_logistic_glm(X_p, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_s_d_td | X_d_td, beta, cuts);
  y_s_d_td ~ ordered_logistic_glm(X_d_td, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_s_d_td | X_p, beta, cuts);
  y_s_d_td ~ ordered_logistic_glm(X_p, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_s_d | X_rv_d, beta, cuts);
  y_s_d ~ ordered_logistic_glm(X_rv_d, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_s_d | X_rv_p, beta, cuts);
  y_s_d ~ ordered_logistic_glm(X_rv_p, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_vi_d | X_d, beta, cuts);
  y_vi_d ~ ordered_logistic_glm(X_d, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_vi_d | X_p, beta, cuts);
  y_vi_d ~ ordered_logistic_glm(X_p, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_vi_d_td | X_d_td, beta, cuts);
  y_vi_d_td ~ ordered_logistic_glm(X_d_td, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_vi_d_td | X_p, beta, cuts);
  y_vi_d_td ~ ordered_logistic_glm(X_p, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_vi_d | X_rv_d, beta, cuts);
  y_vi_d ~ ordered_logistic_glm(X_rv_d, beta, cuts);
  
  target += ordered_logistic_glm_lpmf(y_vi_d | X_rv_p, beta, cuts);
  y_vi_d ~ ordered_logistic_glm(X_rv_p, beta, cuts);
  
  target += categorical_logit_glm_lpmf(y_s_d | X_d, alpha_v, beta_m);
  y_s_d ~ categorical_logit_glm(X_d, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_s_d | X_p, alpha_v, beta_m);
  y_s_d ~ categorical_logit_glm(X_p, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_s_d_td | X_d_td, alpha_v, beta_m);
  y_s_d_td ~ categorical_logit_glm(X_d_td, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_s_d_td | X_p, alpha_v, beta_m);
  y_s_d_td ~ categorical_logit_glm(X_p, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_s_d | X_rv_d, alpha_v, beta_m);
  y_s_d ~ categorical_logit_glm(X_rv_d, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_s_d | X_rv_p, alpha_v, beta_m);
  y_s_d ~ categorical_logit_glm(X_rv_p, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_s_d_td | X_rv_d_td, alpha_v, beta_m);
  y_s_d_td ~ categorical_logit_glm(X_rv_d_td, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_s_d_td | X_rv_p, alpha_v, beta_m);
  y_s_d_td ~ categorical_logit_glm(X_rv_p, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_vi_d | X_d, alpha_v, beta_m);
  y_vi_d ~ categorical_logit_glm(X_d, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_vi_d | X_p, alpha_v, beta_m);
  y_vi_d ~ categorical_logit_glm(X_p, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_vi_d_td | X_d_td, alpha_v, beta_m);
  y_vi_d_td ~ categorical_logit_glm(X_d_td, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_vi_d_td | X_p, alpha_v, beta_m);
  y_vi_d_td ~ categorical_logit_glm(X_p, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_vi_d | X_rv_d, alpha_v, beta_m);
  y_vi_d ~ categorical_logit_glm(X_rv_d, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_vi_d | X_rv_p, alpha_v, beta_m);
  y_vi_d ~ categorical_logit_glm(X_rv_p, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_vi_d_td | X_rv_d_td, alpha_v, beta_m);
  y_vi_d_td ~ categorical_logit_glm(X_rv_d_td, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y_vi_d_td | X_rv_p, alpha_v, beta_m);
  y_vi_d_td ~ categorical_logit_glm(X_rv_p, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y2_vi_d | X_rv_p, alpha_v, beta_m);
  y2_vi_d ~ categorical_logit_glm(X_rv_p, alpha_v, beta_m);
  
  target += categorical_logit_glm_lpmf(y2_vi_d_td | X_rv_p, alpha_v, beta_m);
  y2_vi_d_td ~ categorical_logit_glm(X_rv_p, alpha_v, beta_m);
  
  target += normal_id_glm_lpdf(y_v_d | X_d_a[1], alpha, beta, sigma);
  y_v_d ~ normal_id_glm(X_d_a[1], alpha, beta, sigma);
}

