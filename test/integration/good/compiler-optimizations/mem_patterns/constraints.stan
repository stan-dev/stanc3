data {
  // ints in descending order of size
  int<lower=0> N; // # of obs for training
  int<lower=1> K; // total # of population level effects
  vector[N] diff_low_mid;
  vector[N] diff_high_mid;
  vector[N] mid_price;
  matrix[N, K] X_all;
  real<lower=0> phi_prior_a;
  real<lower=0> phi_prior_b;
  real mu_prior_mu;
  real<lower=0> mu_prior_sigma;
  real<lower=0> sigma_prior_shape;
  real<lower=0> sigma_prior_rate;
}
transformed data {
  int Nr = N - 1;
}
parameters {
  vector<lower=diff_low_mid, upper=diff_high_mid>[N] high_low_est;
  vector[K] b;
  vector[Nr] h;
  vector[2] ar;
  real ma;
  real<lower=0,upper=1> phi_beta;
  real<lower=0> sigma2;
  real Intercept;
  vector[N] mean_price;
  vector<lower=0.0>[N] sigma_price;
  real theta;
  vector<upper=ma>[N] upper_test;
  vector<lower=sigma_price, upper=upper_test>[N] lower_upper_test;
  row_vector<lower=sigma_price', upper=upper_test'>[N] row_vec_lower_upper_test;
  vector<offset=mean_price, multiplier=sigma_price>[N] offset_mult_test;
  ordered[N] ordered_test;
  unit_vector[N] unit_vec_test;
  positive_ordered[N] pos_ordered_test;
  corr_matrix[N] corr_matrix_test;
  cov_matrix[N] cov_matrix_test;
  cholesky_factor_cov[K] chol_fac_cov_test;
  cholesky_factor_corr[K] chol_fac_corr_test;

}

transformed parameters {
  // Transforms from bases
  real<lower=-1,upper=1> phi = fma(phi_beta, 2.0, -1.0);
  real<lower=0> sigma = sqrt(sigma2);
  // price simulation
  vector[N] prices = mid_price + high_low_est;
  vector[Nr] prices_diff = prices[2:N] - prices[1:(N - 1)];
  vector[Nr] mu = Intercept + X_all * b;

  vector[Nr] err;  // actual residuals
  // include ARMA terms
  err = prices_diff - mu;
  mu[2:Nr] += err[1:(Nr-1)] * ma + prices_diff[1:(Nr - 1)] * ar[1];
  mu[3:Nr] += prices_diff[1:(Nr - 2)] * ar[2];
  err = prices_diff - mu;

  vector[Nr] h_i_mean;
  vector<lower=0>[Nr] h_i_sigma;
  h_i_mean[1] = 0.0;
  h_i_sigma[1] = pow(1 - square(phi), -0.5);
  h_i_sigma[2:Nr] = rep_vector(1.0, (Nr - 1));
  h_i_mean[2:Nr] = phi * h[1:(Nr-1)] + theta * square(err[1:(Nr - 1)]);
  vector[Nr] h_sigma = exp(h * (0.5 * sigma) + (0.5 * mu)) + 0.000001;
}

model {
  // jacob adjustment for meas err
  target += -2 * log(sigma_price);
  Intercept ~ normal(mu_prior_mu, mu_prior_sigma);
  phi_beta ~ beta(phi_prior_a, phi_prior_b);
  sigma2 ~ gamma(sigma_prior_shape, sigma_prior_rate);
  ar ~ normal(0, .2);
  ma ~ normal(0, .2);
  h ~ normal(h_i_mean, h_i_sigma);
  sigma_price ~ student_t(3, 1, 1);
  high_low_est ~ uniform(diff_low_mid, diff_high_mid);
  prices ~ normal(mean_price, sigma_price);
  prices_diff ~ normal(mu, h_sigma);
}
