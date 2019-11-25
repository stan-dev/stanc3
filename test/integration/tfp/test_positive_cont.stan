data {
  int<lower=0> N;
  real<lower=0> y[N];
}
parameters {
  real loc_lognormal;
  real<lower=0> scale_lognormal;
  
  real<lower=0> chi_square_nu;
  
  real<lower=0> exp_rate;
  
  real<lower=0> gamma_alpha;
  real<lower=0> gamma_beta;

  real<lower=0> inv_gamma_alpha;
  real<lower=0> inv_gamma_beta;
}
model {
  loc_lognormal ~ normal(0, 5);
  scale_lognormal ~ normal(0, 5);

  chi_square_nu ~ gamma(2,0.1);
  
  exp_rate ~ normal(0, 5);
  
  gamma_alpha ~ normal(0, 5);
  gamma_beta ~ normal(0, 5);

  inv_gamma_alpha ~ normal(0, 5);
  inv_gamma_beta ~ normal(0, 5);

  y ~ lognormal(loc_lognormal, scale_lognormal);
  y ~ chi_square(chi_square_nu);
  y ~ exponential(exp_rate);
  y ~ gamma(gamma_alpha, gamma_beta);
  y ~ inv_gamma(inv_gamma_alpha, inv_gamma_beta);
}