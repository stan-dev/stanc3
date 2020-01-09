data {
}
parameters {
  real<lower=0> p_ln;
  real<lower=0> p_chi2;
  real<lower=0> p_exp;
  real<lower=0> p_gamma;
  real<lower=0> p_inv_gamma;
}
model {
  target += lognormal_lpdf(p_ln | 1, 1);
  target += chi_square_lpdf(p_chi2 | 3);
  target += exponential_lpdf(p_exp | 2);
  target += gamma_lpdf(p_gamma | 2, 1);
  target += inv_gamma_lpdf(p_inv_gamma | 1, 3);
}