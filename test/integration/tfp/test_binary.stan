data {
  int n_binom;
  int y_binom[10];
  int y_bern[10];
}
parameters {
  real<lower=0, upper=1> p_binom;
  real<lower=0, upper=1> p_bern;
}
model {
  target += binomial_lpmf(y_binom | n_binom, p_binom);
  target += bernoulli_lpmf(y_bern | p_bern);
}