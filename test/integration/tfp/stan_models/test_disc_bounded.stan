data {
  int n;
  int y[10];
}
parameters {
  real<lower=0, upper=1> p_binom;
  real p_binom_logit;
}
model {
  target += binomial_lpmf(y | n, p_binom);
  target += binomial_logit_lpmf(y | n, p_binom_logit);
}