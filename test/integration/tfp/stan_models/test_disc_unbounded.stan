data {
  int y[10];
}
parameters {
  real<lower=0> lam;
  real log_lam;
}
model {
  target += poisson_lpmf(y | lam);
  target += poisson_log_lpmf(y | log_lam);
}