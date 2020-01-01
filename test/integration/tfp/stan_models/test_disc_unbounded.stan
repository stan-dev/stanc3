data {
  int y[10];
}
parameters {
  real<lower=0> lam;
  real log_lam;
  real<lower=0, upper=10> nb_alpha;
  real<lower=0, upper=10> nb_beta;
}
model {
  target += poisson_lpmf(y | lam);
  target += poisson_log_lpmf(y | log_lam);
  target += neg_binomial_lpmf(y | nb_alpha, nb_beta);
}