parameters {
  real<lower=0> a;
  real b;
  real<lower=0> c;
  real<lower=0> d;
}
model {
  a ~ gamma(0.5, 0.5);
  a ~ gamma(0.1, b);
  a ~ inv_gamma(0.5, 0.5);
  a ~ inv_gamma(0.5, b);
  c ~ gamma(2, 2);
  d ~ gamma(0.4, 0.6);
}
