parameters {
  real a;
  real<lower=0> b;
  real c;
  real<lower=0> d;
}
model {
  a ~ gamma(2, 2);
  b ~ gamma(2, 2);
  c ~ lognormal(2, 2);
  d ~ lognormal(2, 2);
}
