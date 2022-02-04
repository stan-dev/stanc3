data {
  complex<lower=1, upper=5> x;
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
