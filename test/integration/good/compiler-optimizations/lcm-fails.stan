data {
  int<lower=0> J;          // number of schools
  real y[J];               // estimated treatment effect (school j)
}
parameters {
  real theta[J];
}
model {
  y ~ normal(theta,1);
}
