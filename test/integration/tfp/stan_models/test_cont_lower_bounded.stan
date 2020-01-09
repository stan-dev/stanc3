parameters {
  real<lower=3> p;
}
model {
  target += pareto_lpdf(p | 3, 5);
}
