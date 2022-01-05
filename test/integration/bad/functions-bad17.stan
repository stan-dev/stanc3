functions {
  vector bizbuz_lpdf(vector x) {
    return exp(x);
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
