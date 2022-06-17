functions {
  void badlp(real x) {
    target += normal_lpdf(x|0,1);
    return;
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
