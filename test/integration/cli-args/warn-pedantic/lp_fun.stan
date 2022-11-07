functions {
  void test_lp(real r) {
     target += normal_lpdf(r | 0, 1);
  }
}
parameters {
  real y;
}
transformed parameters {
  test_lp(y);
}
model {
  y ~ normal(0, 1);
}
