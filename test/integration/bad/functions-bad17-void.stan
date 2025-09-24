functions {
  void bizbuz_lpdf(vector x) {
   print(x);
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
