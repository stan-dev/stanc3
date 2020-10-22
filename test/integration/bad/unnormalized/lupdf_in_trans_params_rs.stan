functions {
  real foo_lpdf(real[] y_slice, int start, int end) {
    return normal_lpdf(y_slice| 0, 1);    
  }
}
transformed parameters {
    real p;
    real ya[5];
    real y = reduce_sum(foo_lupdf,ya,1);
}