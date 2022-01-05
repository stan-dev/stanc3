functions {
  real foo_lpdf(array[] real y_slice, int start, int end) {
    return normal_lpdf(y_slice| 0, 1);
  }
}
transformed parameters {
    real p;
    array[5] real ya;
    real y = reduce_sum(foo_lupdf,ya,1);
}
