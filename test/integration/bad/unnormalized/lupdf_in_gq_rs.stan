functions {
  real foo_lpdf(array[] real y_slice, int start, int end) {
    return normal_lpdf(y_slice| 0, 1);
  }
}
parameters {
    real y;
    array[5] real ya;
}
model {
    y ~ normal(0, 1);
}
generated quantities {
    real x = reduce_sum(foo_lupdf,ya,1);
}
