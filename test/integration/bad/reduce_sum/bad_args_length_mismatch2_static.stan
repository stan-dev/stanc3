functions {
  real my_func(real[] y_slice, int start, int end, real mu) {
    return normal_lpdf(y_slice | mu, 0.0);
  }  
}

parameters {
  real a[5];
}

model {
  target += reduce_sum_static(my_func, a, 1, 0.0, 0.0);
}