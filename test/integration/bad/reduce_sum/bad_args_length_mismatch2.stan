functions {
  real my_func(int start, int end, real[] y_slice, real mu) {
    return normal_lpdf(y_slice | mu, 0.0);
  }  
}

parameters {
  real a[5];
}

model {
  target += reduce_sum(my_func, a, 1, 0.0, 0.0);
}