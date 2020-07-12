functions {
  real my_func(real[] y_slice, int start, int end, real mu, real sigma) {
    return normal_lpdf(y_slice | mu, sigma);
  }  
}

parameters {
  vector[1] a[5];
}

model {
  target += reduce_sum(my_func, a, 1, 0.0);
}