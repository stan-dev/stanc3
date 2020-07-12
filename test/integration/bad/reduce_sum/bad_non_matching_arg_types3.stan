functions {
  real my_func(real[] y_slice, int start, int end, real mu, real sigma) {
    return normal_lpdf(y_slice | mu, sigma);
  }  
}

parameters {
  real a[5];
  vector[5] b;
}

model {
    target += reduce_sum(my_func, a, 1, 0.0, b);
}