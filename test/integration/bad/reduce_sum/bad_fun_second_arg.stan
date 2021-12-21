functions {
  real my_func(array[] real y_slice, int start, real end, real mu, real sigma) {
    return normal_lpdf(y_slice | mu, sigma);
  }
}

parameters {
  array[5] real a;
}

model {
  target += reduce_sum(my_func, a, 1, 0.0);
}
