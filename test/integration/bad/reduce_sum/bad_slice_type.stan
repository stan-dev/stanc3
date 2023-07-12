functions {
  real my_func(array[] complex y_slice, int start, int end, real mu, real sigma) {
    return normal_lpdf(get_real(y_slice) | mu, sigma) + normal_lpdf(get_imag(y_slice) | mu, sigma);
  }
}

parameters {
  array[10] complex a;
}

model {
  target += reduce_sum(my_func, a, 1, 0.0, 1.0);
}
