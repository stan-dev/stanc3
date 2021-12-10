functions {
  real my_func(array[] real y_slice, real mu, int start, int end, real sigma) {
    return normal_lpdf(y_slice | mu, sigma);
  }
}

data {
    int b;
}

parameters {
    array[5] real a;
}

model {
    target += reduce_sum(my_func, a, 1, 0.0, b);
}
