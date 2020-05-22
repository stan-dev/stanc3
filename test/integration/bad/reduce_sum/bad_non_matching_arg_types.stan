functions {
  real my_func(real[] y_slice, real mu, int start, int end, real sigma) {
    return normal_lpdf(y_slice | mu, sigma);
  }  
}

data {
    int b;
}

parameters {
    real a[5];
}

model {
    target += reduce_sum(my_func, a, 1, 0.0, b);
}