functions {
  real my_func_8(int start, int end, real[] y_slice, real mu, real sigma, real dummy, real dummy1, real dummy2, real dummy3, real dummy4, real dummy5) {
    return normal_lpdf(y_slice | mu, sigma);
  }
  real my_func_7(int start, int end, real[] y_slice, real mu, real sigma, real dummy, real dummy1, real dummy2, real dummy3, real dummy4) {
    return normal_lpdf(y_slice | mu, sigma);
  }
  real my_func_6(int start, int end, real[] y_slice, real mu, real sigma, real dummy, real dummy1, real dummy2, real dummy3) {
    return normal_lpdf(y_slice | mu, sigma);
  }
  real my_func_5(int start, int end, real[] y_slice, real mu, real sigma, real dummy, real dummy1, real dummy2) {
    return normal_lpdf(y_slice | mu, sigma);
  }
  real my_func_4(int start, int end, real[] y_slice, real mu, real sigma, real dummy, real dummy1) {
    return normal_lpdf(y_slice | mu, sigma);
  }
  real my_func_3(int start, int end, real[] y_slice, real mu, real sigma, real dummy) {
    return normal_lpdf(y_slice | mu, sigma);
  }
  real my_func_2(int start, int end, real[] y_slice, real mu, real sigma) {
    return normal_lpdf(y_slice | mu, sigma);
  }
  real my_func_1(int start, int end, real[] y_slice, real mu) {
    return normal_lpdf(y_slice | mu, 1.0);
  }
  real my_func_0(int start, int end, real[] y_slice) {
    return normal_lpdf(y_slice | 0.0, 1.0);
  }
  real my_func_0a(int start, int end, real[] y_slice) {
    return normal_lpdf(y_slice | 0.0, 1.0);
  }
}

parameters {
  real a[5];
}

model {
  target += reduce_sum(my_func_8, a, 1, 0.0, 1.0, 2.0, 3.0, 3.0, 3.0, 3.0, 3.0);
  target += reduce_sum(my_func_7, a, 1, 0.0, 1.0, 2.0, 3.0, 3.0, 3.0, 3.0);
  target += reduce_sum(my_func_6, a, 1, 0.0, 1.0, 2.0, 3.0, 3.0, 3.0);
  target += reduce_sum(my_func_5, a, 1, 0.0, 1.0, 2.0, 3.0, 3.0);
  target += reduce_sum(my_func_4, a, 1, 0.0, 1.0, 2.0, 3.0);
  target += reduce_sum(my_func_3, a, 1, 0.0, 1.0, 2.0);
  target += reduce_sum(my_func_2, a, 1, 0.0, 1.0);
  target += reduce_sum(my_func_1, a, 1, 0.0);
  target += reduce_sum(my_func_0a, a, 1) + reduce_sum(my_func_0a, a, 1);
}