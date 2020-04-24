functions {
  real my_func_0(real[] y_slice, int end) {
    return 5;
  }
}

parameters {
  real a[5];
}

model {
  target += reduce_sum_static(my_func_0, a, 1);
}