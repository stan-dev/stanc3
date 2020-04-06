functions {
  real my_func_0(int start, int end, real[,,] y_slice) {
    return 5;
  }
}

parameters {
  real a[5,5,5];
}

model {
  target += reduce_sum(my_func_0, a, 1);
}