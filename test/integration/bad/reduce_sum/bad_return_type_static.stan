functions {
  int my_func_0(array[] real y_slice, int start, int end) {
    return 5;
  }
}

parameters {
  array[5] real a;
}

model {
  target += reduce_sum_static(my_func_0, a, 1);
}
