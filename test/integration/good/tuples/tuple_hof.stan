functions {
  real fun(array[] real y_slice, int start, int end,
           tuple(real, array[] int) m) {
    return sum(y_slice) * m.1;
  }
}
data {
  int N;
  array[N] real data_y;
  tuple(real, array[N] int) data_m;
}
transformed data {
  real sum1 = reduce_sum(fun, data_y, 1, data_m);
}
parameters {
  array[N] real param_y;
}
transformed parameters {
  real sum2 = reduce_sum(fun, param_y, 1, data_m);
}
