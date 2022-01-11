functions {
  real fun(array[] real y_slice, int start, int end, real m) {
    return sum(y_slice) * m;
  }

  real fun(array[] real y_slice, int start, int end) {
    return sum(y_slice);
  }
}
transformed data {
  int N = 100;
  array[N] real data_y = ones_array(N);

  real sum_1 = reduce_sum(fun, data_y, 1);
  print(sum_1);
  real sum_2 = reduce_sum(fun, data_y, 1, 5);
  print(sum_2);
}
parameters {
   real y;
}
transformed parameters {
   array[N] real param_y = ones_array(N);

   real p_sum_1 = reduce_sum(fun, param_y, 1);
   print(y, " - ", p_sum_1);
   real p_sum_2 = reduce_sum(fun, param_y, 1, y, y);
   print(y, " -- ", p_sum_2);
}
model {
   y ~ std_normal();
}
