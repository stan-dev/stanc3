functions {
  array[] real foo(real t) {
    array[0] real dydt;
    return dydt;
  }

  tuple(real, array[] real) foo(vector x) {
    tuple(real, array[2] real) y;
    return y;
  }

  matrix empty_user_func() {
    matrix[10, 10] some_mat;
    return some_mat;
  }
}
parameters {
  matrix[10, 10] row_soa;
  real theta;
}
transformed parameters {
  matrix[10, 10] empty_user_func_aos = empty_user_func();
  matrix[10, 10] int_aos_mul_aos = rows(row_soa) * empty_user_func_aos;
}
model {
  target += sum(foo(theta));
  target += foo([theta]').1;
}
