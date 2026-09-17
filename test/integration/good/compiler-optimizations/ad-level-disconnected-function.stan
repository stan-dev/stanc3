functions {
  array[] real foo(real t) {
    array[0] real dydt;
    return dydt;
  }

  tuple(real, array[] real) foo(vector x) {
    tuple(real, array[2] real) y;
    return y;
  }
}
parameters {
  real theta;
}
model {
  target += sum(foo(theta));
  target += foo([theta]').1;
}
