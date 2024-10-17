functions {

  real algebra_system_lpdf(vector x, vector y, array[] real dat,
                        array[] int dat_int) {
    vector[2] f_x;
    f_x[1] = x[1] - y[1];
    f_x[2] = x[2] - y[2];
    return f_x[1];
  }
}
data {

}
transformed data {
  vector[2] x;
  vector[2] y;
  array[0] real dat;
  array[0] int dat_int;

}
parameters {
  vector[2] x_p;
  vector[2] y_p;
}
transformed parameters {
  vector[2] theta_p;

  theta_p = solve_newton(algebra_system_lpdf, x, y, dat, dat_int);

}
