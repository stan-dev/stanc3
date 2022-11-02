functions {
  real algebra_solverfake(real x) {
    return 2 * x;
  }
  vector algebra_system(vector x, vector y, array[] real dat,
                        array[] int dat_int) {
    vector[2] f_x;
    f_x[1] = x[1] - y[1];
    f_x[2] = x[2] - y[2];
    return f_x;
  }
}
data {
  
}
transformed data {
  vector[2] x;
  vector[2] y;
  array[0] real dat;
  array[0] int dat_int;
  vector[2] theta;
  real rel_tol;
  real fun_tol;
  int max_steps;
  
  theta = solve_powell(algebra_system, x, y, dat, dat_int);
  theta = solve_powell_tol(algebra_system, x, rel_tol, fun_tol, max_steps, y,
                           dat, dat_int);
}
parameters {
  vector[2] x_p;
  vector[2] y_p;
  real dummy_parameter;
}
transformed parameters {
  real abc_tp = algebra_solverfake(2.9);
  vector[2] theta_p;
  
  theta_p = solve_powell(algebra_system, x, y, dat, dat_int);
  theta_p = solve_powell_tol(algebra_system, x, rel_tol, fun_tol, max_steps,
                             y, dat, dat_int);
  
  theta_p = solve_powell(algebra_system, x, y_p, dat, dat_int);
  theta_p = solve_powell_tol(algebra_system, x, rel_tol, fun_tol, max_steps,
                             y, dat, dat_int);
  
  theta_p = solve_powell(algebra_system, x_p, y, dat, dat_int);
  theta_p = solve_powell_tol(algebra_system, x_p, rel_tol, fun_tol,
                             max_steps, y, dat, dat_int);
  
  theta_p = solve_powell(algebra_system, x_p, y_p, dat, dat_int);
  theta_p = solve_powell_tol(algebra_system, x_p, rel_tol, fun_tol,
                             max_steps, y_p, dat, dat_int);
}
model {
  dummy_parameter ~ normal(0, 1);
}
