functions {
  real[] sho(real t,
             real[] y,
             real[] theta,
             real[] x,
             int[] x_int) {
    real dydt[2];
    dydt[1] = y[2];
    dydt[2] = -y[1] - theta[1] * y[2];
    return dydt;
  }
  real integrand(real x, real xc, real[] theta, real[] x_r, int[] x_i) {
    return 0.0;
  }
  vector foo(vector shared_params, vector job_params,
             real[] data_r, int[] data_i) {
    return [1, 2, 3]';
  }
  vector goo(vector shared_params, vector job_params,
             real[] data_r, int[] data_i) {
    return [4, 5, 6]';
  }
  real map_rectfake(real x) {
    return 2 * x;
  }
  vector algebra_system(vector x,
                        vector y,
                        real[] dat,
                        int[] dat_int) {
    vector[2] f_x;
    f_x[1] = x[1] - y[1];
    f_x[2] = x[2] - y[2];
    return f_x;
  }
}
data {
  int<lower=1> T;
  real y0_d[2];
  real t0;
  real ts[T];
  real theta_d[1];
  real x[0];
  int x_int[0];
  real x_d_r[0];
  int x_d_i[0];
  vector[3] shared_params_d;
  vector[3] job_params_d[3];
  real data_r[3, 3];
  int data_i[3, 3];
}
parameters {
  real y0_p[2];
  real theta_p[1];
  real x_p[1];
  vector[2] x_p_v;
  vector[3] shared_params_p;
  vector[3] job_params_p[3];
  real x_r;
}
transformed parameters {
  real abc1_p = 3;
  real abc2_p = map_rectfake(abc1_p);
  real abc3_p = map_rectfake(12);
  vector[3] y_hat_tp1
      = map_rect(foo, shared_params_p, job_params_d, data_r, data_i);
  vector[3] y_hat_tp2
      = map_rect(foo, shared_params_d, job_params_p, data_r, data_i);
  vector[3] y_hat_tp3
      = map_rect(foo, shared_params_p, job_params_d, data_r, data_i);
  vector[2] theta_p_as;
  vector[2] x_v;
  vector[2] y_v;
  vector[2] y_p;
  theta_p_as = algebra_solver(algebra_system, x_v, y_v, x_d_r, x_d_i);
  theta_p_as = algebra_solver(algebra_system, x_v, y_v, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_p_as = algebra_solver(algebra_system, x_v, y_p, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_p_as = algebra_solver(algebra_system, x_p_v, y_v, x_d_r, x_d_i);

  theta_p_as = algebra_solver(algebra_system, x_p_v, y_v, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_p_as = algebra_solver(algebra_system, x_p_v, y_p, x_d_r, x_d_i);
  theta_p_as = algebra_solver(algebra_system, x_p_v, y_p, x_d_r, x_d_i, 0.01, 0.01, 10);

  theta_p_as = algebra_solver_newton(algebra_system, x_v, y_v, x_d_r, x_d_i);
  theta_p_as = algebra_solver_newton(algebra_system, x_v, y_v, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_p_as = algebra_solver_newton(algebra_system, x_v, y_p, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_p_as = algebra_solver_newton(algebra_system, x_p_v, y_v, x_d_r, x_d_i);

  theta_p_as = algebra_solver_newton(algebra_system, x_p_v, y_v, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_p_as = algebra_solver_newton(algebra_system, x_p_v, y_p, x_d_r, x_d_i);
  theta_p_as = algebra_solver_newton(algebra_system, x_p_v, y_p, x_d_r, x_d_i, 0.01, 0.01, 10);
}

model {
  real y_hat[T,2];
  y_hat = integrate_ode_adams(sho, y0_d, t0, ts, theta_p, x, x_int);
  y_hat = integrate_ode_adams(sho, y0_p, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_adams(sho, y0_p, t0, ts, theta_p, x, x_int);

  y_hat = integrate_ode_adams(sho, y0_d, t0, ts, theta_p, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_adams(sho, y0_p, t0, ts, theta_d, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_adams(sho, y0_p, t0, ts, theta_p, x, x_int, 1e-10, 1e-10, 1e8);

  y_hat = integrate_ode_bdf(sho, y0_d, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_bdf(sho, y0_d, t0, ts, theta_p, x, x_int);
  y_hat = integrate_ode_bdf(sho, y0_p, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_bdf(sho, y0_p, t0, ts, theta_p, x, x_int);

  y_hat = integrate_ode_bdf(sho, y0_d, t0, ts, theta_d, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_bdf(sho, y0_d, t0, ts, theta_p, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_bdf(sho, y0_p, t0, ts, theta_d, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_bdf(sho, y0_p, t0, ts, theta_p, x, x_int, 1e-10, 1e-10, 1e8);

  y_hat = integrate_ode_rk45(sho, y0_d, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_rk45(sho, y0_d, t0, ts, theta_p, x, x_int);
  y_hat = integrate_ode_rk45(sho, y0_p, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_rk45(sho, y0_p, t0, ts, theta_p, x, x_int);

  real y_1d = integrate_1d(integrand, 0, 1, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, 0.0, 1, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, 0, 1.0, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, 0.0, 1.0, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, 0, 1, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, x_r, 1, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, 0, x_r, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, x_r, x_r, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, x_r, 1, x_d_r, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, 0, x_r, x_d_r, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, x_r, x_r, x_d_r, x_d_r, x_d_i);

  real z_1d = integrate_1d(integrand, 0, 1, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, 0.0, 1, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, 0, 1.0, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, 0.0, 1.0, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, 0, 1, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, x_r, 1, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, 0, x_r, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, x_r, x_r, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, x_r, 1, x_d_r, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, 0, x_r, x_d_r, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, x_r, x_r, x_d_r, x_d_r, x_d_i, 1e-8);

  real abc_m = map_rectfake(abc1_p);
}
generated quantities {
  real y_hat[T,2];
  y_hat = integrate_ode_adams(sho, y0_d, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_adams(sho, y0_d, t0, ts, theta_p, x, x_int);
  y_hat = integrate_ode_adams(sho, y0_p, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_adams(sho, y0_p, t0, ts, theta_p, x, x_int);

  y_hat = integrate_ode_adams(sho, y0_d, t0, ts, theta_d, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_adams(sho, y0_d, t0, ts, theta_p, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_adams(sho, y0_p, t0, ts, theta_d, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_adams(sho, y0_p, t0, ts, theta_p, x, x_int, 1e-10, 1e-10, 1e8);

  y_hat = integrate_ode_bdf(sho, y0_d, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_bdf(sho, y0_d, t0, ts, theta_p, x, x_int);
  y_hat = integrate_ode_bdf(sho, y0_p, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_bdf(sho, y0_p, t0, ts, theta_p, x, x_int);

  y_hat = integrate_ode_bdf(sho, y0_d, t0, ts, theta_d, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_bdf(sho, y0_d, t0, ts, theta_p, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_bdf(sho, y0_p, t0, ts, theta_d, x, x_int, 1e-10, 1e-10, 1e8);
  y_hat = integrate_ode_bdf(sho, y0_p, t0, ts, theta_p, x, x_int, 1e-10, 1e-10, 1e8);
  
  y_hat = integrate_ode_rk45(sho, y0_d, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_rk45(sho, y0_d, t0, ts, theta_p, x, x_int);
  y_hat = integrate_ode_rk45(sho, y0_p, t0, ts, theta_d, x, x_int);
  y_hat = integrate_ode_rk45(sho, y0_p, t0, ts, theta_p, x, x_int);

  real y_1d = integrate_1d(integrand, 0, 1, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, 0.0, 1, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, 0, 1.0, x, x_d_r, x_d_i);
  y_1d = integrate_1d(integrand, 0.0, 1.0, x, x_d_r, x_d_i);

  real z_1d = integrate_1d(integrand, 0, 1, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, 0.0, 1, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, 0, 1.0, x, x_d_r, x_d_i, 1e-8);
  z_1d = integrate_1d(integrand, 0.0, 1.0, x, x_d_r, x_d_i, 1e-8);

  real abc1_gq = map_rectfake(12);
  real abc2_gq = map_rectfake(abc1_p);
  vector[3] y_hat_gq
      = map_rect(foo, shared_params_d, job_params_d, data_r, data_i)
        + map_rect(goo, shared_params_d, job_params_d, data_r, data_i);

  vector[3] yy_hat_gq
      = map_rect(goo, shared_params_d, job_params_d, data_r, data_i);

  vector[2] theta_dbl;
  theta_dbl = algebra_solver(algebra_system, x_v, y_v, x_d_r, x_d_i);
  theta_dbl = algebra_solver(algebra_system, x_v, y_v, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_dbl = algebra_solver(algebra_system, x_v, y_p, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_dbl = algebra_solver(algebra_system, x_p_v, y_v, x_d_r, x_d_i);

  theta_dbl = algebra_solver(algebra_system, x_p_v, y_v, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_dbl = algebra_solver(algebra_system, x_p_v, y_p, x_d_r, x_d_i);
  theta_dbl = algebra_solver(algebra_system, x_p_v, y_p, x_d_r, x_d_i, 0.01, 0.01, 10);

  theta_dbl = algebra_solver_newton(algebra_system, x_v, y_v, x_d_r, x_d_i);
  theta_dbl = algebra_solver_newton(algebra_system, x_v, y_v, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_dbl = algebra_solver_newton(algebra_system, x_v, y_p, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_dbl = algebra_solver_newton(algebra_system, x_p_v, y_v, x_d_r, x_d_i);

  theta_dbl = algebra_solver_newton(algebra_system, x_p_v, y_v, x_d_r, x_d_i, 0.01, 0.01, 10);
  theta_dbl = algebra_solver_newton(algebra_system, x_p_v, y_p, x_d_r, x_d_i);
  theta_dbl = algebra_solver_newton(algebra_system, x_p_v, y_p, x_d_r, x_d_i, 0.01, 0.01, 10);
}
