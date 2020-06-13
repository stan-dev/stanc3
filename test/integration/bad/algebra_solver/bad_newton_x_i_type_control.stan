functions {
  vector algebra_system (vector y,
                         vector theta,
                         real[] x_r,
                         int[] x_i) {
    vector[2] f_y;
    f_y[1] = y[1] - theta[1];
    f_y[2] = y[2] - theta[2];
    return f_y;
  }
}


data {

}

transformed data {
  vector[2] y;
  real x_r[0];
  real x_i[0];
}

parameters {
  vector[2] theta_p;
  real dummy_parameter;
}

transformed parameters {
  vector[2] y_s_p;
  y_s_p = algebra_solver_newton(algebra_system, y, theta_p, x_r, x_i, 0.01, 0.01, 10);
}

model {
  dummy_parameter ~ normal(0, 1);
}
