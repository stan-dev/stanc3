functions {
  vector algebra_system (array[] real y,
                         vector theta,
                         array[] real x_r,
                         array[] int x_i) {
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
  array[0] real x_r;
  array[0] int x_i;
}

parameters {
  real theta_p;
  real dummy_parameter;
}

transformed parameters {
  vector[2] y_s_p;
  y_s_p = solve_newton(algebra_system, y, theta_p, x_r, x_i, x_i);
}

model {
  dummy_parameter ~ normal(0, 1);
}
