functions {
  vector f_0_arg(real t, vector z) {
    return z;
  }
  vector f_1_arg(real t, vector z, real a) {
    return z;
  }  
  vector f_2_arg(real t, vector z, int b, real a) {
    return z;
  }
}

data {
  int N;
  int M;
  int i;
}

transformed data {
  real rel_tol;
  real abs_tol;
  int max_num_steps;
}

parameters {
  real y;
  
  vector[N] y0;
  real t0;
  array[N] real times;
}

transformed parameters {
  array[M] vector[N] z;

  z = ode_adjoint_tol(f_0_arg, y0, t0, times, rel_tol, abs_tol, max_num_steps);

  z = ode_adjoint_tol(f_1_arg, y0, t0, times, rel_tol, abs_tol, max_num_steps, y);

  z = ode_adjoint_tol(f_2_arg, y0, t0, times, rel_tol, abs_tol, max_num_steps, i, y);
}

model {  
  y ~ normal(0, 1);
}