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
  real rel_tol_f;
  vector[N] abs_tol_f;
  real rel_tol_b;
  vector[N] abs_tol_b;
  real rel_tol_q;
  real abs_tol_q;
  int max_num_steps;
  int num_checkpoints;
  int interpolation_polynomial;
  int solver_f;
  int solver_b;
}

parameters {
  real y;
  
  vector[N] y0;
  real t0;
  array[N] real times;
}

transformed parameters {
  array[M] vector[N] z;

  z = ode_adjoint_tol_ctl(f_0_arg, y0, t0, times, rel_tol_f, abs_tol_f, rel_tol_b, abs_tol_b, rel_tol_q, abs_tol_q,
                           max_num_steps, num_checkpoints, interpolation_polynomial, solver_f, solver_b);

  z = ode_adjoint_tol_ctl(f_1_arg, y0, t0, times, rel_tol_f, abs_tol_f, rel_tol_b, abs_tol_b, rel_tol_q, abs_tol_q,
                           max_num_steps, num_checkpoints, interpolation_polynomial, solver_f, solver_b,
                           y);

  z = ode_adjoint_tol_ctl(f_2_arg, y0, t0, times, rel_tol_f, abs_tol_f, rel_tol_b, abs_tol_b, rel_tol_q, abs_tol_q,
                           max_num_steps, num_checkpoints, interpolation_polynomial, solver_f, solver_b,
                           i, y);
}

model {  
  y ~ normal(0, 1);
}