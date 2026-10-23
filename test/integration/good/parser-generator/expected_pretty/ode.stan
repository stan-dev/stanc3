/* $ stanc --auto-format ../ode.stan : [exit 0] */
functions {
  array[] real sho(real t, array[] real y, array[] real theta,
                   array[] real x, array[] int x_int) {
    array[2] real dydt;
    dydt[1] = y[2];
    dydt[2] = -y[1] - theta[1] * y[2];
    return dydt;
  }
}
data {
  int<lower=1> T;
  array[2] real y0;
  real t0;
  array[T] real ts;
  array[1] real theta;
}
transformed data {
  array[0] real x;
  array[0] int x_int;
}
model {
  
}
generated quantities {
  array[T, 2] real y_hat;
  y_hat = integrate_ode(sho, y0, t0, ts, theta, x, x_int);
  
  // add measurement error
  for (t in 1 : T) {
    y_hat[t, 1] = y_hat[t, 1] + normal_rng(0, 0.1);
    y_hat[t, 2] = y_hat[t, 2] + normal_rng(0, 0.1);
  }
}

/*
warning: integrate_ode is deprecated and will be removed in Stan 3.0. Use ode_rk45 instead.
    ┌─ ../ode.stan:26:11
 25 │    array[T, 2] real y_hat;
 26 │    y_hat = integrate_ode(sho, y0, t0, ts, theta, x, x_int);
    │            ^^^^^^^^^^^^^ Use ode_rk45 instead.
 27 │    
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
*/
