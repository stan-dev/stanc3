/* $ stanc --auto-format --canonicalize braces,parentheses ../deprecated.stan : [exit 0] */
functions {
  array[] real sho(real t, array[] real y, array[] real theta,
                   array[] real x, array[] int x_int);
  array[] real sho(real t, array[] real y, array[] real theta,
                   array[] real x, array[] int x_int) {
    array[2] real dydt;
    dydt[1] = y[2];
    dydt[2] = -y[1] - theta[1] * y[2];
    return dydt;
  }
}
data {
  int<lower=1> N;
  array[N] real x_quad;
}
transformed data {
  int a = -12;
  real b = 1.5;
  int c = abs(a);
  real d = abs(b);
  array[0] int x_i;
  array[0] real x_r;
  array[5, 5] real idxs;
  idxs[1][ : ] = idxs[1][ : ];
}
parameters {
  real x;
  array[3] real theta;
}
model {
  
}
generated quantities {
  array[2] real y0 = {1.0, 2.0};
  array[3] real ts = {0.5, 1.0, 2.0};
  array[3, 2] real y_hat = integrate_ode(sho, y0, 0.0, ts, theta, x_r, x_i);
  array[3, 2] real y_hat_45 = integrate_ode_rk45(sho, y0, 0.0, ts, theta,
                                x_r, x_i);
  array[3, 2] real y_hat_bdf = integrate_ode_bdf(sho, y0, 0.0, ts, theta,
                                 x_r, x_i);
  array[3, 2] real y_hat_adams = integrate_ode_adams(sho, y0, 0.0, ts, theta,
                                   x_r, x_i);
}

/*
warning: Assignment of variable to itself.
    ┌─ ../deprecated.stan:24:16
 23 │    array[5,5] real idxs;
 24 │    idxs[1][:] = idxs[1][:];
    │    ----------   ^^^^^^^^^^ Value here.
    │    │
    │    Assigned to itself.
 25 │   }
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ../deprecated.stan:2:16
  1 │  functions {
  2 │    array[] real sho(real t, array[] real y, array[] real theta,
    │                 ^^^ Forward declaration.
  3 │                     array[] real x, array[] int x_int);
warning: integrate_ode is deprecated and will be removed in Stan 3.0. Use ode_rk45 instead.
    ┌─ ../deprecated.stan:36:27
 35 │    array[3] real ts = {0.5, 1.0, 2.0};
 36 │    array[3,2] real y_hat = integrate_ode(sho, y0, 0.0, ts, theta, x_r, x_i );
    │                            ^^^^^^^^^^^^^ Use ode_rk45 instead.
 37 │    array[3,2] real y_hat_45 = integrate_ode_rk45(sho, y0, 0.0, ts, theta, x_r, x_i );
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
warning: integrate_ode_rk45 is deprecated and will be removed in Stan 3.0. Use ode_rk45 instead.
    ┌─ ../deprecated.stan:37:30
 36 │    array[3,2] real y_hat = integrate_ode(sho, y0, 0.0, ts, theta, x_r, x_i );
 37 │    array[3,2] real y_hat_45 = integrate_ode_rk45(sho, y0, 0.0, ts, theta, x_r, x_i );
    │                               ^^^^^^^^^^^^^^^^^^ Use ode_rk45 instead.
 38 │    array[3,2] real y_hat_bdf = integrate_ode_bdf(sho, y0, 0.0, ts, theta, x_r, x_i );
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
warning: integrate_ode_bdf is deprecated and will be removed in Stan 3.0. Use ode_bdf instead.
    ┌─ ../deprecated.stan:38:31
 37 │    array[3,2] real y_hat_45 = integrate_ode_rk45(sho, y0, 0.0, ts, theta, x_r, x_i );
 38 │    array[3,2] real y_hat_bdf = integrate_ode_bdf(sho, y0, 0.0, ts, theta, x_r, x_i );
    │                                ^^^^^^^^^^^^^^^^^ Use ode_bdf instead.
 39 │    array[3,2] real y_hat_adams = integrate_ode_adams(sho, y0, 0.0, ts, theta, x_r, x_i );
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
warning: integrate_ode_adams is deprecated and will be removed in Stan 3.0. Use ode_adams instead.
    ┌─ ../deprecated.stan:39:33
 38 │    array[3,2] real y_hat_bdf = integrate_ode_bdf(sho, y0, 0.0, ts, theta, x_r, x_i );
 39 │    array[3,2] real y_hat_adams = integrate_ode_adams(sho, y0, 0.0, ts, theta, x_r, x_i );
    │                                  ^^^^^^^^^^^^^^^^^^^ Use ode_adams instead.
 40 │  }
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
*/
