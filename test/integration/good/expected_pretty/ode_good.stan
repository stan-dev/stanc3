/* $ stanc --auto-format ../ode_good.stan : [exit 0] */
functions {
  array[] real harm_osc_ode(real t,
                            array[] real y, // state
                            array[] real theta, // parameters
                            array[] real x, // data
                            array[] int x_int) {
    // integer data
    array[2] real dydt;
    dydt[1] = x[1] * y[2];
    dydt[2] = -y[1] - theta[1] * y[2];
    return dydt;
  }
}
data {
  array[2] real y0;
  real t0;
  array[10] real ts;
  array[1] real x;
  array[0] int x_int;
  array[10, 2] real y;
}
parameters {
  array[1] real theta;
  real<lower=0> sigma;
}
transformed parameters {
  array[10, 2] real y_hat;
  y_hat = integrate_ode(
                        harm_osc_ode, // system
                        y0, // initial state
                        t0, // initial time
                        ts, // solution times
                        theta, // parameters
                        x, // data
                        x_int); // integer data
  y_hat = integrate_ode_rk45(
            harm_osc_ode, // system
            y0, // initial state
            t0, // initial time
            ts, // solution times
            theta, // parameters
            x, // data
            x_int); // integer data
  y_hat = integrate_ode_bdf(
            harm_osc_ode, // system
            y0, // initial state
            t0, // initial time
            ts, // solution times
            theta, // parameters
            x, // data
            x_int); // integer data
  y_hat = integrate_ode_adams(
            harm_osc_ode, // system
            y0, // initial state
            t0, // initial time
            ts, // solution times
            theta, // parameters
            x, // data
            x_int); // integer data
  y_hat = integrate_ode_rk45(
            harm_osc_ode, // system
            y0, // initial state
            t0, // initial time
            ts, // solution times
            theta, // parameters
            x, // data
            x_int, 0.01, 0.01, 10); // integer data
  y_hat = integrate_ode_bdf(
            harm_osc_ode, // system
            y0, // initial state
            t0, // initial time
            ts, // solution times
            theta, // parameters
            x, // data
            x_int, 0.01, 0.01, 10); // integer data
  y_hat = integrate_ode_adams(
            harm_osc_ode, // system
            y0, // initial state
            t0, // initial time
            ts, // solution times
            theta, // parameters
            x, // data
            x_int, 0.01, 0.01, 10); // integer data
}
model {
  for (t in 1 : 10) 
    y[t] ~ normal(y_hat[t], sigma); // independent normal noise
}

/*
warning: integrate_ode is deprecated and will be removed in Stan 3.0. Use ode_rk45 instead.
    ┌─ ../ode_good.stan:28:11
 27 │    array[10, 2] real y_hat;
 28 │    y_hat = integrate_ode(harm_osc_ode, // system
    │            ^^^^^^^^^^^^^ Use ode_rk45 instead.
 29 │                          y0, // initial state
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
warning: integrate_ode_rk45 is deprecated and will be removed in Stan 3.0. Use ode_rk45 instead.
    ┌─ ../ode_good.stan:35:11
 34 │                          x_int); // integer data
 35 │    y_hat = integrate_ode_rk45(harm_osc_ode, // system
    │            ^^^^^^^^^^^^^^^^^^ Use ode_rk45 instead.
 36 │                               y0, // initial state
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
warning: integrate_ode_bdf is deprecated and will be removed in Stan 3.0. Use ode_bdf instead.
    ┌─ ../ode_good.stan:42:11
 41 │                               x_int); // integer data
 42 │    y_hat = integrate_ode_bdf(harm_osc_ode, // system
    │            ^^^^^^^^^^^^^^^^^ Use ode_bdf instead.
 43 │                              y0, // initial state
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
warning: integrate_ode_adams is deprecated and will be removed in Stan 3.0. Use ode_adams instead.
    ┌─ ../ode_good.stan:49:11
 48 │                              x_int); // integer data
 49 │    y_hat = integrate_ode_adams(harm_osc_ode, // system
    │            ^^^^^^^^^^^^^^^^^^^ Use ode_adams instead.
 50 │                                y0, // initial state
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
warning: integrate_ode_rk45 is deprecated and will be removed in Stan 3.0. Use ode_rk45 instead.
    ┌─ ../ode_good.stan:56:11
 55 │                                x_int); // integer data
 56 │    y_hat = integrate_ode_rk45(harm_osc_ode, // system
    │            ^^^^^^^^^^^^^^^^^^ Use ode_rk45 instead.
 57 │                               y0, // initial state
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
warning: integrate_ode_bdf is deprecated and will be removed in Stan 3.0. Use ode_bdf instead.
    ┌─ ../ode_good.stan:63:11
 62 │                               x_int, 0.01, 0.01, 10); // integer data
 63 │    y_hat = integrate_ode_bdf(harm_osc_ode, // system
    │            ^^^^^^^^^^^^^^^^^ Use ode_bdf instead.
 64 │                              y0, // initial state
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
warning: integrate_ode_adams is deprecated and will be removed in Stan 3.0. Use ode_adams instead.
    ┌─ ../ode_good.stan:70:11
 69 │                              x_int, 0.01, 0.01, 10); // integer data
 70 │    y_hat = integrate_ode_adams(harm_osc_ode, // system
    │            ^^^^^^^^^^^^^^^^^^^ Use ode_adams instead.
 71 │                                y0, // initial state
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
*/
