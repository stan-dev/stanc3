functions {
  array[] real harm_osc_ode(real t,
                      array[] real y,         // state
                      array[] real theta,     // parameters
                      array[] real x,         // data
                      array[] int x_int) {    // integer data
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
  array[1] vector[3] x;
  array[0] int x_int;
  array[10,2] real y;
}
parameters {
  array[1] real theta;
  real<lower=0> sigma;
}
transformed parameters {
  array[10,2] real y_hat;
  y_hat = integrate_ode_rk45(harm_osc_ode,  // system
                     y0,            // initial state
                     t0,            // initial time
                     ts,            // solution times
                     theta,         // parameters
                     x,             // data
                     x_int);        // integer data

}
model {
  for (t in 1:10)
    y[t] ~ normal(y_hat[t], sigma);  // independent normal noise
}
