functions {
  array[] real harm_osc_ode(real t, // time
                            array[] real y, // state
                            array[] real theta, // parameters
                            array[] real x, // data
                            array[] int x_int) {
    // integer data
    array[size(y)] real dydt;
    // ... set dydt at state y and time t ...
    return dydt;
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
}

