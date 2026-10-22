/* $ stanc --standalone-functions --allow-undefined --auto-format ../integrate.stan : [exit 0] */
functions {
  vector integrand(vector x) {
    return exp(-square(x));
  }
  
  array[] real integrand_ode(real r, array[] real f, array[] real theta,
                             array[] real x_r, array[] int x_i) {
    array[1] real df_dx;
    real x = logit(r);
    df_dx[1] = exp(-square(x)) * 1 / (r * (1 - r));
    return (df_dx);
  }
  
  real ode_integrate() {
    array[0] int x_i;
    // ok:
    //return(integrate_ode_rk45(integrand_ode, rep_array(0.0, 1),
    //1E-5, rep_array(1.0-1E-5, 1), rep_array(0.0, 0), rep_array(0.0,
    //0), x_i)[1,1]);
    // not ok
    return (integrate_ode_bdf(integrand_ode, rep_array(0.0, 1), 1E-5,
              rep_array(1.0 - 1E-5, 1), rep_array(0.0, 0), rep_array(0.0, 0),
              x_i)[1, 1]);
  }
}
data {
  
}
model {
  
}

/*
warning: integrate_ode_bdf is deprecated and will be removed in Stan 3.0. Use ode_bdf instead.
    ┌─ ../integrate.stan:21:13
 20 │      // not ok
 21 │      return (integrate_ode_bdf(integrand_ode, rep_array(0.0, 1), 1E-5,
    │              ^^^^^^^^^^^^^^^^^ Use ode_bdf instead.
 22 │                                rep_array(1.0 - 1E-5, 1), rep_array(0.0, 0),
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
*/
