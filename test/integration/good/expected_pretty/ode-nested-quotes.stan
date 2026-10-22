/* $ stanc --auto-format ../ode-nested-quotes.stan : [exit 0] */
/**
 * Minimal example derived from jimsinmss example brought
 * up on stan-users mailing list, Feb 2015.
 */
functions {
  array[] real foo(real t, array[] real y, array[] real theta,
                   array[] real x_r, array[] int x_i) {
    return rep_array(1.0, 1);
  }
}
transformed data {
  real y;
  array[2, 2] real t;
  y = integrate_ode(foo, rep_array(1.0, 1), 1.0, t[1], rep_array(1.0, 1),
                    rep_array(1.0, 1), rep_array(1, 1))[1, 1];
}
model {
  
}

/*
warning: integrate_ode is deprecated and will be removed in Stan 3.0. Use ode_rk45 instead.
    ┌─ ../ode-nested-quotes.stan:14:7
 13 │    array[2, 2] real t;
 14 │    y = integrate_ode(foo, rep_array(1.0, 1), 1.0, t[1], rep_array(1.0, 1),
    │        ^^^^^^^^^^^^^ Use ode_rk45 instead.
 15 │                      rep_array(1.0, 1), rep_array(1, 1))[1, 1];
    = The new interface is slightly different, see:
      https://mc-stan.org/users/documentation/case-studies/convert_odes.html
*/
