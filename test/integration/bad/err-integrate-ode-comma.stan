transformed data {
  array[10,10] real y;
  real y0;
  real t0;
  array[10] real ts;
  array[3] real theta;
  array[2] real x;
  array[4] int x_int;
  y = integrate_ode_rk45(foo,y0,t0,ts,theta,x x_int);
}
model {
}
