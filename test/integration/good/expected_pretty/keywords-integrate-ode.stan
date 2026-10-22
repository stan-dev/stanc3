/* $ stanc --auto-format ../keywords-integrate-ode.stan : [exit 0] */
parameters {
  real integrate_ode_foo;
}
model {
  integrate_ode_foo ~ normal(0, 1);
}

