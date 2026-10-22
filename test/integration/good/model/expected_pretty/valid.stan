/* $ stanc --auto-format ../valid.stan : [exit 0] */
parameters {
  real x;
}
model {
  target += -0.5 * square(x);
}

