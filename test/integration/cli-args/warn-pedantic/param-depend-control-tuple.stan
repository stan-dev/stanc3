// A condition on t.2 must not depend on the parameter a, because only t.1
// gets a value from a.
parameters {
  real a;
}
model {
  tuple(real, real) t;
  t.1 = a;
  t.2 = 1;
  // t.2 = 1 does not depend on a, so there is no warning.
  if (t.2 > 0) {
    target += 1;
  }
  // t.1 = a depends on a, so there is a warning.
  if (t.1 > 0) {
    target += 1;
  }
  a ~ normal(0, 1);
}
