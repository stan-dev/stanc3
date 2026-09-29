// Control flow that depends on one field of a tuple must not be reported as
// depending on a parameter written to a different field.
parameters {
  real a;
}
model {
  tuple(real, real) t;
  t.1 = a;
  t.2 = 1;
  // t.2 is data-only: no warning.
  if (t.2 > 0) {
    target += 1;
  }
  // t.1 carries the parameter: warning.
  if (t.1 > 0) {
    target += 1;
  }
  a ~ normal(0, 1);
}
