// t.1 = a and t.2 = 1, so only the condition on t.1 should warn.
// The prior count of a should be 2.
parameters {
  real a;
}
model {
  tuple(real, real) t;
  t.1 = a;
  t.2 = 1;
  // no warning, because t.2 = 1 does not depend on a
  if (t.2 > 0) {
    target += 1;
  }
  // warning, because t.1 = a
  if (t.1 > 0) {
    target += 1;
  }
  a ~ normal(0, 1);
}
