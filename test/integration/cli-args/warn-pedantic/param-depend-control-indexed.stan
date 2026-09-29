// Control flow that depends on one element of a container must not be
// reported as depending on a parameter written to a different element.
// Reaching definitions are keyed by name; the dependency graph consults the
// subscripts (design-docs/active/vectorize-loop-fission.md section 7.7).
parameters {
  real a;
}
model {
  vector[2] theta;
  theta[1] = a;
  theta[2] = 1;
  // theta[2] is data-only: no warning.
  if (theta[2] > 0) {
    target += 1;
  }
  // theta[1] carries the parameter: warning.
  if (theta[1] > 0) {
    target += 1;
  }
  a ~ normal(0, 1);
}
