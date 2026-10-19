// A condition on theta[2] must not depend on the parameter a, because only
// theta[1] gets a value from a.

// Pedantic mode also warns that a has 2 priors. A factor is a prior of a
// parameter if it connects to that parameter and to no data. The second
// target += 1 runs only if theta[1] > 0, and theta[1] = a. Thus that factor
// and a ~ normal(0, 1) are the two priors of a.



parameters {
  real a;
}
model {
  vector[2] theta;
  theta[1] = a;
  theta[2] = 1;
  // theta[2] = 1 does not depend on a, so there is no warning.
  if (theta[2] > 0) {
    target += 1;
  }
  // theta[1] = a depends on a, so there is a warning.
  if (theta[1] > 0) {
    target += 1;
  }
  a ~ normal(0, 1);
}
