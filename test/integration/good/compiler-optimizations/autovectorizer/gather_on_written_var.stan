// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
//
// 9b. Gather on a written variable: Varying -> confused -> sequential.
data {
  int<lower=2> N;
  array[N] int<lower=1, upper=N> idx;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v9b = v0;
  vector[N] y9b;
  for (n in 1 : N) {
    v9b[idx[n]] = x[n];
    y9b[n] = v9b[idx[n]];
  }
  target += sum(v9b) + sum(y9b);
}
