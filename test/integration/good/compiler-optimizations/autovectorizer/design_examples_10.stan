// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 10. Distinct literal positions: Independent, both hoist.
// => v10[:, 1] = x;  w10[:] = v10[:, 2];
data {
  int<lower=2> N;
  vector[N] x;
}
model {
  matrix[N, 2] v10 = rep_matrix(0, N, 2);
  vector[N] w10;
  for (n in 1 : N) {
    v10[n, 1] = x[n];
    w10[n] = v10[n, 2];
  }
  target += sum(v10) + sum(w10);
}
