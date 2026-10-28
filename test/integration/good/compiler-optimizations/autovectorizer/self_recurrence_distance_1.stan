// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
//
// 6. Recurrence: self-edge with {Lt}, distance 1. Sequential.
// => unchanged
data {
  int<lower=2> N;
  vector[N] u;
  vector[N] v0;
}
model {
  vector[N] v6 = v0;
  for (n in 2 : N) {
    v6[n] = v6[n - 1] + u[n];
  }
  target += sum(v6);
}
