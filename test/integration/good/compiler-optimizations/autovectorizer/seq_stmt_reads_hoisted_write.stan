// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Partially vectorized
//
// 3. Sequential statement reads a hoisted write: a->b {Eq}, loop after.
// => mu3[:] = 2 * x;  for (n in 1:N) print(mu3[n]);
data {
  int<lower=2> N;
  vector[N] x;
}
model {
  vector[N] mu3;
  for (n in 1 : N) {
    mu3[n] = 2 * x[n];
    print(mu3[n]);
  }
  target += sum(mu3);
}
