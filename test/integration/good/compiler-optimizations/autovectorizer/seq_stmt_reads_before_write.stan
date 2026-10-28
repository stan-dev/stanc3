// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Partially vectorized
//
// 4. Sequential statement reads before the write: a->b {Eq}, loop before.
// => for (n in 1:N) print(v4[n]);  v4[:] = x;
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v4 = v0;
  for (n in 1 : N) {
    print(v4[n]);
    v4[n] = x[n];
  }
  target += sum(v4);
}
