// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 2. Anti-dependence, distance 0: order kept (Allen and Kennedy p. 508).
// a->b {Eq}
// => mu2[:] = v2 + 1;  v2[:] = x;
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] mu2;
  vector[N] v2 = v0;
  for (n in 1 : N) {
    mu2[n] = v2[n] + 1;
    v2[n] = x[n];
  }
  target += sum(mu2) + sum(v2);
}
