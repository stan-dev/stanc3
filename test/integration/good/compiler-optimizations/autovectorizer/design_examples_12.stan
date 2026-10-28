// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 12. Symbolic offset (k data): both subscripts are Affine {1; {0; [k]}},
// the symbols cancel, a->b {Eq}. Widening shifts the range by k.
// => v12[(1 + k):(N + k)] = x;  y12[:] = v12[(1 + k):(N + k)] * 2;
data {
  int<lower=2> N;
  int<lower=0> k;
  vector[N] x;
}
model {
  vector[N + k] v12 = rep_vector(0, N + k);
  vector[N] y12;
  for (n in 1 : N) {
    v12[n + k] = x[n];
    y12[n] = v12[n + k] * 2;
  }
  target += sum(v12) + sum(y12);
}
