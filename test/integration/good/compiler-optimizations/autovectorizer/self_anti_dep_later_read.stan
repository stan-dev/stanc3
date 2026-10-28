// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 11. Self anti-dependence: read of a later iteration's element. No edge.
// => a11[1:(N-1)] = a11[2:N] + 1;   (deep_copy on the right-hand side makes
//    it safe, section 7.11)
data {
  int<lower=2> N;
  vector[N] a0;
}
model {
  vector[N] a11 = a0;
  for (n in 1 : (N - 1)) {
    a11[n] = a11[n + 1] + 1;
  }
  target += sum(a11);
}
