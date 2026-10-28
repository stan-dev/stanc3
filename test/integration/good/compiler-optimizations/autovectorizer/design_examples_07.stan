// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 7. Carried but acyclic: a->b {Lt}, distance 1. Legal to distribute, and
// offset widening (section 7.6) expresses it.
// => v7[2:N] = x[2:N];  y7[2:N] = v7[1:(N-1)];   (order forced by a->b)
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v7 = v0;
  vector[N] y7 = v0;
  for (n in 2 : N) {
    v7[n] = x[n];
    y7[n] = v7[n - 1];
  }
  target += sum(v7) + sum(y7);
}
