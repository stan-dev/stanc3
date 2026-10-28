// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
//
// 9a. Whole-vector read of a written variable: subs = [] -> confused ->
// sequential.
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v9a = v0;
  for (n in 1 : N) {
    v9a[n] = x[n];
    target += sum(v9a);
  }
}
