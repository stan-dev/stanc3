// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
//
// 9c. Container declared in the body: subs = [] -> confused -> sequential.
data {
  int<lower=2> N;
  vector[N] x;
}
model {
  vector[N] y9c;
  for (n in 1 : N) {
    vector[2] t9c;
    t9c[1] = x[n];
    y9c[n] = t9c[1];
  }
  target += sum(y9c);
}
