// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
//
// 5. Both directions through effects: a<->c (effects), a->b, b->c: one
// strongly connected component.
// => unchanged
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v5 = v0;
  for (n in 1 : N) {
    print(v5[n]);
    v5[n] = x[n];
    print(v5[n]);
  }
  target += sum(v5);
}
