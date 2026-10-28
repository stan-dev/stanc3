// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
//
// 8. Scalar temporary: t8 has subs = [], so a<->b {Lt,Eq,Gt}: one SCC.
// => unchanged (until scalar expansion, section 7.9 item 2)
data {
  int<lower=2> N;
  vector[N] x;
}
model {
  vector[N] mu8;
  for (n in 1 : N) {
    real t8 = 2 * x[n];
    mu8[n] = t8 + 1;
  }
  target += sum(mu8);
}
