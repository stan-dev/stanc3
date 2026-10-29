// The first statement reads v2[n] and the second then overwrites it, an
// anti-dependence with distance 0. Both statements vectorize, and the vector
// statements keep the original order. Direction sets are for the loop over n.
// Dependences: anti a->b {Eq}
// Desired outcome: fully vectorized.
//   `mu2[:] = v2 + 1` then `v2[:] = x`
// The read must happen before the write, as in Allen and Kennedy p. 508.
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
