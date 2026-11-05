// A first-order recurrence in which v6[n] reads v6[n - 1], written by the
// previous iteration. Direction sets are for the loop over n.
// Dependences: true self-edge {Lt}, distance 1
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N] u;
  vector[N] v0;
}
model {
  vector[N] v6 = v0;
  for (n in 2 : N) {
    v6[n] = v6[n - 1] + u[n];
  }
  target += sum(v6);
}
