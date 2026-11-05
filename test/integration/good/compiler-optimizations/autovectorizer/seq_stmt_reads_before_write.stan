// A print statement must stay in the loop and reads v4[n] before the next
// statement overwrites it. The write becomes a vector statement placed after
// the remaining loop. Direction sets are for the loop over n.
// Dependences: anti a->b {Eq}
// Desired outcome: partially vectorized.
//   `for (n in 1:N) print(v4[n]);` then `v4[:] = x`
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v4 = v0;
  for (n in 1 : N) {
    print(v4[n]);
    v4[n] = x[n];
  }
  target += sum(v4);
}
