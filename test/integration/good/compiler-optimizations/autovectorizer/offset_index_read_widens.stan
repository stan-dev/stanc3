// An assignment loop `w[n] = x[n - 1]` that reads the data vector at an
// offset from the loop variable.
// Desired outcome: fully vectorized.
//   `w[2:N] = x[(2 - 1):(N - 1)]`
// The subscript `n - 1` is affine in the loop variable, so the read becomes
// the loop's range shifted by the offset.
data {
  int<lower=0> N;
  vector[N] x;
}
model {
  vector[N] w;
  for (n in 2 : N) {
    w[n] = x[n - 1];
  }
  target += normal_lpdf(w | 0, 1);
}
