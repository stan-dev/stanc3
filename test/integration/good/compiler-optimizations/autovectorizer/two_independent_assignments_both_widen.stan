// One loop holds two assignments, `v[n] = x[n] + alpha` and `w[n] = y[n] * 2`.
// Neither statement reads what the other writes. Each statement becomes a
// separate vector statement and the loop is removed.
// Desired outcome: fully vectorized.
//   `v[1:N] = x[1:N] + alpha`
//   `w[1:N] = y[1:N] * 2`
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real alpha;
}
model {
  vector[N] v;
  vector[N] w;
  for (n in 1 : N) {
    v[n] = x[n] + alpha;
    w[n] = y[n] * 2;
  }
  target += normal_lpdf(v | w, 1);
}
