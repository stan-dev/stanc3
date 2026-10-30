// An assignment loop `v[n] = x[n] * w[n] / mu[n]` over three vectors. The
// scalar product and quotient become elementwise `.*` and `./` on the whole
// vectors.
// Desired outcome: fully vectorized.
//   `v[1:N] = (x .* w) ./ mu`
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] w;
}
parameters {
  vector<lower=0>[N] mu;
}
model {
  vector[N] v;
  for (n in 1 : N) {
    v[n] = x[n] * w[n] / mu[n];
  }
  target += normal_lpdf(v | 0, 1);
}
