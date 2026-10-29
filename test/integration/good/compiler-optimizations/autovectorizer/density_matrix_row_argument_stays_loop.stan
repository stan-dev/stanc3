// Each iteration adds `normal_lpdf(m[n] | mu, sigma)` where `m[n]` is a row of
// the matrix `m`. The pass only widens int and real arguments, and `m[n]` is
// already a row_vector, so the density is not rewritten.
// Desired outcome: left as a loop.
data {
  int<lower=0> N;
  matrix[N, 2] m;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  for (n in 1 : N) {
    target += normal_lpdf(m[n] | mu, sigma);
  }
}
