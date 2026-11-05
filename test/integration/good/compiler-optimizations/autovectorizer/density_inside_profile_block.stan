// The loop body is a profile block around `target += normal_lpdf(y[n] | x[n],
// sigma)`. The pass widens statements inside the profile block and keeps the
// profile around the vector statement.
// Desired outcome: fully vectorized.
//   `profile("lik") { target += normal_lpdf(y[1:N] | x[1:N], sigma) }`
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real<lower=0> sigma;
}
model {
  for (n in 1 : N) {
    profile("lik") {
      target += normal_lpdf(y[n] | x[n], sigma);
    }
  }
}
