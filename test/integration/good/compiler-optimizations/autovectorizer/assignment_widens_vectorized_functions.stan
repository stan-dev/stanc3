// An assignment loop `v[n] = exp(x[n]) + sqrt(square(u[n]))`. Each unary
// function has a vector signature, so the calls nest on whole vectors.
// Desired outcome: fully vectorized.
//   `v[1:N] = exp(x) + sqrt(square(u))`
data {
  int<lower=0> N;
  vector[N] x;
}
parameters {
  vector[N] u;
}
model {
  vector[N] v;
  for (n in 1 : N) {
    v[n] = exp(x[n]) + sqrt(square(u[n]));
  }
  target += normal_lpdf(v | 0, 1);
}
