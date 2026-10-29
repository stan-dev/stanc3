// A tilde over a user-defined density `foo_lpdf` with a real signature.
// The vectorizer only widens built-in densities, whose signatures accept
// containers.
// Desired outcome: left as a loop.
// A user-defined density has no vectorized signature to call.
functions {
  real foo_lpdf(real y, real mu) {
    return -square(y - mu);
  }
}
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real mu;
}
model {
  for (n in 1 : N) {
    y[n] ~ foo(mu);
  }
}
