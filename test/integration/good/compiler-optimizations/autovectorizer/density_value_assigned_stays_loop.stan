// The loop assigns a density value to each element with
// `v[n] = normal_lpdf(y[n] | mu, 1)`.
// Desired outcome: left as a loop.
// A vectorized density returns one summed value, but the loop stores a
// separate value per element.
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real mu;
}
model {
  vector[N] v;
  for (n in 1 : N) {
    v[n] = normal_lpdf(y[n] | mu, 1);
  }
  target += sum(v);
}
