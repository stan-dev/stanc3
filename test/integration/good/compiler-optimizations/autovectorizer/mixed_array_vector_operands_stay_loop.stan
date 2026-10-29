// An assignment loop `v[n] = x[n] * u[n]` where `x` is a real array and
// `u` is a vector. Widening would multiply an array by a vector.
// Desired outcome: left as a loop.
// Neither `*` nor `.*` accepts an array and a vector together.
data {
  int<lower=0> N;
  array[N] real x;
}
parameters {
  vector[N] u;
}
model {
  vector[N] v;
  for (n in 1 : N) {
    v[n] = x[n] * u[n];
  }
  target += normal_lpdf(v | 0, 1);
}
