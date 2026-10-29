// The loop assigns `v[n] = r[n] * 2` where `v` and `r` are row_vectors. The
// widened slices keep the row_vector type, so the vector statement assigns a
// row_vector to a row_vector slice.
// Desired outcome: fully vectorized.
//   `v[1:N] = r[1:N] * 2`
data {
  int<lower=0> N;
  row_vector[N] r;
}
parameters {
  real<lower=0> sigma;
}
model {
  row_vector[N] v;
  for (n in 1 : N) {
    v[n] = r[n] * 2;
  }
  target += normal_lpdf(v | 0, sigma);
}
