// The loop writes and reads v9b through the index array idx[n]. The
// subscript varies in a way the dependence test cannot model, so the
// accesses may overlap across iterations. Direction sets are for the loop
// over n.
// Dependences: Varying subscript -> confused -> sequential
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  array[N] int<lower=1, upper=N> idx;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v9b = v0;
  vector[N] y9b;
  for (n in 1 : N) {
    v9b[idx[n]] = x[n];
    y9b[n] = v9b[idx[n]];
  }
  target += sum(v9b) + sum(y9b);
}
