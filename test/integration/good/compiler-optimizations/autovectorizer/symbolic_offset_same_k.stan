// The write v12[n + k] and the read v12[n + k] use the same symbolic offset
// k, a data variable. The symbols cancel, so the dependence is loop
// independent and both statements become vector statements with ranges
// shifted by k. Direction sets are for the loop over n.
// Dependences: both subscripts are Affine {1; {0; [k]}}, true a->b {Eq}
// Desired outcome: fully vectorized.
//   `v12[(1 + k):(N + k)] = x` then `y12[:] = v12[(1 + k):(N + k)] * 2`
data {
  int<lower=2> N;
  int<lower=0> k;
  vector[N] x;
}
model {
  vector[N + k] v12 = rep_vector(0, N + k);
  vector[N] y12;
  for (n in 1 : N) {
    v12[n + k] = x[n];
    y12[n] = v12[n + k] * 2;
  }
  target += sum(v12) + sum(y12);
}
