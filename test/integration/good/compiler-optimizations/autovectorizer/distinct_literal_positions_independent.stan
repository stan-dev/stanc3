// The loop writes column 1 of v10 and reads column 2. The literal column
// indices differ, so the accesses never overlap and both statements become
// vector statements. Direction sets are for the loop over n.
// Dependences: independent (distinct literal positions), no edges
// Desired outcome: fully vectorized.
//   `v10[:, 1] = x` then `w10[:] = v10[:, 2]`
data {
  int<lower=2> N;
  vector[N] x;
}
model {
  matrix[N, 2] v10 = rep_matrix(0, N, 2);
  vector[N] w10;
  for (n in 1 : N) {
    v10[n, 1] = x[n];
    w10[n] = v10[n, 2];
  }
  target += sum(v10) + sum(w10);
}
