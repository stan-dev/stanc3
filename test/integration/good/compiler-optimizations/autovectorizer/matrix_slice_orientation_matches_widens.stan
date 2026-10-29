// An inner loop over `j` copies `w2[j, i] = y[i, j]` and `w3[i, j] = z[i, j]`,
// where `y[i]` is a vector and `z[i]` is a row_vector. The column slice of `w2`
// is a vector and the row slice of `w3` is a row_vector. Both slices match
// the right-hand sides and the inner loop becomes two vector statements.
// Desired outcome: partially vectorized.
//   `w2[1:N, i] = y[i, 1:N]`
//   `w3[i, 1:N] = z[i, 1:N]`
// The outer loop over `i` stays because the pass does not widen an index
// that is already a slice.
data {
  int<lower=0> N;
}
parameters {
  array[N] vector[N] y;
  array[N] row_vector[N] z;
}
model {
  matrix[N, N] w2;
  matrix[N, N] w3;
  for (i in 1 : N) {
    for (j in 1 : N) {
      w2[j, i] = y[i, j];
      w3[i, j] = z[i, j];
    }
  }
}
