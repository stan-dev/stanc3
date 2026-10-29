// An inner loop over `j` copies `w1[i, j] = y[i, j]` and `w4[j, i] = z[i, j]`,
// where `y[i]` is a vector and `z[i]` is a row_vector. The row slice of `w1` is
// a row_vector but its right hand side is a vector, and the column slice of
// `w4` is a vector but its right hand side is a row_vector.
// Desired outcome: left as a loop.
// The widened left and right hand sides must have the same type, so neither
// assignment is rewritten.
data {
  int<lower=0> N;
}
parameters {
  array[N] vector[N] y;
  array[N] row_vector[N] z;
}
model {
  matrix[N, N] w1;
  matrix[N, N] w4;
  for (i in 1 : N) {
    for (j in 1 : N) {
      w1[i, j] = y[i, j];
      w4[j, i] = z[i, j];
    }
  }
}
