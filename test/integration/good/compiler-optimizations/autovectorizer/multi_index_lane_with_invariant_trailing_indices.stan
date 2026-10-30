// The loop assigns `a[i, 1, 1] = x[i]` into a three dimensional int array.
// The loop variable is the first index and the trailing indices are constants,
// so the first index becomes a slice and the other two stay as they are.
// Desired outcome: fully vectorized.
//   `a[1:N, 1, 1] = x[1:N]`
data {
  int<lower=0> N;
  array[N] int x;
}
model {
  array[N, 2, 2] int a;
  for (i in 1 : N) {
    a[i, 1, 1] = x[i];
  }
}
