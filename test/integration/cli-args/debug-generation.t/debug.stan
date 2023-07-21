data {
  int<lower=0> N;
  int<lower=0> K;
  array[K + N] real y;
  tuple(int, array[N] real) x;
}
parameters {
  simplex[K + N + 1 + x.1] theta;
}
