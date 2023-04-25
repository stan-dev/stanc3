data {
  int<lower=0> N;
  int<lower=0> K;
  array[K+N] real y;
}

parameters {
  simplex[K+N] theta; // mixture proportions
}
