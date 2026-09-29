functions {
  vector elementwise_exp(vector x) {
    int N = rows(x);
    vector[N] result;
    for (i in 1:N)
      result[i] = exp(x[i]);
    return result;
  }
  real twice(real x) {
    return x + x;
  }
}
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
}
parameters {
  vector[K] beta;
}
model {
  vector[N] mu = elementwise_exp(X * beta);
  beta ~ std_normal();
  target += normal_lpdf(mu | 0, 1);
}
generated quantities {
  real y = twice(normal_rng(0, 1));
}
