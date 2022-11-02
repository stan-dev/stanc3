functions {
  real foo_lpdf(vector x, vector y, vector z) {
    return 0.1;
  }

  real foo_lpdf(vector x, real y, real z) {
    return 0.2;
  }

  real foo_lcdf(real interval, real y, real z) {
    return 0.3;
  }

  real foo_lccdf(real interval, real y, real z) {
    return 0.4;
  }
}
data {
  int N;
  vector[N] X;
}
parameters {
  real alpha, beta;
  vector[N] mu, sigma;
}
model {
  X ~ foo(alpha, beta) T[0, ]; // does not fail
  X ~ foo(mu, sigma) T[0, ]; // this does
}
