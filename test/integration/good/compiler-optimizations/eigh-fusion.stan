data {
  int<lower=1> N;
  matrix[N, N] A;
}
parameters {
  real x;
}
transformed parameters {
  // fused: adjacent pair, same argument
  matrix[N, N] Q;
  vector[N] R;
  Q = eigenvectors_sym(A * x);
  R = eigenvalues_sym(A * x);
  // fused: adjacent pair in the reverse order
  matrix[N, N] Q2;
  vector[N] R2;
  R2 = eigenvalues_sym(A + x);
  Q2 = eigenvectors_sym(A + x);
  // not fused: different arguments
  matrix[N, N] Q3;
  vector[N] R3;
  Q3 = eigenvectors_sym(A * x);
  R3 = eigenvalues_sym(A - x);
  // not fused: not adjacent
  matrix[N, N] Q4;
  vector[N] R4;
  real z;
  Q4 = eigenvectors_sym(A / x);
  z = x * 2;
  R4 = eigenvalues_sym(A / x);
}
model {
  x ~ normal(0, 1);
  target += sum(Q * R) + sum(Q2 * R2) + sum(Q3 * R3) + sum(Q4 * R4) + z;
}
generated quantities {
  // fused: pair inside a nested block
  matrix[N, N] gq_Q;
  vector[N] gq_R;
  {
    gq_Q = eigenvectors_sym(A);
    gq_R = eigenvalues_sym(A);
  }
  real gq = sum(gq_Q * gq_R);
}
