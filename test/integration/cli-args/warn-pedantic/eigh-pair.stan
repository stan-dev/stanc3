data {
  int<lower=1> N;
  matrix[N, N] A;
  matrix[N, N] B;
}
transformed parameters {
  matrix[N, N] Q;
  vector[N] R;
  // warned: both primitives on the same argument
  Q = eigenvectors_sym(A);
  R = eigenvalues_sym(A);
  // not warned: different arguments
  matrix[N, N] Q2 = eigenvectors_sym(B);
  vector[N] R2 = eigenvalues_sym(A');
}
model {
  target += sum(Q * R) + sum(Q2 * R2);
}
