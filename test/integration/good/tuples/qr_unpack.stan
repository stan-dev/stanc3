data {
  int<lower=1> N;
  int<lower=1> M;
}
parameters {
  matrix[N, M] A;
}
model {
  matrix[N, M] Q;
  matrix[M, M] R;

  (Q, R) = qr_thin(A);

  // ... do something with Q and R ...
  target += sum(Q) + dot_self(to_vector(R));
}
