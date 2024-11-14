data {
  int N;
  real r;
}
parameters {
  vector[N] v;
  row_vector[N] rv;
  matrix[N, N] A;
}
generated quantities {
  int i = 3 % N;
  real x = +r;
  int b = r <= N;
  if (r != 0.0) {
    b = 0;
  }
  row_vector[N] drv = rv / A;
  vector[N] dv = A \ v;
  dv = +dv;
  dv = -dv;
}
