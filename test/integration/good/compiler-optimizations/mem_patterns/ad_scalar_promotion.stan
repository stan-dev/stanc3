data {
  int N;
  vector[N] x;
  row_vector[N] xr;
  matrix[N, N] X;
}
parameters {
  real test1;
  real test2;
}
model {
  vector[N] test;
  test1 ~ std_normal();
  test2 ~ std_normal();
  // fma(ad scalar, data vector, ad scalar): promoted to
  // fma(rep_vector(test1, rows(x)), x, test2) so the result is SoA
  test = fma(test1, x, test2);
  target += sum(test);
  target += fma(test1, x, test2);
  // row vector and matrix data promote through rep_row_vector / rep_matrix
  row_vector[N] test_r = fma(test1, xr, test2);
  target += sum(test_r);
  matrix[N, N] test_m = fma(test1, X, test2);
  target += sum(test_m);
  // elementwise operators are promoted too
  vector[N] test_add = test1 + x;
  target += sum(test_add);
  // multiply by an AD scalar is rewritten to elt_multiply on a rep_* matrix
  vector[N] test_mul = test1 * x;
  target += sum(test_mul);
  matrix[N, N] test_mul_m = X * test2;
  target += sum(test_mul_m);
  // ternary branches are always AoS, so no promotion happens here
  vector[N] tern = N > 1 ? fma(test1, x, test2) : fma(test2, x, test1);
  target += sum(tern);
}
