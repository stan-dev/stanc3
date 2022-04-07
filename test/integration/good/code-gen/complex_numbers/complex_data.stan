data {
  int N;
  complex z;
  complex_vector[N] z1;
  complex_row_vector[N] z2;
  complex_matrix[N,N] z3;

  int M;
  array[M] complex z4;
  array[M] complex_vector[N] z5;
  array[M] complex_row_vector[N] z6;
  array[M] complex_matrix[N,N] z7;

  // not the same order, important:
  complex_matrix[N,M] z8;

}
