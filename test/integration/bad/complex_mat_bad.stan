data {
   int N;
   complex_matrix[N,N] Z;
}

model {
  real x = Z[1,1];
  target += x;
}
