data {
  int N;
}

transformed parameters {
  array[N] complex_matrix[N,N] Z1;

  array[N] matrix[N,N] R;

  Z1 = R; // fine
  R = Z1; // error
}
