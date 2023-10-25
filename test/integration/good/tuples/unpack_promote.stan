data {
  int<lower=0> N;
  vector[N] x;
  array[N] int y;
}
transformed data {
  tuple(array[N] int, vector[N]) td = (y, x);
}
parameters {
  array[N] real alpha;
  vector[N] theta;
}
model {
  array[N] real r;
  array[N] complex z;
  complex_vector[N] v;
  
  (r, v) = td;
  print(r, v);
  (r, z[ : ], v) = (y, y, x);
  print(r, z, v);
  (r, z, v) = (alpha, alpha, theta);
  print(r, z, v);
  
  tuple(complex_vector[N], int) tup;
  
  (tup.1, tup.2) = (theta, 1);
  print(tup);
}
