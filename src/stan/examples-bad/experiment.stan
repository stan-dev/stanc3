functions {
  void foo(matrix x, real[] y) return;
}

transformed data {
  real x [2];
  real<location=x> y [2]= {1,2};
  print(y[x]);
}