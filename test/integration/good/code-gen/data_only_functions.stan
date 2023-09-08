functions {
  real baz(data matrix y) {
    return y[1, 1];
  }

  real bar(data array[] matrix z) {
    return z[1][1, 1];
  }

  real foo(data array[,,] real x, data array[,] int y) {
    return x[1, 1, 1];
  }

  real baz_param(matrix y) {
    return y[1, 1];
  }

  real bar_param(array[] matrix z) {
    return z[1][1, 1];
  }

  real foo_param(array[,,] real x, array[,] int y) {
    return x[1, 1, 1];
  }
}
data {
  int N;
  matrix[N, N] d;
}
parameters {
  matrix[N, N] p;
}
model {
  target += baz(d);
  target += bar({d, d});
  target += foo({{{1.5}}}, {{1, 2}});

  target += baz_param(d);
  target += bar_param({d, d});
  target += foo_param({{{1.5}}}, {{1, 2}});
}
