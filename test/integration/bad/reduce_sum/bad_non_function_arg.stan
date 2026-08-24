
transformed data {
  int N = 100;
  real g;
}
parameters {
  array[N] real y1;
  array[N] real y2;
  array[N] real y3;
}
model {
  target += reduce_sum(g, y1, 1);
}

