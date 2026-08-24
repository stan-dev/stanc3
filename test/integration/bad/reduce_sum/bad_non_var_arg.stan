
transformed data {
  int N = 100;
}
parameters {
  array[N] real y1;
  array[N] real y2;
  array[N] real y3;
}
model {
  target += reduce_sum(0.0, y1, 1);
}

