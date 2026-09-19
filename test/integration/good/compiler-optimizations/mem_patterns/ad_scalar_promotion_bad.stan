data {
  int N;
  vector[N] x;
}
parameters {
  real test1;
  real test2;
}
model {
  vector[N] test = fma(test1, x, test2);
  for (i in 1:N) {
    target += test[i];
  }
}
