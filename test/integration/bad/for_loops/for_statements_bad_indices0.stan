functions {
  int foo(int a) {
    while (1) {
      array[2] int vs;
      for (v in vs) v = 3.2;
    }
    return 0;
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
