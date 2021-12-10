functions {
  real foo(int x){return 3;}
}
transformed data {
  int N = 5;
}
parameters {
  array[N] ordered[2] mu;
}
model {
  for (i in 1:4) {
    foo(3);
  }
}
