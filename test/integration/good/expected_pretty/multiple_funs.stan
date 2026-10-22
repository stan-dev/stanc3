/* $ stanc --auto-format ../multiple_funs.stan : [exit 0] */
functions {
  int foo(int a) {
    return a;
  }
  int bar(int a) {
    return a;
  }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}

