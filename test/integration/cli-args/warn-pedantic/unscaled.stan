functions {
  real f() {
    return 1000;
  }
}
parameters {
  real x;
}
model {
  x ~ normal(0.001, 10000);
  real z;
  z = -1000 + 0.00001;
}
