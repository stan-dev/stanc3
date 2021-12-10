functions {
  real foo(real x) {
    return x + target();
  }
}
parameters {
  real z;
}
model {
  z ~ normal(0,1);
  print(foo(z));
}
