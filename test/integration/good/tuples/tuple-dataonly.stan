functions {
  real foo(data tuple(real, real) x) {
    return x.1;
  }
}
data {
  tuple(real, real) d;
}
model {
  target += foo(d);
}
