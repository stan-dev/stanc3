
functions {
  real foo(data (real, real) x){
    return x.1;
  }
}

data {
  (real, real) d;
}

model {
  target += foo(d);
}
