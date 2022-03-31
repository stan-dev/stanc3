
functions {
  real foo(data (real, real) x){
    return x.1;
  }
}

parameters {
  (real, real) ps;
}

model {
  target += foo(ps);
}
