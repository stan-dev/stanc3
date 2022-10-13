
functions {
  real foo(data tuple(real, real) x){
    return x.1;
  }
}

parameters {
  tuple(real, real) ps;
}

model {
  target += foo(ps);
}
