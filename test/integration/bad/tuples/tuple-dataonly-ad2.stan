
functions {
  real foo(data tuple(real, real) x){
    return x.1;
  }
}

data {
  real x1;
}

parameters {
  real x2;
}

model {
  target += foo((x1,x2));
}
