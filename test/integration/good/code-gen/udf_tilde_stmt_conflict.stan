functions {
  real normal(real a) {
    return square(a);
  }
}
parameters {
    real x;
}
model {
    x ~ normal(0,1);
}