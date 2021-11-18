functions {
  real bar_baz_lpdf(real a, real b) {
    return a / b;
  }
  real foo_bar_lpdf(array[,,,] real x){
    return 1.0;
  }
  real baz_foo_lpdf(complex z){
    return get_imag(z);
  }
}
parameters {
  real y;
}
model {
  y ~ bar_baz(3.2);
}
