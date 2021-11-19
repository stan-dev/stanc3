functions {
  real bar_baz_lpdf(real a, real b) {
    return a / b;
  }
  real foo_bar_lpdf(array[,,,] real x){
    return 1.0;
  }
  real baz_foo_lpdf(complex z, real a){
    return get_imag(z) * a;
  }
}
parameters {
  real y;
  complex z;
  array[1,1,1,1] real arr;
}
model {
  y ~ bar_baz(3.2);
  z ~ baz_foo(1.5);
  arr ~ foo_bar();
}
