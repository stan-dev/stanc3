functions {
  complex_vector czv(complex_vector z) {
    return z * 2;
  }
}
parameters {
  vector[3] beta;
}
model {
  target += get_real(sum(czv(beta)));
}
