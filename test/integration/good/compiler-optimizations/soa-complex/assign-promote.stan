parameters {
  vector[3] beta;
}
model {
  complex_vector[3] z;
  z = beta;
  target += get_real(sum(z * 2));
}
