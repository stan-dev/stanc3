data {
  int N;
}
parameters {
  real a;
  real b;
  vector[N] y;
  vector[N] y_soa;
}
model {
  // y is indexed inside a loop, so it is AoS
  for (i in 1:N) {
    target += normal_lpdf(y[i] | 0, 1);
  }
  y_soa ~ std_normal();
  // fma(real, Matrix<var>, real) returns Matrix<var>, but promoting `a` to a
  // var<Matrix> makes Stan Math return a var<Matrix>, so z can be SoA even
  // though y is AoS
  vector[N] z = fma(a, y, b);
  target += sum(z);
  // y_soa is already SoA, so there is nothing to promote here
  vector[N] z_soa = fma(a, y_soa, b);
  target += sum(z_soa);
}
