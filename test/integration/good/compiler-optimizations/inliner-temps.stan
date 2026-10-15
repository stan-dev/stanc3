functions {
  matrix sign_fix(matrix a) {
    int d = rows(a);
    matrix[d, d] out = a;
    for (j in 1 : d) {
      if (a[1, j] < 0) {
        out[ : , j] = -a[ : , j];
      }
    }
    return out;
  }
  real self_diff(real x) {
    return x - x;
  }
}
data {
  int<lower=2> d;
  matrix[d, d] L;
  matrix[d, d] U;
}
generated quantities {
  real z = self_diff(normal_rng(0, 1));
  matrix[d, d] a = sign_fix(L * U);
}
