/* $ stanc --auto-format ../to_matrix_exp.stan : [exit 0] */
functions {
  matrix foo(matrix t) {
    return t;
  }
}
data {
  int N;
  row_vector[N] b;
}
transformed parameters {
  matrix[N, N] log_lik = foo(to_matrix(b));
}

