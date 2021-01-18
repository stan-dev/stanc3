data {
  int d_int;
  int d_int_array[d_int];
  real d_real;
  real d_real_array[d_int];
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  matrix[d_int, d_int] d_matrix;
}
parameters {
  real p_real;
  real p_real_array[d_int];
  matrix[d_int, d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  real y_p;
}
transformed parameters {
  matrix[d_int, d_int] tp_matrix;
  vector[d_int] tp_vector;
  row_vector[d_int] tp_row_vector;
  real tp_real;
  tp_matrix = cholesky_decompose(d_matrix);
  tp_matrix = cholesky_decompose(p_matrix);
  tp_matrix = cholesky_decompose(p_matrix);
  tp_real = beta(p_real, p_real);
  tp_matrix = beta(p_matrix, p_matrix);
  tp_vector = beta(p_vector, p_vector);
  tp_vector = beta(2 * p_vector, 3 * p_vector);
  tp_row_vector = beta(p_row_vector, p_row_vector);

}
model {
  y_p ~ normal(0, 1);
}

