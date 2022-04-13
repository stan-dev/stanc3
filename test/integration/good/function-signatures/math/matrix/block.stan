data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
  complex_matrix[d_int,d_int] d_cmatrix;
}

transformed data {
  matrix[d_int,d_int] transformed_data_matrix;

  transformed_data_matrix = block(d_matrix, d_int, d_int, d_int, d_int);

  complex_matrix[d_int,d_int] transformed_data_cmatrix;

  transformed_data_cmatrix = block(d_cmatrix, d_int, d_int, d_int, d_int);

}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
  complex_matrix[d_int,d_int] p_cmatrix;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;

  transformed_param_matrix = block(d_matrix, d_int, d_int, d_int, d_int);
  transformed_param_matrix = block(p_matrix, d_int, d_int, d_int, d_int);

  complex_matrix[d_int,d_int] transformed_param_cmatrix;

  transformed_param_cmatrix = block(d_cmatrix, d_int, d_int, d_int, d_int);
  transformed_param_cmatrix = block(p_cmatrix, d_int, d_int, d_int, d_int);

}
model {
  y_p ~ normal(0,1);
}
