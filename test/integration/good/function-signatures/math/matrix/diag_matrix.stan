data {
  int d_int;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;

}

transformed data {
  matrix[d_int,d_int] transformed_data_matrix;

  transformed_data_matrix = diag_matrix(d_vector);
  transformed_data_matrix = diag_matrix(d_row_vector);

  complex_matrix[d_int,d_int] transformed_data_cmatrix;
  transformed_data_cmatrix = diag_matrix(d_cvector);
  transformed_data_cmatrix = diag_matrix(d_crow_vector);
}
parameters {
  real y_p;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;

  transformed_param_matrix = diag_matrix(d_vector);
  transformed_param_matrix = diag_matrix(d_row_vector);
  transformed_param_matrix = diag_matrix(p_vector);
  transformed_param_matrix = diag_matrix(p_row_vector);

  complex_matrix[d_int,d_int] transformed_param_cmatrix;
  transformed_param_cmatrix = diag_matrix(d_cvector);
  transformed_param_cmatrix = diag_matrix(d_crow_vector);
  transformed_param_cmatrix = diag_matrix(p_cvector);
  transformed_param_cmatrix = diag_matrix(p_crow_vector);

}
model {
  y_p ~ normal(0,1);
}
