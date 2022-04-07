data {
  int d_int;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  matrix[d_int,d_int] d_matrix;

  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;
  complex_matrix[d_int, d_int] d_cmatrix;
}

transformed data {
  matrix[d_int,d_int] transformed_data_matrix;

  transformed_data_matrix = diag_pre_multiply(d_vector, d_matrix);
  transformed_data_matrix = diag_pre_multiply(d_row_vector, d_matrix);

  complex_matrix[d_int,d_int] transformed_data_cmatrix;
  transformed_data_cmatrix = diag_pre_multiply(d_cvector, d_cmatrix);
  transformed_data_cmatrix = diag_pre_multiply(d_crow_vector, d_cmatrix);

}
parameters {
  real y_p;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  matrix[d_int,d_int] p_matrix;

  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;
  complex_matrix[d_int, d_int] p_cmatrix;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;

  transformed_param_matrix = diag_pre_multiply(d_vector, d_matrix);
  transformed_param_matrix = diag_pre_multiply(d_row_vector, d_matrix);

  transformed_param_matrix = diag_pre_multiply(p_vector, d_matrix);
  transformed_param_matrix = diag_pre_multiply(p_row_vector, d_matrix);

  transformed_param_matrix = diag_pre_multiply(d_vector, p_matrix);
  transformed_param_matrix = diag_pre_multiply(d_row_vector, p_matrix);

  transformed_param_matrix = diag_pre_multiply(p_vector, p_matrix);
  transformed_param_matrix = diag_pre_multiply(p_row_vector, p_matrix);

  complex_matrix[d_int,d_int] transformed_param_cmatrix;

  transformed_param_cmatrix = diag_pre_multiply(d_cvector, d_cmatrix);
  transformed_param_cmatrix = diag_pre_multiply(d_crow_vector, d_cmatrix);

  transformed_param_cmatrix = diag_pre_multiply(p_cvector, d_cmatrix);
  transformed_param_cmatrix = diag_pre_multiply(p_crow_vector, d_cmatrix);

  transformed_param_cmatrix = diag_pre_multiply(d_cvector, p_cmatrix);
  transformed_param_cmatrix = diag_pre_multiply(d_crow_vector, p_cmatrix);

  transformed_param_cmatrix = diag_pre_multiply(p_cvector, p_cmatrix);
  transformed_param_cmatrix = diag_pre_multiply(p_crow_vector, p_cmatrix);

}
model {
  y_p ~ normal(0,1);
}
