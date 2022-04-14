data {
  int d_int;
  vector[d_int] d_vector;
  complex_vector[d_int] d_cvector;

}

transformed data {
  matrix[d_int,d_int] transformed_data_matrix;

  transformed_data_matrix = diag_matrix(d_vector);

  complex_matrix[d_int,d_int] transformed_data_cmatrix;
  transformed_data_cmatrix = diag_matrix(d_cvector);
}
parameters {
  real y_p;
  vector[d_int] p_vector;
  complex_vector[d_int] p_cvector;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;

  transformed_param_matrix = diag_matrix(d_vector);
  transformed_param_matrix = diag_matrix(p_vector);

  complex_matrix[d_int,d_int] transformed_param_cmatrix;
  transformed_param_cmatrix = diag_matrix(d_cvector);
  transformed_param_cmatrix = diag_matrix(p_cvector);

}
model {
  y_p ~ normal(0,1);
}
