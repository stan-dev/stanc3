data {
  int d_int;
  real d_real;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;

  complex d_complex;
  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;
}

transformed data {
  matrix[d_int,d_int] transformed_data_matrix;

  transformed_data_matrix = rep_matrix(d_real, d_int, d_int);
  transformed_data_matrix = rep_matrix(d_vector, d_int);
  transformed_data_matrix = rep_matrix(d_row_vector, d_int);

  complex_matrix[d_int,d_int] transformed_data_cmatrix;
  transformed_data_cmatrix = rep_matrix(d_complex, d_int, d_int);
  transformed_data_cmatrix = rep_matrix(d_cvector, d_int);
  transformed_data_cmatrix = rep_matrix(d_crow_vector, d_int);
}
parameters {
  real p_real;
  real y_p;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;

  complex p_complex;
  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;

  transformed_param_matrix = rep_matrix(d_real, d_int, d_int);
  transformed_param_matrix = rep_matrix(d_vector, d_int);
  transformed_param_matrix = rep_matrix(d_row_vector, d_int);
  transformed_param_matrix = rep_matrix(p_real, d_int, d_int);
  transformed_param_matrix = rep_matrix(p_vector, d_int);
  transformed_param_matrix = rep_matrix(p_row_vector, d_int);

  complex_matrix[d_int,d_int] transformed_param_cmatrix;

  transformed_param_cmatrix = rep_matrix(d_complex, d_int, d_int);
  transformed_param_cmatrix = rep_matrix(d_cvector, d_int);
  transformed_param_cmatrix = rep_matrix(d_crow_vector, d_int);
  transformed_param_cmatrix = rep_matrix(p_complex, d_int, d_int);
  transformed_param_cmatrix = rep_matrix(p_cvector, d_int);
  transformed_param_cmatrix = rep_matrix(p_crow_vector, d_int);

}
model {
  y_p ~ normal(0,1);
}
