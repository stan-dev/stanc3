data {
  int d_int;
  real d_real;
  matrix[d_int,d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;

  complex d_complex;
  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;
  complex_matrix[d_int, d_int] d_cmatrix;
}

transformed data {
  matrix[d_int,d_int] transformed_data_matrix;
  row_vector[d_int] transformed_data_row_vector;

  transformed_data_matrix = append_col(d_matrix, d_matrix);
  transformed_data_matrix = append_col(d_vector, d_matrix);
  transformed_data_matrix = append_col(d_matrix, d_vector);
  transformed_data_matrix = append_col(d_vector, d_vector);
  transformed_data_row_vector = append_col(d_row_vector, d_row_vector);
  transformed_data_row_vector = append_col(d_real, d_row_vector);
  transformed_data_row_vector = append_col(d_row_vector, d_real);

  complex_matrix[d_int,d_int] transformed_data_cmatrix;
  complex_row_vector[d_int] transformed_data_crow_vector;

  transformed_data_cmatrix = append_col(d_cmatrix, d_cmatrix);
  transformed_data_cmatrix = append_col(d_cvector, d_cmatrix);
  transformed_data_cmatrix = append_col(d_cmatrix, d_cvector);
  transformed_data_cmatrix = append_col(d_cvector, d_cvector);
  transformed_data_crow_vector = append_col(d_crow_vector, d_crow_vector);
  transformed_data_crow_vector = append_col(d_complex, d_crow_vector);
  transformed_data_crow_vector = append_col(d_crow_vector, d_complex);
}
parameters {
  real p_real;
  matrix[d_int,d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;

  complex p_complex;
  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;
  complex_matrix[d_int, d_int] p_cmatrix;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;
  row_vector[d_int] transformed_param_row_vector;

  transformed_param_matrix = append_col(p_matrix, d_matrix);
  transformed_param_matrix = append_col(d_matrix, p_matrix);
  transformed_param_matrix = append_col(p_matrix, p_matrix);
  transformed_param_matrix = append_col(d_matrix, d_matrix);

  transformed_param_matrix = append_col(p_vector, d_matrix);
  transformed_param_matrix = append_col(d_vector, p_matrix);
  transformed_param_matrix = append_col(p_vector, p_matrix);
  transformed_param_matrix = append_col(d_vector, d_matrix);

  transformed_param_matrix = append_col(p_matrix, d_vector);
  transformed_param_matrix = append_col(d_matrix, p_vector);
  transformed_param_matrix = append_col(p_matrix, p_vector);
  transformed_param_matrix = append_col(d_matrix, d_vector);

  transformed_param_matrix = append_col(p_vector, d_vector);
  transformed_param_matrix = append_col(d_vector, p_vector);
  transformed_param_matrix = append_col(p_vector, p_vector);
  transformed_param_matrix = append_col(d_vector, d_vector);

  transformed_param_row_vector = append_col(p_row_vector, d_row_vector);
  transformed_param_row_vector = append_col(d_row_vector, p_row_vector);
  transformed_param_row_vector = append_col(p_row_vector, p_row_vector);
  transformed_param_row_vector = append_col(d_row_vector, d_row_vector);

  transformed_param_row_vector = append_col(p_real, d_row_vector);
  transformed_param_row_vector = append_col(d_real, p_row_vector);
  transformed_param_row_vector = append_col(p_real, p_row_vector);
  transformed_param_row_vector = append_col(d_real, d_row_vector);

  transformed_param_row_vector = append_col(p_row_vector, d_real);
  transformed_param_row_vector = append_col(d_row_vector, p_real);
  transformed_param_row_vector = append_col(p_row_vector, p_real);
  transformed_param_row_vector = append_col(d_row_vector, d_real);

  complex_matrix[d_int,d_int] transformed_param_cmatrix;
  complex_row_vector[d_int] transformed_param_crow_vector;

  transformed_param_cmatrix = append_col(p_cmatrix, d_cmatrix);
  transformed_param_cmatrix = append_col(d_cmatrix, p_cmatrix);
  transformed_param_cmatrix = append_col(p_cmatrix, p_cmatrix);
  transformed_param_cmatrix = append_col(d_cmatrix, d_cmatrix);

  transformed_param_cmatrix = append_col(p_cvector, d_cmatrix);
  transformed_param_cmatrix = append_col(d_cvector, p_cmatrix);
  transformed_param_cmatrix = append_col(p_cvector, p_cmatrix);
  transformed_param_cmatrix = append_col(d_cvector, d_cmatrix);

  transformed_param_cmatrix = append_col(p_cmatrix, d_cvector);
  transformed_param_cmatrix = append_col(d_cmatrix, p_cvector);
  transformed_param_cmatrix = append_col(p_cmatrix, p_cvector);
  transformed_param_cmatrix = append_col(d_cmatrix, d_cvector);

  transformed_param_cmatrix = append_col(p_cvector, d_cvector);
  transformed_param_cmatrix = append_col(d_cvector, p_cvector);
  transformed_param_cmatrix = append_col(p_cvector, p_cvector);
  transformed_param_cmatrix = append_col(d_cvector, d_cvector);

  transformed_param_crow_vector = append_col(p_crow_vector, d_crow_vector);
  transformed_param_crow_vector = append_col(d_crow_vector, p_crow_vector);
  transformed_param_crow_vector = append_col(p_crow_vector, p_crow_vector);
  transformed_param_crow_vector = append_col(d_crow_vector, d_crow_vector);

  transformed_param_crow_vector = append_col(p_complex, d_crow_vector);
  transformed_param_crow_vector = append_col(d_complex, p_crow_vector);
  transformed_param_crow_vector = append_col(p_complex, p_crow_vector);
  transformed_param_crow_vector = append_col(d_complex, d_crow_vector);

  transformed_param_crow_vector = append_col(p_crow_vector, d_complex);
  transformed_param_crow_vector = append_col(d_crow_vector, p_complex);
  transformed_param_crow_vector = append_col(p_crow_vector, p_complex);
  transformed_param_crow_vector = append_col(d_crow_vector, d_complex);


}
model {
  p_real ~ normal(0,1);
}
