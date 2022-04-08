data {
  int d_int;
  real d_real;
  matrix[d_int,d_int] d_matrix;
  row_vector[d_int] d_row_vector;
  vector[d_int] d_vector;

  complex d_complex;
  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;
  complex_matrix[d_int, d_int] d_cmatrix;
}

transformed data {
  matrix[d_int,d_int] transformed_data_matrix;
  vector[d_int] transformed_data_vector;

  transformed_data_matrix = append_row(d_matrix, d_matrix);
  transformed_data_matrix = append_row(d_row_vector, d_matrix);
  transformed_data_matrix = append_row(d_matrix, d_row_vector);
  transformed_data_matrix = append_row(d_row_vector, d_row_vector);
  transformed_data_vector = append_row(d_vector, d_vector);
  transformed_data_vector = append_row(d_real, d_vector);
  transformed_data_vector = append_row(d_vector, d_real);

  complex_matrix[d_int,d_int] transformed_data_cmatrix;
  complex_vector[d_int] transformed_data_cvector;
  
  transformed_data_cmatrix = append_row(d_cmatrix, d_cmatrix);
  transformed_data_cmatrix = append_row(d_crow_vector, d_cmatrix);
  transformed_data_cmatrix = append_row(d_cmatrix, d_crow_vector);
  transformed_data_cmatrix = append_row(d_crow_vector, d_crow_vector);
  transformed_data_cvector = append_row(d_cvector, d_cvector);
  transformed_data_cvector = append_row(d_complex, d_cvector);
  transformed_data_cvector = append_row(d_cvector, d_complex);
}
parameters {
  real p_real;
  matrix[d_int,d_int] p_matrix;
  row_vector[d_int] p_row_vector;
  vector[d_int] p_vector;

  complex p_complex;
  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;
  complex_matrix[d_int, d_int] p_cmatrix;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;
  vector[d_int] transformed_param_vector;

  transformed_param_matrix = append_row(p_matrix, d_matrix);
  transformed_param_matrix = append_row(d_matrix, p_matrix);
  transformed_param_matrix = append_row(p_matrix, p_matrix);
  transformed_param_matrix = append_row(d_matrix, d_matrix);

  transformed_param_matrix = append_row(p_row_vector, d_matrix);
  transformed_param_matrix = append_row(d_row_vector, p_matrix);
  transformed_param_matrix = append_row(p_row_vector, p_matrix);
  transformed_param_matrix = append_row(d_row_vector, d_matrix);

  transformed_param_matrix = append_row(p_matrix, d_row_vector);
  transformed_param_matrix = append_row(d_matrix, p_row_vector);
  transformed_param_matrix = append_row(p_matrix, p_row_vector);
  transformed_param_matrix = append_row(d_matrix, d_row_vector);

  transformed_param_matrix = append_row(p_row_vector, d_row_vector);
  transformed_param_matrix = append_row(d_row_vector, p_row_vector);
  transformed_param_matrix = append_row(p_row_vector, p_row_vector);
  transformed_param_matrix = append_row(d_row_vector, d_row_vector);

  transformed_param_vector = append_row(p_vector, d_vector);
  transformed_param_vector = append_row(d_vector, p_vector);
  transformed_param_vector = append_row(p_vector, p_vector);
  transformed_param_vector = append_row(d_vector, d_vector);

  transformed_param_vector = append_row(p_real, d_vector);
  transformed_param_vector = append_row(d_real, p_vector);
  transformed_param_vector = append_row(p_real, p_vector);
  transformed_param_vector = append_row(d_real, d_vector);

  transformed_param_vector = append_row(p_vector, p_real);
  transformed_param_vector = append_row(d_vector, p_real);
  transformed_param_vector = append_row(p_vector, d_real);
  transformed_param_vector = append_row(d_vector, d_real);


  complex_matrix[d_int,d_int] transformed_param_cmatrix;
  complex_vector[d_int] transformed_param_cvector;
  transformed_param_cmatrix = append_row(p_cmatrix, d_cmatrix);
  transformed_param_cmatrix = append_row(d_cmatrix, p_cmatrix);
  transformed_param_cmatrix = append_row(p_cmatrix, p_cmatrix);
  transformed_param_cmatrix = append_row(d_cmatrix, d_cmatrix);

  transformed_param_cmatrix = append_row(p_crow_vector, d_cmatrix);
  transformed_param_cmatrix = append_row(d_crow_vector, p_cmatrix);
  transformed_param_cmatrix = append_row(p_crow_vector, p_cmatrix);
  transformed_param_cmatrix = append_row(d_crow_vector, d_cmatrix);

  transformed_param_cmatrix = append_row(p_cmatrix, d_crow_vector);
  transformed_param_cmatrix = append_row(d_cmatrix, p_crow_vector);
  transformed_param_cmatrix = append_row(p_cmatrix, p_crow_vector);
  transformed_param_cmatrix = append_row(d_cmatrix, d_crow_vector);

  transformed_param_cmatrix = append_row(p_crow_vector, d_crow_vector);
  transformed_param_cmatrix = append_row(d_crow_vector, p_crow_vector);
  transformed_param_cmatrix = append_row(p_crow_vector, p_crow_vector);
  transformed_param_cmatrix = append_row(d_crow_vector, d_crow_vector);

  transformed_param_cvector = append_row(p_cvector, d_cvector);
  transformed_param_cvector = append_row(d_cvector, p_cvector);
  transformed_param_cvector = append_row(p_cvector, p_cvector);
  transformed_param_cvector = append_row(d_cvector, d_cvector);

  transformed_param_cvector = append_row(p_complex, d_cvector);
  transformed_param_cvector = append_row(d_complex, p_cvector);
  transformed_param_cvector = append_row(p_complex, p_cvector);
  transformed_param_cvector = append_row(d_complex, d_cvector);

  transformed_param_cvector = append_row(p_cvector, p_complex);
  transformed_param_cvector = append_row(d_cvector, p_complex);
  transformed_param_cvector = append_row(p_cvector, d_complex);
  transformed_param_cvector = append_row(d_cvector, d_complex);

}
model {
  p_real ~ normal(0,1);
}
