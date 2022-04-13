data {
  int d_int;
  array[d_int] int d_int_array;
  array[d_int] real d_real_array;
  matrix[d_int, d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;

  complex_matrix[d_int, d_int] d_cmatrix;
  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;
  array[d_int] complex d_carray;
}
transformed data {
  row_vector[d_int] transformed_data_row_vector;

  transformed_data_row_vector = to_row_vector(d_matrix);
  transformed_data_row_vector = to_row_vector(d_vector);
  transformed_data_row_vector = to_row_vector(d_row_vector);
  transformed_data_row_vector = to_row_vector(d_int_array);
  transformed_data_row_vector = to_row_vector(d_real_array);

  complex_row_vector[d_int] transformed_data_crow_vector;

  transformed_data_crow_vector = to_row_vector(d_cmatrix);
  transformed_data_crow_vector = to_row_vector(d_cvector);
  transformed_data_crow_vector = to_row_vector(d_crow_vector);
  transformed_data_crow_vector = to_row_vector(d_carray);
}
parameters {
  real p_real;
  real y_p;
  array[d_int] real p_real_array;
  matrix[d_int, d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;

  complex_matrix[d_int, d_int] p_cmatrix;
  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;
  array[d_int] complex p_carray;
}
transformed parameters {
  row_vector[d_int] transformed_param_row_vector;

  transformed_param_row_vector = to_row_vector(d_matrix);
  transformed_param_row_vector = to_row_vector(d_vector);
  transformed_param_row_vector = to_row_vector(d_row_vector);
  transformed_param_row_vector = to_row_vector(d_int_array);
  transformed_param_row_vector = to_row_vector(d_real_array);
  transformed_param_row_vector = to_row_vector(p_matrix);
  transformed_param_row_vector = to_row_vector(p_vector);
  transformed_param_row_vector = to_row_vector(p_row_vector);
  transformed_param_row_vector = to_row_vector(p_real_array);

  complex_row_vector[d_int] transformed_param_crow_vector;

  transformed_param_crow_vector = to_row_vector(d_cmatrix);
  transformed_param_crow_vector = to_row_vector(d_cvector);
  transformed_param_crow_vector = to_row_vector(d_crow_vector);
  transformed_param_crow_vector = to_row_vector(d_carray);
  transformed_param_crow_vector = to_row_vector(p_cmatrix);
  transformed_param_crow_vector = to_row_vector(p_cvector);
  transformed_param_crow_vector = to_row_vector(p_crow_vector);
  transformed_param_crow_vector = to_row_vector(p_carray);

}
model {
  y_p ~ normal(0, 1);
}

