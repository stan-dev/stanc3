data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  complex_matrix[d_int, d_int] d_complex_matrix;


}

transformed data {
  int td_int;
  matrix[d_int, d_int] td_matrix;
  complex_matrix[d_int, d_int] td_complex_matrix;
  array[d_int] row_vector[d_int] td_row_vector_array_1d;
  array[d_int] complex_row_vector[d_int] td_complex_row_vector_array_1d;

  td_complex_row_vector_array_1d = to_row_vector_array(d_complex_matrix);
  td_row_vector_array_1d = to_row_vector_array(d_matrix);
}

parameters {
  matrix[d_int, d_int] p_matrix;
  complex_matrix[d_int, d_int] p_complex_matrix;
  array[d_int] row_vector[d_int] p_row_vector_array_1d;
  array[d_int] complex_row_vector[d_int] p_complex_row_vector_array_1d;


}

transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;
  complex_matrix[d_int, d_int] transformed_param_complex_matrix;
  array[d_int] row_vector[d_int] transformed_param_row_vector_array_1d;
  array[d_int] complex_row_vector[d_int] transformed_param_complex_row_vector_array_1d;

  transformed_param_complex_row_vector_array_1d = to_row_vector_array(d_complex_matrix);
  transformed_param_complex_row_vector_array_1d = to_row_vector_array(p_complex_matrix);
  transformed_param_row_vector_array_1d = to_row_vector_array(d_matrix);
  transformed_param_row_vector_array_1d = to_row_vector_array(p_matrix);
}

