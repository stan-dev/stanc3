data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  array[d_int] int d_int_array_1d;


}

transformed data {
  int td_int;
  vector[d_int] td_vector;
  matrix[d_int, d_int] td_matrix;
  array[d_int] int td_int_array_1d;

  td_vector = zip_index(d_matrix, d_int_array_1d, d_int_array_1d);
}

parameters {
  vector[d_int] p_vector;
  matrix[d_int, d_int] p_matrix;


}

transformed parameters {
  vector[d_int] transformed_param_vector;
  matrix[d_int, d_int] transformed_param_matrix;

  transformed_param_vector = zip_index(d_matrix, d_int_array_1d, d_int_array_1d);
  transformed_param_vector = zip_index(p_matrix, d_int_array_1d, d_int_array_1d);
}

