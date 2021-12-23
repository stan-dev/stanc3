data {
  int d_int;
  array[d_int, d_int] int d_int_array_2;
  array[d_int, d_int, d_int] int d_int_array_3;
  array[d_int, d_int] real d_real_array_2;
  array[d_int, d_int, d_int] real d_real_array_3;
  matrix[d_int, d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
}
transformed data {
  array[d_int] real transformed_data_real_array;
  array[d_int] int transformed_data_int_array;
  
  transformed_data_real_array = to_array_1d(d_matrix);
  transformed_data_real_array = to_array_1d(d_vector);
  transformed_data_real_array = to_array_1d(d_row_vector);
  transformed_data_real_array = to_array_1d(d_real_array_2);
  transformed_data_int_array = to_array_1d(d_int_array_2);
  transformed_data_real_array = to_array_1d(d_real_array_3);
  transformed_data_int_array = to_array_1d(d_int_array_3);
}
parameters {
  real y_p;
  array[d_int, d_int] real p_real_array_2;
  array[d_int, d_int, d_int] real p_real_array_3;
  matrix[d_int, d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
}
transformed parameters {
  array[d_int] real transformed_param_real_array;
  
  transformed_param_real_array = to_array_1d(d_matrix);
  transformed_param_real_array = to_array_1d(d_vector);
  transformed_param_real_array = to_array_1d(d_row_vector);
  transformed_param_real_array = to_array_1d(d_real_array_2);
  transformed_param_real_array = to_array_1d(d_int_array_2);
  transformed_param_real_array = to_array_1d(d_real_array_3);
  transformed_param_real_array = to_array_1d(d_int_array_3);
  
  transformed_param_real_array = to_array_1d(p_matrix);
  transformed_param_real_array = to_array_1d(p_vector);
  transformed_param_real_array = to_array_1d(p_row_vector);
  transformed_param_real_array = to_array_1d(p_real_array_2);
  transformed_param_real_array = to_array_1d(p_real_array_3);
}
model {
  y_p ~ normal(0, 1);
}

