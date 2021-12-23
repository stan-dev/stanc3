data {
  int d_int;
  array[d_int] int d_int_array;
  array[d_int] real d_real_array;
  matrix[d_int, d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
}
transformed data {
  vector[d_int] transformed_data_vector;
  
  transformed_data_vector = to_vector(d_matrix);
  transformed_data_vector = to_vector(d_vector);
  transformed_data_vector = to_vector(d_row_vector);
  transformed_data_vector = to_vector(d_int_array);
  transformed_data_vector = to_vector(d_real_array);
}
parameters {
  real p_real;
  real y_p;
  array[d_int] real p_real_array;
  matrix[d_int, d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
}
transformed parameters {
  vector[d_int] transformed_param_vector;
  
  transformed_param_vector = to_vector(d_matrix);
  transformed_param_vector = to_vector(d_vector);
  transformed_param_vector = to_vector(d_row_vector);
  transformed_param_vector = to_vector(d_int_array);
  transformed_param_vector = to_vector(d_real_array);
  transformed_param_vector = to_vector(p_matrix);
  transformed_param_vector = to_vector(p_vector);
  transformed_param_vector = to_vector(p_row_vector);
  transformed_param_vector = to_vector(p_real_array);
}
model {
  y_p ~ normal(0, 1);
}

