data { 
  int d_int;
  int d_int_array[d_int];
  int d_int_array_2d[d_int,2];
  int d_int_array_3d[d_int,2,3];
  int d_int_array_4d[d_int,2,3,4];
  int d_int_array_5d[d_int,2,3,4,5];
  real d_real;
  real d_real_array[d_int];
  real d_real_array_2d[d_int,2];
  real d_real_array_3d[d_int,2,3];
  matrix[d_int,d_int] d_matrix;
  matrix[d_int,d_int] d_matrix_array[d_int];
  matrix[d_int,d_int] d_matrix_array_2d[d_int,2];
  matrix[d_int,d_int] d_matrix_array_3d[d_int,2,3];
  vector[d_int] d_vector;
  vector[d_int] d_vector_array[d_int];
  vector[d_int] d_vector_array_2d[d_int,2];
  vector[d_int] d_vector_array_3d[d_int,2,3];
  row_vector[d_int] d_row_vector;
  row_vector[d_int] d_row_vector_array[d_int];
  row_vector[d_int] d_row_vector_array_2d[d_int,2];
  row_vector[d_int] d_row_vector_array_3d[d_int,2,3];
}
transformed data {
  real transformed_data_real;
  real transformed_data_real_array[d_int];
  real transformed_data_real_array_2d[d_int,2];
  real transformed_data_real_array_3d[d_int,2,3];
  matrix[d_int,d_int] transformed_data_matrix;
  matrix[d_int,d_int] transformed_data_matrix_array[d_int];
  matrix[d_int,d_int] transformed_data_matrix_array_2d[d_int,2];
  matrix[d_int,d_int] transformed_data_matrix_array_3d[d_int,2,3];
  vector[d_int] transformed_data_vector;
  vector[d_int] transformed_data_vector_array[d_int];
  vector[d_int] transformed_data_vector_array_2d[d_int,2];
  vector[d_int] transformed_data_vector_array_3d[d_int,2,3];
  row_vector[d_int] transformed_data_row_vector;
  row_vector[d_int] transformed_data_row_vector_array[d_int];
  row_vector[d_int] transformed_data_row_vector_array_2d[d_int,2];
  row_vector[d_int] transformed_data_row_vector_array_3d[d_int,2,3];

  transformed_data_real_array = binary_log_loss(d_int_array, d_real);
  transformed_data_real_array = binary_log_loss(d_int_array, d_real_array);
  transformed_data_real_array_2d = binary_log_loss(d_int_array_2d, d_real);
  transformed_data_real_array_2d = binary_log_loss(d_int_array_2d, d_real_array_2d);
  transformed_data_real_array_3d = binary_log_loss(d_int_array_3d, d_real);
  transformed_data_real_array_3d = binary_log_loss(d_int_array_3d, d_real_array_3d);

  transformed_data_real_array = binary_log_loss(d_int, d_real_array);
  transformed_data_real_array_2d = binary_log_loss(d_int, d_real_array_2d);
  transformed_data_real_array_3d = binary_log_loss(d_int, d_real_array_3d);

  transformed_data_vector = binary_log_loss(d_int, d_vector);
  transformed_data_vector = binary_log_loss(d_int_array, d_vector);
  transformed_data_vector_array = binary_log_loss(d_int, d_vector_array);
  transformed_data_vector_array = binary_log_loss(d_int_array_2d, d_vector_array);
  transformed_data_vector_array_2d = binary_log_loss(d_int, d_vector_array_2d);
  transformed_data_vector_array_2d = binary_log_loss(d_int_array_3d, d_vector_array_2d);
  transformed_data_vector_array_3d = binary_log_loss(d_int, d_vector_array_3d);
  transformed_data_vector_array_3d = binary_log_loss(d_int_array_4d, d_vector_array_3d);

  transformed_data_row_vector = binary_log_loss(d_int, d_row_vector);
  transformed_data_row_vector = binary_log_loss(d_int_array, d_row_vector);
  transformed_data_row_vector_array = binary_log_loss(d_int, d_row_vector_array);
  transformed_data_row_vector_array = binary_log_loss(d_int_array_2d, d_row_vector_array);
  transformed_data_row_vector_array_2d = binary_log_loss(d_int, d_row_vector_array_2d);
  transformed_data_row_vector_array_2d = binary_log_loss(d_int_array_3d, d_row_vector_array_2d);
  transformed_data_row_vector_array_3d = binary_log_loss(d_int, d_row_vector_array_3d);
  transformed_data_row_vector_array_3d = binary_log_loss(d_int_array_4d, d_row_vector_array_3d);

  transformed_data_matrix = binary_log_loss(d_int, d_matrix);
  transformed_data_matrix = binary_log_loss(d_int_array_2d, d_matrix);
  transformed_data_matrix_array = binary_log_loss(d_int, d_matrix_array);
  transformed_data_matrix_array = binary_log_loss(d_int_array_3d, d_matrix_array);
  transformed_data_matrix_array_2d = binary_log_loss(d_int, d_matrix_array_2d);
  transformed_data_matrix_array_2d = binary_log_loss(d_int_array_4d, d_matrix_array_2d);
  transformed_data_matrix_array_3d = binary_log_loss(d_int, d_matrix_array_3d);
  transformed_data_matrix_array_3d = binary_log_loss(d_int_array_5d, d_matrix_array_3d);

}
parameters {
  real y_p;
}
transformed parameters {
  real transformed_param_real;
  real transformed_param_real_array[d_int];
  real transformed_param_real_array_2d[d_int,2];
  real transformed_param_real_array_3d[d_int,2,3];
  matrix[d_int,d_int] transformed_param_matrix;
  matrix[d_int,d_int] transformed_param_matrix_array[d_int];
  matrix[d_int,d_int] transformed_param_matrix_array_2d[d_int,2];
  matrix[d_int,d_int] transformed_param_matrix_array_3d[d_int,2,3];
  vector[d_int] transformed_param_vector;
  vector[d_int] transformed_param_vector_array[d_int];
  vector[d_int] transformed_param_vector_array_2d[d_int,2];
  vector[d_int] transformed_param_vector_array_3d[d_int,2,3];
  row_vector[d_int] transformed_param_row_vector;
  row_vector[d_int] transformed_param_row_vector_array[d_int];
  row_vector[d_int] transformed_param_row_vector_array_2d[d_int,2];
  row_vector[d_int] transformed_param_row_vector_array_3d[d_int,2,3];

  transformed_param_real_array = binary_log_loss(d_int_array, transformed_param_real);
  transformed_param_real_array = binary_log_loss(d_int_array, transformed_param_real_array);
  transformed_param_real_array_2d = binary_log_loss(d_int_array_2d, transformed_param_real);
  transformed_param_real_array_2d = binary_log_loss(d_int_array_2d, transformed_param_real_array_2d);
  transformed_param_real_array_3d = binary_log_loss(d_int_array_3d, transformed_param_real);
  transformed_param_real_array_3d = binary_log_loss(d_int_array_3d, transformed_param_real_array_3d);

  transformed_param_real_array = binary_log_loss(d_int, transformed_param_real_array);
  transformed_param_real_array_2d = binary_log_loss(d_int, transformed_param_real_array_2d);
  transformed_param_real_array_3d = binary_log_loss(d_int, transformed_param_real_array_3d);

  transformed_param_vector = binary_log_loss(d_int, transformed_param_vector);
  transformed_param_vector = binary_log_loss(d_int_array, transformed_param_vector);
  transformed_param_vector_array = binary_log_loss(d_int, transformed_param_vector_array);
  transformed_param_vector_array = binary_log_loss(d_int_array_2d, transformed_param_vector_array);
  transformed_param_vector_array_2d = binary_log_loss(d_int, transformed_param_vector_array_2d);
  transformed_param_vector_array_2d = binary_log_loss(d_int_array_3d, transformed_param_vector_array_2d);
  transformed_param_vector_array_3d = binary_log_loss(d_int, transformed_param_vector_array_3d);
  transformed_param_vector_array_3d = binary_log_loss(d_int_array_4d, transformed_param_vector_array_3d);

  transformed_param_row_vector = binary_log_loss(d_int, transformed_param_row_vector);
  transformed_param_row_vector = binary_log_loss(d_int_array, transformed_param_row_vector);
  transformed_param_row_vector_array = binary_log_loss(d_int, transformed_param_row_vector_array);
  transformed_param_row_vector_array = binary_log_loss(d_int_array_2d, transformed_param_row_vector_array);
  transformed_param_row_vector_array_2d = binary_log_loss(d_int, transformed_param_row_vector_array_2d);
  transformed_param_row_vector_array_2d = binary_log_loss(d_int_array_3d, transformed_param_row_vector_array_2d);
  transformed_param_row_vector_array_3d = binary_log_loss(d_int, transformed_param_row_vector_array_3d);
  transformed_param_row_vector_array_3d = binary_log_loss(d_int_array_4d, transformed_param_row_vector_array_3d);

  transformed_param_matrix = binary_log_loss(d_int, transformed_param_matrix);
  transformed_param_matrix = binary_log_loss(d_int_array_2d, transformed_param_matrix);
  transformed_param_matrix_array = binary_log_loss(d_int, transformed_param_matrix_array);
  transformed_param_matrix_array = binary_log_loss(d_int_array_3d, transformed_param_matrix_array);
  transformed_param_matrix_array_2d = binary_log_loss(d_int, transformed_param_matrix_array_2d);
  transformed_param_matrix_array_2d = binary_log_loss(d_int_array_4d, transformed_param_matrix_array_2d);
  transformed_param_matrix_array_3d = binary_log_loss(d_int, transformed_param_matrix_array_3d);
  transformed_param_matrix_array_3d = binary_log_loss(d_int_array_5d, transformed_param_matrix_array_3d);
}
model {  
  y_p ~ normal(0,1);
}
