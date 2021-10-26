data { 
  int d_int;
  int d_int_array[d_int];
  int d_int_array_2d[d_int,2];
  int d_int_array_3d[d_int,2,3];
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

  transformed_data_real_array = lchoose(d_int_array, d_int);
  transformed_data_real_array = lchoose(d_int_array, d_real);
  transformed_data_real_array_2d = lchoose(d_int_array_2d, d_int);
  transformed_data_real_array_2d = lchoose(d_int_array_2d, d_real);
  transformed_data_real_array_3d = lchoose(d_int_array_3d, d_int);
  transformed_data_real_array_3d = lchoose(d_int_array_3d, d_real);

  transformed_data_real_array = lchoose(d_int, d_int_array);
  transformed_data_real_array = lchoose(d_real, d_int_array);
  transformed_data_real_array_2d = lchoose(d_int, d_int_array_2d);
  transformed_data_real_array_2d = lchoose(d_real, d_int_array_2d);
  transformed_data_real_array_3d = lchoose(d_int, d_int_array_3d);
  transformed_data_real_array_3d = lchoose(d_real, d_int_array_3d);

  transformed_data_real_array = lchoose(d_int, d_real_array);
  transformed_data_real_array = lchoose(d_real, d_real_array);
  transformed_data_real_array_2d = lchoose(d_int, d_real_array_2d);
  transformed_data_real_array_2d = lchoose(d_real, d_real_array_2d);
  transformed_data_real_array_3d = lchoose(d_int, d_real_array_3d);
  transformed_data_real_array_3d = lchoose(d_real, d_real_array_3d);

  transformed_data_real_array = lchoose(d_real_array, d_int);
  transformed_data_real_array = lchoose(d_real_array, d_real);
  transformed_data_real_array_2d = lchoose(d_real_array_2d, d_int);
  transformed_data_real_array_2d = lchoose(d_real_array_2d, d_real);
  transformed_data_real_array_3d = lchoose(d_real_array_3d, d_int);
  transformed_data_real_array_3d = lchoose(d_real_array_3d, d_real);

  transformed_data_real_array = lchoose(d_int_array, d_int_array);
  transformed_data_real_array = lchoose(d_real_array, d_real_array);
  transformed_data_real_array_2d = lchoose(d_int_array_2d, d_int_array_2d);
  transformed_data_real_array_2d = lchoose(d_real_array_2d, d_real_array_2d);
  transformed_data_real_array_3d = lchoose(d_int_array_3d, d_int_array_3d);
  transformed_data_real_array_3d = lchoose(d_real_array_3d, d_real_array_3d);

  transformed_data_vector = lchoose(d_vector, d_int);
  transformed_data_vector = lchoose(d_vector, d_real);
  transformed_data_vector_array = lchoose(d_vector_array, d_int);
  transformed_data_vector_array = lchoose(d_vector_array, d_real);
  transformed_data_vector_array_2d = lchoose(d_vector_array_2d, d_int);
  transformed_data_vector_array_2d = lchoose(d_vector_array_2d, d_real);
  transformed_data_vector_array_3d = lchoose(d_vector_array_3d, d_int);
  transformed_data_vector_array_3d = lchoose(d_vector_array_3d, d_real);

  transformed_data_vector = lchoose(d_int, d_vector);
  transformed_data_vector = lchoose(d_real, d_vector);
  transformed_data_vector_array = lchoose(d_int, d_vector_array);
  transformed_data_vector_array = lchoose(d_real, d_vector_array);
  transformed_data_vector_array_2d = lchoose(d_int, d_vector_array_2d);
  transformed_data_vector_array_2d = lchoose(d_real, d_vector_array_2d);
  transformed_data_vector_array_3d = lchoose(d_int, d_vector_array_3d);
  transformed_data_vector_array_3d = lchoose(d_real, d_vector_array_3d);

  transformed_data_vector = lchoose(d_vector, d_vector);
  transformed_data_vector_array = lchoose(d_vector_array, d_vector_array);
  transformed_data_vector_array_2d = lchoose(d_vector_array_2d, d_vector_array_2d);
  transformed_data_vector_array_3d = lchoose(d_vector_array_3d, d_vector_array_3d);

  transformed_data_row_vector = lchoose(d_row_vector, d_int);
  transformed_data_row_vector = lchoose(d_row_vector, d_real);
  transformed_data_row_vector_array = lchoose(d_row_vector_array, d_int);
  transformed_data_row_vector_array = lchoose(d_row_vector_array, d_real);
  transformed_data_row_vector_array_2d = lchoose(d_row_vector_array_2d, d_int);
  transformed_data_row_vector_array_2d = lchoose(d_row_vector_array_2d, d_real);
  transformed_data_row_vector_array_3d = lchoose(d_row_vector_array_3d, d_int);
  transformed_data_row_vector_array_3d = lchoose(d_row_vector_array_3d, d_real);

  transformed_data_row_vector = lchoose(d_int, d_row_vector);
  transformed_data_row_vector = lchoose(d_real, d_row_vector);
  transformed_data_row_vector_array = lchoose(d_int, d_row_vector_array);
  transformed_data_row_vector_array = lchoose(d_real, d_row_vector_array);
  transformed_data_row_vector_array_2d = lchoose(d_int, d_row_vector_array_2d);
  transformed_data_row_vector_array_2d = lchoose(d_real, d_row_vector_array_2d);
  transformed_data_row_vector_array_3d = lchoose(d_int, d_row_vector_array_3d);
  transformed_data_row_vector_array_3d = lchoose(d_real, d_row_vector_array_3d);

  transformed_data_row_vector = lchoose(d_row_vector, d_row_vector);
  transformed_data_row_vector_array = lchoose(d_row_vector_array, d_row_vector_array);
  transformed_data_row_vector_array_2d = lchoose(d_row_vector_array_2d, d_row_vector_array_2d);
  transformed_data_row_vector_array_3d = lchoose(d_row_vector_array_3d, d_row_vector_array_3d);

  transformed_data_matrix = lchoose(d_matrix, d_int);
  transformed_data_matrix = lchoose(d_matrix, d_real);
  transformed_data_matrix_array = lchoose(d_matrix_array, d_int);
  transformed_data_matrix_array = lchoose(d_matrix_array, d_real);
  transformed_data_matrix_array_2d = lchoose(d_matrix_array_2d, d_int);
  transformed_data_matrix_array_2d = lchoose(d_matrix_array_2d, d_real);
  transformed_data_matrix_array_3d = lchoose(d_matrix_array_3d, d_int);
  transformed_data_matrix_array_3d = lchoose(d_matrix_array_3d, d_real);

  transformed_data_matrix = lchoose(d_int, d_matrix);
  transformed_data_matrix = lchoose(d_real, d_matrix);
  transformed_data_matrix_array = lchoose(d_int, d_matrix_array);
  transformed_data_matrix_array = lchoose(d_real, d_matrix_array);
  transformed_data_matrix_array_2d = lchoose(d_int, d_matrix_array_2d);
  transformed_data_matrix_array_2d = lchoose(d_real, d_matrix_array_2d);
  transformed_data_matrix_array_3d = lchoose(d_int, d_matrix_array_3d);
  transformed_data_matrix_array_3d = lchoose(d_real, d_matrix_array_3d);

  transformed_data_matrix = lchoose(d_matrix, d_matrix);
  transformed_data_matrix_array = lchoose(d_matrix_array, d_matrix_array);
  transformed_data_matrix_array_2d = lchoose(d_matrix_array_2d, d_matrix_array_2d);
  transformed_data_matrix_array_3d = lchoose(d_matrix_array_3d, d_matrix_array_3d);
}
parameters {
  real p_real;
  real p_real_array[d_int];
  real p_real_array_2d[d_int,2];
  real p_real_array_3d[d_int,2,3];
  matrix[d_int,d_int] p_matrix;
  matrix[d_int,d_int] p_matrix_array[d_int];
  matrix[d_int,d_int] p_matrix_array_2d[d_int,2];
  matrix[d_int,d_int] p_matrix_array_3d[d_int,2,3];
  vector[d_int] p_vector;
  vector[d_int] p_vector_array[d_int];
  vector[d_int] p_vector_array_2d[d_int,2];
  vector[d_int] p_vector_array_3d[d_int,2,3];
  row_vector[d_int] p_row_vector;
  row_vector[d_int] p_row_vector_array[d_int];
  row_vector[d_int] p_row_vector_array_2d[d_int,2];
  row_vector[d_int] p_row_vector_array_3d[d_int,2,3];
  real y_p;
}
transformed parameters {
  real transformed_param_array[d_int];
  real transformed_param_array_2d[d_int,2];
  real transformed_param_array_3d[d_int,2,3];
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

  transformed_param_array = lchoose(d_int_array, p_real);
  transformed_param_array_2d = lchoose(d_int_array_2d, p_real);
  transformed_param_array_3d = lchoose(d_int_array_3d, p_real);

  transformed_param_array = lchoose(p_real, d_int_array);
  transformed_param_array_2d = lchoose(p_real, d_int_array_2d);
  transformed_param_array_3d = lchoose(p_real, d_int_array_3d);

  transformed_param_array = lchoose(p_real_array, d_int);
  transformed_param_array = lchoose(p_real_array, d_real);
  transformed_param_array = lchoose(p_real_array, p_real);
  transformed_param_array = lchoose(d_real_array, p_real);
  transformed_param_array_2d = lchoose(p_real_array_2d, d_int);
  transformed_param_array_2d = lchoose(p_real_array_2d, d_real);
  transformed_param_array_2d = lchoose(p_real_array_2d, p_real);
  transformed_param_array_2d = lchoose(d_real_array_2d, p_real);
  transformed_param_array_3d = lchoose(p_real_array_3d, d_int);
  transformed_param_array_3d = lchoose(p_real_array_3d, d_real);
  transformed_param_array_3d = lchoose(p_real_array_3d, p_real);
  transformed_param_array_3d = lchoose(d_real_array_3d, p_real);

  transformed_param_array = lchoose(d_int, p_real_array);
  transformed_param_array = lchoose(d_real, p_real_array);
  transformed_param_array = lchoose(p_real, p_real_array);
  transformed_param_array = lchoose(p_real, d_real_array);
  transformed_param_array_2d = lchoose(d_int, p_real_array_2d);
  transformed_param_array_2d = lchoose(d_real, p_real_array_2d);
  transformed_param_array_2d = lchoose(p_real, p_real_array_2d);
  transformed_param_array_2d = lchoose(p_real, d_real_array_2d);
  transformed_param_array_3d = lchoose(d_int, p_real_array_3d);
  transformed_param_array_3d = lchoose(d_real, p_real_array_3d);
  transformed_param_array_3d = lchoose(p_real, p_real_array_3d);
  transformed_param_array_3d = lchoose(p_real, d_real_array_3d);

  transformed_param_array = lchoose(d_real_array, p_real_array);
  transformed_param_array = lchoose(p_real_array, d_real_array);
  transformed_param_array = lchoose(p_real_array, p_real_array);
  transformed_param_array_2d = lchoose(d_real_array_2d, p_real_array_2d);
  transformed_param_array_2d = lchoose(p_real_array_2d, d_real_array_2d);
  transformed_param_array_2d = lchoose(p_real_array_2d, p_real_array_2d);
  transformed_param_array_3d = lchoose(d_real_array_3d, p_real_array_3d);
  transformed_param_array_3d = lchoose(p_real_array_3d, d_real_array_3d);
  transformed_param_array_3d = lchoose(p_real_array_3d, p_real_array_3d);

  transformed_param_vector = lchoose(p_vector, p_real);
  transformed_param_vector = lchoose(p_vector, d_real);
  transformed_param_vector = lchoose(p_vector, d_int);
  transformed_param_vector = lchoose(d_vector, p_real);
  transformed_param_vector_array = lchoose(p_vector_array, p_real);
  transformed_param_vector_array = lchoose(p_vector_array, d_real);
  transformed_param_vector_array = lchoose(p_vector_array, d_int);
  transformed_param_vector_array = lchoose(d_vector_array, p_real);
  transformed_param_vector_array_2d = lchoose(p_vector_array_2d, p_real);
  transformed_param_vector_array_2d = lchoose(p_vector_array_2d, d_real);
  transformed_param_vector_array_2d = lchoose(p_vector_array_2d, d_int);
  transformed_param_vector_array_2d = lchoose(d_vector_array_2d, p_real);
  transformed_param_vector_array_3d = lchoose(p_vector_array_3d, p_real);
  transformed_param_vector_array_3d = lchoose(p_vector_array_3d, d_real);
  transformed_param_vector_array_3d = lchoose(p_vector_array_3d, d_int);
  transformed_param_vector_array_3d = lchoose(d_vector_array_3d, p_real);

  transformed_param_vector = lchoose(p_real, p_vector);
  transformed_param_vector = lchoose(p_real, d_vector);
  transformed_param_vector = lchoose(d_real, p_vector);
  transformed_param_vector = lchoose(d_int, p_vector);
  transformed_param_vector_array = lchoose(p_real, p_vector_array);
  transformed_param_vector_array = lchoose(p_real, d_vector_array);
  transformed_param_vector_array = lchoose(d_real, p_vector_array);
  transformed_param_vector_array = lchoose(d_int, p_vector_array);
  transformed_param_vector_array_2d = lchoose(p_real, p_vector_array_2d);
  transformed_param_vector_array_2d = lchoose(p_real, d_vector_array_2d);
  transformed_param_vector_array_2d = lchoose(d_real, p_vector_array_2d);
  transformed_param_vector_array_2d = lchoose(d_int, p_vector_array_2d);
  transformed_param_vector_array_3d = lchoose(p_real, p_vector_array_3d);
  transformed_param_vector_array_3d = lchoose(p_real, d_vector_array_3d);
  transformed_param_vector_array_3d = lchoose(d_real, p_vector_array_3d);
  transformed_param_vector_array_3d = lchoose(d_int, p_vector_array_3d);

  transformed_param_vector = lchoose(p_vector, p_vector);
  transformed_param_vector = lchoose(p_vector, d_vector);
  transformed_param_vector = lchoose(d_vector, p_vector);
  transformed_param_vector_array = lchoose(p_vector_array, p_vector_array);
  transformed_param_vector_array = lchoose(p_vector_array, d_vector_array);
  transformed_param_vector_array = lchoose(d_vector_array, p_vector_array);
  transformed_param_vector_array_2d = lchoose(p_vector_array_2d, p_vector_array_2d);
  transformed_param_vector_array_2d = lchoose(p_vector_array_2d, d_vector_array_2d);
  transformed_param_vector_array_2d = lchoose(d_vector_array_2d, p_vector_array_2d);
  transformed_param_vector_array_3d = lchoose(p_vector_array_3d, p_vector_array_3d);
  transformed_param_vector_array_3d = lchoose(p_vector_array_3d, d_vector_array_3d);
  transformed_param_vector_array_3d = lchoose(d_vector_array_3d, p_vector_array_3d);

  transformed_param_row_vector = lchoose(p_row_vector, p_real);
  transformed_param_row_vector = lchoose(p_row_vector, d_real);
  transformed_param_row_vector = lchoose(p_row_vector, d_int);
  transformed_param_row_vector = lchoose(d_row_vector, p_real);
  transformed_param_row_vector_array = lchoose(p_row_vector_array, p_real);
  transformed_param_row_vector_array = lchoose(p_row_vector_array, d_real);
  transformed_param_row_vector_array = lchoose(p_row_vector_array, d_int);
  transformed_param_row_vector_array = lchoose(d_row_vector_array, p_real);
  transformed_param_row_vector_array_2d = lchoose(p_row_vector_array_2d, p_real);
  transformed_param_row_vector_array_2d = lchoose(p_row_vector_array_2d, d_real);
  transformed_param_row_vector_array_2d = lchoose(p_row_vector_array_2d, d_int);
  transformed_param_row_vector_array_2d = lchoose(d_row_vector_array_2d, p_real);
  transformed_param_row_vector_array_3d = lchoose(p_row_vector_array_3d, p_real);
  transformed_param_row_vector_array_3d = lchoose(p_row_vector_array_3d, d_real);
  transformed_param_row_vector_array_3d = lchoose(p_row_vector_array_3d, d_int);
  transformed_param_row_vector_array_3d = lchoose(d_row_vector_array_3d, p_real);

  transformed_param_row_vector = lchoose(p_real, p_row_vector);
  transformed_param_row_vector = lchoose(d_real, p_row_vector);
  transformed_param_row_vector = lchoose(d_int, p_row_vector);
  transformed_param_row_vector = lchoose(p_real, d_row_vector);
  transformed_param_row_vector_array = lchoose(p_real, p_row_vector_array);
  transformed_param_row_vector_array = lchoose(d_real, p_row_vector_array);
  transformed_param_row_vector_array = lchoose(d_int, p_row_vector_array);
  transformed_param_row_vector_array = lchoose(p_real, d_row_vector_array);
  transformed_param_row_vector_array_2d = lchoose(p_real, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = lchoose(d_real, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = lchoose(d_int, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = lchoose(p_real, d_row_vector_array_2d);
  transformed_param_row_vector_array_3d = lchoose(p_real, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = lchoose(d_real, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = lchoose(d_int, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = lchoose(p_real, d_row_vector_array_3d);

  transformed_param_row_vector = lchoose(p_row_vector, p_row_vector);
  transformed_param_row_vector = lchoose(p_row_vector, d_row_vector);
  transformed_param_row_vector = lchoose(d_row_vector, p_row_vector);
  transformed_param_row_vector_array = lchoose(p_row_vector_array, p_row_vector_array);
  transformed_param_row_vector_array = lchoose(p_row_vector_array, d_row_vector_array);
  transformed_param_row_vector_array = lchoose(d_row_vector_array, p_row_vector_array);
  transformed_param_row_vector_array_2d = lchoose(p_row_vector_array_2d, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = lchoose(p_row_vector_array_2d, d_row_vector_array_2d);
  transformed_param_row_vector_array_2d = lchoose(d_row_vector_array_2d, p_row_vector_array_2d);
  transformed_param_row_vector_array_3d = lchoose(p_row_vector_array_3d, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = lchoose(p_row_vector_array_3d, d_row_vector_array_3d);
  transformed_param_row_vector_array_3d = lchoose(d_row_vector_array_3d, p_row_vector_array_3d);

  transformed_param_matrix = lchoose(p_matrix, p_real);
  transformed_param_matrix = lchoose(p_matrix, d_real);
  transformed_param_matrix = lchoose(p_matrix, d_int);
  transformed_param_matrix = lchoose(d_matrix, p_real);
  transformed_param_matrix_array = lchoose(p_matrix_array, p_real);
  transformed_param_matrix_array = lchoose(p_matrix_array, d_real);
  transformed_param_matrix_array = lchoose(p_matrix_array, d_int);
  transformed_param_matrix_array = lchoose(d_matrix_array, p_real);
  transformed_param_matrix_array_2d = lchoose(p_matrix_array_2d, p_real);
  transformed_param_matrix_array_2d = lchoose(p_matrix_array_2d, d_real);
  transformed_param_matrix_array_2d = lchoose(p_matrix_array_2d, d_int);
  transformed_param_matrix_array_2d = lchoose(d_matrix_array_2d, p_real);
  transformed_param_matrix_array_3d = lchoose(p_matrix_array_3d, p_real);
  transformed_param_matrix_array_3d = lchoose(p_matrix_array_3d, d_real);
  transformed_param_matrix_array_3d = lchoose(p_matrix_array_3d, d_int);
  transformed_param_matrix_array_3d = lchoose(d_matrix_array_3d, p_real);

  transformed_param_matrix = lchoose(p_real, p_matrix);
  transformed_param_matrix = lchoose(p_real, d_matrix);
  transformed_param_matrix = lchoose(d_real, p_matrix);
  transformed_param_matrix = lchoose(d_int, p_matrix);
  transformed_param_matrix_array = lchoose(p_real, p_matrix_array);
  transformed_param_matrix_array = lchoose(p_real, d_matrix_array);
  transformed_param_matrix_array = lchoose(d_real, p_matrix_array);
  transformed_param_matrix_array = lchoose(d_int, p_matrix_array);
  transformed_param_matrix_array_2d = lchoose(p_real, p_matrix_array_2d);
  transformed_param_matrix_array_2d = lchoose(p_real, d_matrix_array_2d);
  transformed_param_matrix_array_2d = lchoose(d_real, p_matrix_array_2d);
  transformed_param_matrix_array_2d = lchoose(d_int, p_matrix_array_2d);
  transformed_param_matrix_array_3d = lchoose(p_real, p_matrix_array_3d);
  transformed_param_matrix_array_3d = lchoose(p_real, d_matrix_array_3d);
  transformed_param_matrix_array_3d = lchoose(d_real, p_matrix_array_3d);
  transformed_param_matrix_array_3d = lchoose(d_int, p_matrix_array_3d);

  transformed_param_matrix = lchoose(p_matrix, p_matrix);
  transformed_param_matrix = lchoose(p_matrix, d_matrix);
  transformed_param_matrix = lchoose(d_matrix, p_matrix);
  transformed_param_matrix_array = lchoose(p_matrix_array, p_matrix_array);
  transformed_param_matrix_array = lchoose(p_matrix_array, d_matrix_array);
  transformed_param_matrix_array = lchoose(d_matrix_array, p_matrix_array);
  transformed_param_matrix_array_2d = lchoose(p_matrix_array_2d, p_matrix_array_2d);
  transformed_param_matrix_array_2d = lchoose(p_matrix_array_2d, d_matrix_array_2d);
  transformed_param_matrix_array_2d = lchoose(d_matrix_array_2d, p_matrix_array_2d);
  transformed_param_matrix_array_3d = lchoose(p_matrix_array_3d, p_matrix_array_3d);
  transformed_param_matrix_array_3d = lchoose(p_matrix_array_3d, d_matrix_array_3d);
  transformed_param_matrix_array_3d = lchoose(d_matrix_array_3d, p_matrix_array_3d);
}
model {  
  y_p ~ normal(0,1);
}
