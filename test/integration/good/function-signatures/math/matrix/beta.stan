data {
  int d_int;
  array[d_int] int d_int_array;
  array[d_int, 2] int d_int_array_2d;
  array[d_int, 2, 3] int d_int_array_3d;
  real d_real;
  array[d_int] real d_real_array;
  array[d_int, 2] real d_real_array_2d;
  array[d_int, 2, 3] real d_real_array_3d;
  matrix[d_int, d_int] d_matrix;
  array[d_int] matrix[d_int, d_int] d_matrix_array;
  array[d_int, 2] matrix[d_int, d_int] d_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] d_matrix_array_3d;
  vector[d_int] d_vector;
  array[d_int] vector[d_int] d_vector_array;
  array[d_int, 2] vector[d_int] d_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] d_vector_array_3d;
  row_vector[d_int] d_row_vector;
  array[d_int] row_vector[d_int] d_row_vector_array;
  array[d_int, 2] row_vector[d_int] d_row_vector_array_2d;
  array[d_int, 2, 3] row_vector[d_int] d_row_vector_array_3d;
}
transformed data {
  real transformed_data_real;
  array[d_int] real transformed_data_real_array;
  array[d_int, 2] real transformed_data_real_array_2d;
  array[d_int, 2, 3] real transformed_data_real_array_3d;
  matrix[d_int, d_int] transformed_data_matrix;
  array[d_int] matrix[d_int, d_int] transformed_data_matrix_array;
  array[d_int, 2] matrix[d_int, d_int] transformed_data_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] transformed_data_matrix_array_3d;
  vector[d_int] transformed_data_vector;
  array[d_int] vector[d_int] transformed_data_vector_array;
  array[d_int, 2] vector[d_int] transformed_data_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] transformed_data_vector_array_3d;
  row_vector[d_int] transformed_data_row_vector;
  array[d_int] row_vector[d_int] transformed_data_row_vector_array;
  array[d_int, 2] row_vector[d_int] transformed_data_row_vector_array_2d;
  array[d_int, 2, 3] row_vector[d_int] transformed_data_row_vector_array_3d;
  
  transformed_data_real_array = beta(d_int_array, d_int);
  transformed_data_real_array = beta(d_int_array, d_real);
  transformed_data_real_array_2d = beta(d_int_array_2d, d_int);
  transformed_data_real_array_2d = beta(d_int_array_2d, d_real);
  transformed_data_real_array_3d = beta(d_int_array_3d, d_int);
  transformed_data_real_array_3d = beta(d_int_array_3d, d_real);
  
  transformed_data_real_array = beta(d_int, d_int_array);
  transformed_data_real_array = beta(d_real, d_int_array);
  transformed_data_real_array_2d = beta(d_int, d_int_array_2d);
  transformed_data_real_array_2d = beta(d_real, d_int_array_2d);
  transformed_data_real_array_3d = beta(d_int, d_int_array_3d);
  transformed_data_real_array_3d = beta(d_real, d_int_array_3d);
  
  transformed_data_real_array = beta(d_int, d_real_array);
  transformed_data_real_array = beta(d_real, d_real_array);
  transformed_data_real_array_2d = beta(d_int, d_real_array_2d);
  transformed_data_real_array_2d = beta(d_real, d_real_array_2d);
  transformed_data_real_array_3d = beta(d_int, d_real_array_3d);
  transformed_data_real_array_3d = beta(d_real, d_real_array_3d);
  
  transformed_data_real_array = beta(d_real_array, d_int);
  transformed_data_real_array = beta(d_real_array, d_real);
  transformed_data_real_array_2d = beta(d_real_array_2d, d_int);
  transformed_data_real_array_2d = beta(d_real_array_2d, d_real);
  transformed_data_real_array_3d = beta(d_real_array_3d, d_int);
  transformed_data_real_array_3d = beta(d_real_array_3d, d_real);
  
  transformed_data_real_array = beta(d_int_array, d_int_array);
  transformed_data_real_array = beta(d_real_array, d_real_array);
  transformed_data_real_array_2d = beta(d_int_array_2d, d_int_array_2d);
  transformed_data_real_array_2d = beta(d_real_array_2d, d_real_array_2d);
  transformed_data_real_array_3d = beta(d_int_array_3d, d_int_array_3d);
  transformed_data_real_array_3d = beta(d_real_array_3d, d_real_array_3d);
  
  transformed_data_vector = beta(d_vector, d_int);
  transformed_data_vector = beta(d_vector, d_real);
  transformed_data_vector_array = beta(d_vector_array, d_int);
  transformed_data_vector_array = beta(d_vector_array, d_real);
  transformed_data_vector_array_2d = beta(d_vector_array_2d, d_int);
  transformed_data_vector_array_2d = beta(d_vector_array_2d, d_real);
  transformed_data_vector_array_3d = beta(d_vector_array_3d, d_int);
  transformed_data_vector_array_3d = beta(d_vector_array_3d, d_real);
  
  transformed_data_vector = beta(d_int, d_vector);
  transformed_data_vector = beta(d_real, d_vector);
  transformed_data_vector_array = beta(d_int, d_vector_array);
  transformed_data_vector_array = beta(d_real, d_vector_array);
  transformed_data_vector_array_2d = beta(d_int, d_vector_array_2d);
  transformed_data_vector_array_2d = beta(d_real, d_vector_array_2d);
  transformed_data_vector_array_3d = beta(d_int, d_vector_array_3d);
  transformed_data_vector_array_3d = beta(d_real, d_vector_array_3d);
  
  transformed_data_vector = beta(d_vector, d_vector);
  transformed_data_vector_array = beta(d_vector_array, d_vector_array);
  transformed_data_vector_array_2d = beta(d_vector_array_2d,
                                          d_vector_array_2d);
  transformed_data_vector_array_3d = beta(d_vector_array_3d,
                                          d_vector_array_3d);
  
  transformed_data_row_vector = beta(d_row_vector, d_int);
  transformed_data_row_vector = beta(d_row_vector, d_real);
  transformed_data_row_vector_array = beta(d_row_vector_array, d_int);
  transformed_data_row_vector_array = beta(d_row_vector_array, d_real);
  transformed_data_row_vector_array_2d = beta(d_row_vector_array_2d, d_int);
  transformed_data_row_vector_array_2d = beta(d_row_vector_array_2d, d_real);
  transformed_data_row_vector_array_3d = beta(d_row_vector_array_3d, d_int);
  transformed_data_row_vector_array_3d = beta(d_row_vector_array_3d, d_real);
  
  transformed_data_row_vector = beta(d_int, d_row_vector);
  transformed_data_row_vector = beta(d_real, d_row_vector);
  transformed_data_row_vector_array = beta(d_int, d_row_vector_array);
  transformed_data_row_vector_array = beta(d_real, d_row_vector_array);
  transformed_data_row_vector_array_2d = beta(d_int, d_row_vector_array_2d);
  transformed_data_row_vector_array_2d = beta(d_real, d_row_vector_array_2d);
  transformed_data_row_vector_array_3d = beta(d_int, d_row_vector_array_3d);
  transformed_data_row_vector_array_3d = beta(d_real, d_row_vector_array_3d);
  
  transformed_data_row_vector = beta(d_row_vector, d_row_vector);
  transformed_data_row_vector_array = beta(d_row_vector_array,
                                           d_row_vector_array);
  transformed_data_row_vector_array_2d = beta(d_row_vector_array_2d,
                                              d_row_vector_array_2d);
  transformed_data_row_vector_array_3d = beta(d_row_vector_array_3d,
                                              d_row_vector_array_3d);
  
  transformed_data_matrix = beta(d_matrix, d_int);
  transformed_data_matrix = beta(d_matrix, d_real);
  transformed_data_matrix_array = beta(d_matrix_array, d_int);
  transformed_data_matrix_array = beta(d_matrix_array, d_real);
  transformed_data_matrix_array_2d = beta(d_matrix_array_2d, d_int);
  transformed_data_matrix_array_2d = beta(d_matrix_array_2d, d_real);
  transformed_data_matrix_array_3d = beta(d_matrix_array_3d, d_int);
  transformed_data_matrix_array_3d = beta(d_matrix_array_3d, d_real);
  
  transformed_data_matrix = beta(d_int, d_matrix);
  transformed_data_matrix = beta(d_real, d_matrix);
  transformed_data_matrix_array = beta(d_int, d_matrix_array);
  transformed_data_matrix_array = beta(d_real, d_matrix_array);
  transformed_data_matrix_array_2d = beta(d_int, d_matrix_array_2d);
  transformed_data_matrix_array_2d = beta(d_real, d_matrix_array_2d);
  transformed_data_matrix_array_3d = beta(d_int, d_matrix_array_3d);
  transformed_data_matrix_array_3d = beta(d_real, d_matrix_array_3d);
  
  transformed_data_matrix = beta(d_matrix, d_matrix);
  transformed_data_matrix_array = beta(d_matrix_array, d_matrix_array);
  transformed_data_matrix_array_2d = beta(d_matrix_array_2d,
                                          d_matrix_array_2d);
  transformed_data_matrix_array_3d = beta(d_matrix_array_3d,
                                          d_matrix_array_3d);
}
parameters {
  real p_real;
  array[d_int] real p_real_array;
  array[d_int, 2] real p_real_array_2d;
  array[d_int, 2, 3] real p_real_array_3d;
  matrix[d_int, d_int] p_matrix;
  array[d_int] matrix[d_int, d_int] p_matrix_array;
  array[d_int, 2] matrix[d_int, d_int] p_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] p_matrix_array_3d;
  vector[d_int] p_vector;
  array[d_int] vector[d_int] p_vector_array;
  array[d_int, 2] vector[d_int] p_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] p_vector_array_3d;
  row_vector[d_int] p_row_vector;
  array[d_int] row_vector[d_int] p_row_vector_array;
  array[d_int, 2] row_vector[d_int] p_row_vector_array_2d;
  array[d_int, 2, 3] row_vector[d_int] p_row_vector_array_3d;
  real y_p;
}
transformed parameters {
  array[d_int] real transformed_param_array;
  array[d_int, 2] real transformed_param_array_2d;
  array[d_int, 2, 3] real transformed_param_array_3d;
  matrix[d_int, d_int] transformed_param_matrix;
  array[d_int] matrix[d_int, d_int] transformed_param_matrix_array;
  array[d_int, 2] matrix[d_int, d_int] transformed_param_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] transformed_param_matrix_array_3d;
  vector[d_int] transformed_param_vector;
  array[d_int] vector[d_int] transformed_param_vector_array;
  array[d_int, 2] vector[d_int] transformed_param_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] transformed_param_vector_array_3d;
  row_vector[d_int] transformed_param_row_vector;
  array[d_int] row_vector[d_int] transformed_param_row_vector_array;
  array[d_int, 2] row_vector[d_int] transformed_param_row_vector_array_2d;
  array[d_int, 2, 3] row_vector[d_int] transformed_param_row_vector_array_3d;
  
  transformed_param_array = beta(d_int_array, p_real);
  transformed_param_array_2d = beta(d_int_array_2d, p_real);
  transformed_param_array_3d = beta(d_int_array_3d, p_real);
  
  transformed_param_array = beta(p_real, d_int_array);
  transformed_param_array_2d = beta(p_real, d_int_array_2d);
  transformed_param_array_3d = beta(p_real, d_int_array_3d);
  
  transformed_param_array = beta(p_real_array, d_int);
  transformed_param_array = beta(p_real_array, d_real);
  transformed_param_array = beta(p_real_array, p_real);
  transformed_param_array = beta(d_real_array, p_real);
  transformed_param_array_2d = beta(p_real_array_2d, d_int);
  transformed_param_array_2d = beta(p_real_array_2d, d_real);
  transformed_param_array_2d = beta(p_real_array_2d, p_real);
  transformed_param_array_2d = beta(d_real_array_2d, p_real);
  transformed_param_array_3d = beta(p_real_array_3d, d_int);
  transformed_param_array_3d = beta(p_real_array_3d, d_real);
  transformed_param_array_3d = beta(p_real_array_3d, p_real);
  transformed_param_array_3d = beta(d_real_array_3d, p_real);
  
  transformed_param_array = beta(d_int, p_real_array);
  transformed_param_array = beta(d_real, p_real_array);
  transformed_param_array = beta(p_real, p_real_array);
  transformed_param_array = beta(p_real, d_real_array);
  transformed_param_array_2d = beta(d_int, p_real_array_2d);
  transformed_param_array_2d = beta(d_real, p_real_array_2d);
  transformed_param_array_2d = beta(p_real, p_real_array_2d);
  transformed_param_array_2d = beta(p_real, d_real_array_2d);
  transformed_param_array_3d = beta(d_int, p_real_array_3d);
  transformed_param_array_3d = beta(d_real, p_real_array_3d);
  transformed_param_array_3d = beta(p_real, p_real_array_3d);
  transformed_param_array_3d = beta(p_real, d_real_array_3d);
  
  transformed_param_array = beta(d_real_array, p_real_array);
  transformed_param_array = beta(p_real_array, d_real_array);
  transformed_param_array = beta(p_real_array, p_real_array);
  transformed_param_array_2d = beta(d_real_array_2d, p_real_array_2d);
  transformed_param_array_2d = beta(p_real_array_2d, d_real_array_2d);
  transformed_param_array_2d = beta(p_real_array_2d, p_real_array_2d);
  transformed_param_array_3d = beta(d_real_array_3d, p_real_array_3d);
  transformed_param_array_3d = beta(p_real_array_3d, d_real_array_3d);
  transformed_param_array_3d = beta(p_real_array_3d, p_real_array_3d);
  
  transformed_param_vector = beta(p_vector, p_real);
  transformed_param_vector = beta(p_vector, d_real);
  transformed_param_vector = beta(p_vector, d_int);
  transformed_param_vector = beta(d_vector, p_real);
  transformed_param_vector_array = beta(p_vector_array, p_real);
  transformed_param_vector_array = beta(p_vector_array, d_real);
  transformed_param_vector_array = beta(p_vector_array, d_int);
  transformed_param_vector_array = beta(d_vector_array, p_real);
  transformed_param_vector_array_2d = beta(p_vector_array_2d, p_real);
  transformed_param_vector_array_2d = beta(p_vector_array_2d, d_real);
  transformed_param_vector_array_2d = beta(p_vector_array_2d, d_int);
  transformed_param_vector_array_2d = beta(d_vector_array_2d, p_real);
  transformed_param_vector_array_3d = beta(p_vector_array_3d, p_real);
  transformed_param_vector_array_3d = beta(p_vector_array_3d, d_real);
  transformed_param_vector_array_3d = beta(p_vector_array_3d, d_int);
  transformed_param_vector_array_3d = beta(d_vector_array_3d, p_real);
  
  transformed_param_vector = beta(p_real, p_vector);
  transformed_param_vector = beta(p_real, d_vector);
  transformed_param_vector = beta(d_real, p_vector);
  transformed_param_vector = beta(d_int, p_vector);
  transformed_param_vector_array = beta(p_real, p_vector_array);
  transformed_param_vector_array = beta(p_real, d_vector_array);
  transformed_param_vector_array = beta(d_real, p_vector_array);
  transformed_param_vector_array = beta(d_int, p_vector_array);
  transformed_param_vector_array_2d = beta(p_real, p_vector_array_2d);
  transformed_param_vector_array_2d = beta(p_real, d_vector_array_2d);
  transformed_param_vector_array_2d = beta(d_real, p_vector_array_2d);
  transformed_param_vector_array_2d = beta(d_int, p_vector_array_2d);
  transformed_param_vector_array_3d = beta(p_real, p_vector_array_3d);
  transformed_param_vector_array_3d = beta(p_real, d_vector_array_3d);
  transformed_param_vector_array_3d = beta(d_real, p_vector_array_3d);
  transformed_param_vector_array_3d = beta(d_int, p_vector_array_3d);
  
  transformed_param_vector = beta(p_vector, p_vector);
  transformed_param_vector = beta(p_vector, d_vector);
  transformed_param_vector = beta(d_vector, p_vector);
  transformed_param_vector_array = beta(p_vector_array, p_vector_array);
  transformed_param_vector_array = beta(p_vector_array, d_vector_array);
  transformed_param_vector_array = beta(d_vector_array, p_vector_array);
  transformed_param_vector_array_2d = beta(p_vector_array_2d,
                                           p_vector_array_2d);
  transformed_param_vector_array_2d = beta(p_vector_array_2d,
                                           d_vector_array_2d);
  transformed_param_vector_array_2d = beta(d_vector_array_2d,
                                           p_vector_array_2d);
  transformed_param_vector_array_3d = beta(p_vector_array_3d,
                                           p_vector_array_3d);
  transformed_param_vector_array_3d = beta(p_vector_array_3d,
                                           d_vector_array_3d);
  transformed_param_vector_array_3d = beta(d_vector_array_3d,
                                           p_vector_array_3d);
  
  transformed_param_row_vector = beta(p_row_vector, p_real);
  transformed_param_row_vector = beta(p_row_vector, d_real);
  transformed_param_row_vector = beta(p_row_vector, d_int);
  transformed_param_row_vector = beta(d_row_vector, p_real);
  transformed_param_row_vector_array = beta(p_row_vector_array, p_real);
  transformed_param_row_vector_array = beta(p_row_vector_array, d_real);
  transformed_param_row_vector_array = beta(p_row_vector_array, d_int);
  transformed_param_row_vector_array = beta(d_row_vector_array, p_real);
  transformed_param_row_vector_array_2d = beta(p_row_vector_array_2d, p_real);
  transformed_param_row_vector_array_2d = beta(p_row_vector_array_2d, d_real);
  transformed_param_row_vector_array_2d = beta(p_row_vector_array_2d, d_int);
  transformed_param_row_vector_array_2d = beta(d_row_vector_array_2d, p_real);
  transformed_param_row_vector_array_3d = beta(p_row_vector_array_3d, p_real);
  transformed_param_row_vector_array_3d = beta(p_row_vector_array_3d, d_real);
  transformed_param_row_vector_array_3d = beta(p_row_vector_array_3d, d_int);
  transformed_param_row_vector_array_3d = beta(d_row_vector_array_3d, p_real);
  
  transformed_param_row_vector = beta(p_real, p_row_vector);
  transformed_param_row_vector = beta(d_real, p_row_vector);
  transformed_param_row_vector = beta(d_int, p_row_vector);
  transformed_param_row_vector = beta(p_real, d_row_vector);
  transformed_param_row_vector_array = beta(p_real, p_row_vector_array);
  transformed_param_row_vector_array = beta(d_real, p_row_vector_array);
  transformed_param_row_vector_array = beta(d_int, p_row_vector_array);
  transformed_param_row_vector_array = beta(p_real, d_row_vector_array);
  transformed_param_row_vector_array_2d = beta(p_real, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = beta(d_real, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = beta(d_int, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = beta(p_real, d_row_vector_array_2d);
  transformed_param_row_vector_array_3d = beta(p_real, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = beta(d_real, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = beta(d_int, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = beta(p_real, d_row_vector_array_3d);
  
  transformed_param_row_vector = beta(p_row_vector, p_row_vector);
  transformed_param_row_vector = beta(p_row_vector, d_row_vector);
  transformed_param_row_vector = beta(d_row_vector, p_row_vector);
  transformed_param_row_vector_array = beta(p_row_vector_array,
                                            p_row_vector_array);
  transformed_param_row_vector_array = beta(p_row_vector_array,
                                            d_row_vector_array);
  transformed_param_row_vector_array = beta(d_row_vector_array,
                                            p_row_vector_array);
  transformed_param_row_vector_array_2d = beta(p_row_vector_array_2d,
                                               p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = beta(p_row_vector_array_2d,
                                               d_row_vector_array_2d);
  transformed_param_row_vector_array_2d = beta(d_row_vector_array_2d,
                                               p_row_vector_array_2d);
  transformed_param_row_vector_array_3d = beta(p_row_vector_array_3d,
                                               p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = beta(p_row_vector_array_3d,
                                               d_row_vector_array_3d);
  transformed_param_row_vector_array_3d = beta(d_row_vector_array_3d,
                                               p_row_vector_array_3d);
  
  transformed_param_matrix = beta(p_matrix, p_real);
  transformed_param_matrix = beta(p_matrix, d_real);
  transformed_param_matrix = beta(p_matrix, d_int);
  transformed_param_matrix = beta(d_matrix, p_real);
  transformed_param_matrix_array = beta(p_matrix_array, p_real);
  transformed_param_matrix_array = beta(p_matrix_array, d_real);
  transformed_param_matrix_array = beta(p_matrix_array, d_int);
  transformed_param_matrix_array = beta(d_matrix_array, p_real);
  transformed_param_matrix_array_2d = beta(p_matrix_array_2d, p_real);
  transformed_param_matrix_array_2d = beta(p_matrix_array_2d, d_real);
  transformed_param_matrix_array_2d = beta(p_matrix_array_2d, d_int);
  transformed_param_matrix_array_2d = beta(d_matrix_array_2d, p_real);
  transformed_param_matrix_array_3d = beta(p_matrix_array_3d, p_real);
  transformed_param_matrix_array_3d = beta(p_matrix_array_3d, d_real);
  transformed_param_matrix_array_3d = beta(p_matrix_array_3d, d_int);
  transformed_param_matrix_array_3d = beta(d_matrix_array_3d, p_real);
  
  transformed_param_matrix = beta(p_real, p_matrix);
  transformed_param_matrix = beta(p_real, d_matrix);
  transformed_param_matrix = beta(d_real, p_matrix);
  transformed_param_matrix = beta(d_int, p_matrix);
  transformed_param_matrix_array = beta(p_real, p_matrix_array);
  transformed_param_matrix_array = beta(p_real, d_matrix_array);
  transformed_param_matrix_array = beta(d_real, p_matrix_array);
  transformed_param_matrix_array = beta(d_int, p_matrix_array);
  transformed_param_matrix_array_2d = beta(p_real, p_matrix_array_2d);
  transformed_param_matrix_array_2d = beta(p_real, d_matrix_array_2d);
  transformed_param_matrix_array_2d = beta(d_real, p_matrix_array_2d);
  transformed_param_matrix_array_2d = beta(d_int, p_matrix_array_2d);
  transformed_param_matrix_array_3d = beta(p_real, p_matrix_array_3d);
  transformed_param_matrix_array_3d = beta(p_real, d_matrix_array_3d);
  transformed_param_matrix_array_3d = beta(d_real, p_matrix_array_3d);
  transformed_param_matrix_array_3d = beta(d_int, p_matrix_array_3d);
  
  transformed_param_matrix = beta(p_matrix, p_matrix);
  transformed_param_matrix = beta(p_matrix, d_matrix);
  transformed_param_matrix = beta(d_matrix, p_matrix);
  transformed_param_matrix_array = beta(p_matrix_array, p_matrix_array);
  transformed_param_matrix_array = beta(p_matrix_array, d_matrix_array);
  transformed_param_matrix_array = beta(d_matrix_array, p_matrix_array);
  transformed_param_matrix_array_2d = beta(p_matrix_array_2d,
                                           p_matrix_array_2d);
  transformed_param_matrix_array_2d = beta(p_matrix_array_2d,
                                           d_matrix_array_2d);
  transformed_param_matrix_array_2d = beta(d_matrix_array_2d,
                                           p_matrix_array_2d);
  transformed_param_matrix_array_3d = beta(p_matrix_array_3d,
                                           p_matrix_array_3d);
  transformed_param_matrix_array_3d = beta(p_matrix_array_3d,
                                           d_matrix_array_3d);
  transformed_param_matrix_array_3d = beta(d_matrix_array_3d,
                                           p_matrix_array_3d);
}
model {
  y_p ~ normal(0, 1);
}

