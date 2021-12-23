data {
  int d_int;
  array[d_int] int d_int_array;
  array[d_int, 2] int d_int_array_2d;
  array[d_int, 2, 3] int d_int_array_3d;
  array[d_int, 2, 3, 4] int d_int_array_4d;
  array[d_int, 2, 3, 4, 5] int d_int_array_5d;
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
  
  transformed_data_real_array = falling_factorial(d_int, d_int_array);
  transformed_data_real_array = falling_factorial(d_real, d_int_array);
  transformed_data_real_array = falling_factorial(d_int_array, d_int_array);
  transformed_data_real_array = falling_factorial(d_real_array, d_int_array);
  transformed_data_real_array_2d = falling_factorial(d_real, d_int_array_2d);
  transformed_data_real_array_2d = falling_factorial(d_int, d_int_array_2d);
  transformed_data_real_array_2d = falling_factorial(d_real_array_2d,
                                                     d_int_array_2d);
  transformed_data_real_array_2d = falling_factorial(d_int_array_2d,
                                                     d_int_array_2d);
  transformed_data_real_array_3d = falling_factorial(d_real, d_int_array_3d);
  transformed_data_real_array_3d = falling_factorial(d_real_array_3d,
                                                     d_int_array_3d);
  transformed_data_real_array_3d = falling_factorial(d_int, d_int_array_3d);
  transformed_data_real_array_3d = falling_factorial(d_int_array_3d,
                                                     d_int_array_3d);
  
  transformed_data_real_array = falling_factorial(d_real_array, d_int);
  transformed_data_real_array_2d = falling_factorial(d_real_array_2d, d_int);
  transformed_data_real_array_3d = falling_factorial(d_real_array_3d, d_int);
  
  transformed_data_vector = falling_factorial(d_vector, d_int);
  transformed_data_vector = falling_factorial(d_vector, d_int_array);
  transformed_data_vector_array = falling_factorial(d_vector_array, d_int);
  transformed_data_vector_array = falling_factorial(d_vector_array,
                                                    d_int_array_2d);
  transformed_data_vector_array_2d = falling_factorial(d_vector_array_2d,
                                                       d_int);
  transformed_data_vector_array_2d = falling_factorial(d_vector_array_2d,
                                                       d_int_array_3d);
  transformed_data_vector_array_3d = falling_factorial(d_vector_array_3d,
                                                       d_int);
  transformed_data_vector_array_3d = falling_factorial(d_vector_array_3d,
                                                       d_int_array_4d);
  
  transformed_data_row_vector = falling_factorial(d_row_vector, d_int);
  transformed_data_row_vector = falling_factorial(d_row_vector, d_int_array);
  transformed_data_row_vector_array = falling_factorial(d_row_vector_array,
                                                        d_int);
  transformed_data_row_vector_array = falling_factorial(d_row_vector_array,
                                                        d_int_array_2d);
  transformed_data_row_vector_array_2d = falling_factorial(d_row_vector_array_2d,
                                                           d_int);
  transformed_data_row_vector_array_2d = falling_factorial(d_row_vector_array_2d,
                                                           d_int_array_3d);
  transformed_data_row_vector_array_3d = falling_factorial(d_row_vector_array_3d,
                                                           d_int);
  transformed_data_row_vector_array_3d = falling_factorial(d_row_vector_array_3d,
                                                           d_int_array_4d);
  
  transformed_data_matrix = falling_factorial(d_matrix, d_int);
  transformed_data_matrix = falling_factorial(d_matrix, d_int_array_2d);
  transformed_data_matrix_array = falling_factorial(d_matrix_array, d_int);
  transformed_data_matrix_array = falling_factorial(d_matrix_array,
                                                    d_int_array_3d);
  transformed_data_matrix_array_2d = falling_factorial(d_matrix_array_2d,
                                                       d_int);
  transformed_data_matrix_array_2d = falling_factorial(d_matrix_array_2d,
                                                       d_int_array_4d);
  transformed_data_matrix_array_3d = falling_factorial(d_matrix_array_3d,
                                                       d_int);
  transformed_data_matrix_array_3d = falling_factorial(d_matrix_array_3d,
                                                       d_int_array_5d);
}
parameters {
  real y_p;
}
transformed parameters {
  real transformed_param_real;
  array[d_int] real transformed_param_real_array;
  array[d_int, 2] real transformed_param_real_array_2d;
  array[d_int, 2, 3] real transformed_param_real_array_3d;
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
  
  transformed_param_real_array = falling_factorial(transformed_param_real,
                                                   d_int_array);
  transformed_param_real_array = falling_factorial(transformed_param_real_array,
                                                   d_int_array);
  transformed_param_real_array_2d = falling_factorial(transformed_param_real,
                                                      d_int_array_2d);
  transformed_param_real_array_2d = falling_factorial(transformed_param_real_array_2d,
                                                      d_int_array_2d);
  transformed_param_real_array_3d = falling_factorial(transformed_param_real,
                                                      d_int_array_3d);
  transformed_param_real_array_3d = falling_factorial(transformed_param_real_array_3d,
                                                      d_int_array_3d);
  
  transformed_param_real_array = falling_factorial(transformed_param_real_array,
                                                   d_int);
  transformed_param_real_array_2d = falling_factorial(transformed_param_real_array_2d,
                                                      d_int);
  transformed_param_real_array_3d = falling_factorial(transformed_param_real_array_3d,
                                                      d_int);
  
  transformed_param_vector = falling_factorial(transformed_param_vector,
                                               d_int);
  transformed_param_vector = falling_factorial(transformed_param_vector,
                                               d_int_array);
  transformed_param_vector_array = falling_factorial(transformed_param_vector_array,
                                                     d_int);
  transformed_param_vector_array = falling_factorial(transformed_param_vector_array,
                                                     d_int_array_2d);
  transformed_param_vector_array_2d = falling_factorial(transformed_param_vector_array_2d,
                                                        d_int);
  transformed_param_vector_array_2d = falling_factorial(transformed_param_vector_array_2d,
                                                        d_int_array_3d);
  transformed_param_vector_array_3d = falling_factorial(transformed_param_vector_array_3d,
                                                        d_int);
  transformed_param_vector_array_3d = falling_factorial(transformed_param_vector_array_3d,
                                                        d_int_array_4d);
  
  transformed_param_row_vector = falling_factorial(transformed_param_row_vector,
                                                   d_int);
  transformed_param_row_vector = falling_factorial(transformed_param_row_vector,
                                                   d_int_array);
  transformed_param_row_vector_array = falling_factorial(transformed_param_row_vector_array,
                                                         d_int);
  transformed_param_row_vector_array = falling_factorial(transformed_param_row_vector_array,
                                                         d_int_array_2d);
  transformed_param_row_vector_array_2d = falling_factorial(transformed_param_row_vector_array_2d,
                                                            d_int);
  transformed_param_row_vector_array_2d = falling_factorial(transformed_param_row_vector_array_2d,
                                                            d_int_array_3d);
  transformed_param_row_vector_array_3d = falling_factorial(transformed_param_row_vector_array_3d,
                                                            d_int);
  transformed_param_row_vector_array_3d = falling_factorial(transformed_param_row_vector_array_3d,
                                                            d_int_array_4d);
  
  transformed_param_matrix = falling_factorial(transformed_param_matrix,
                                               d_int);
  transformed_param_matrix = falling_factorial(transformed_param_matrix,
                                               d_int_array_2d);
  transformed_param_matrix_array = falling_factorial(transformed_param_matrix_array,
                                                     d_int);
  transformed_param_matrix_array = falling_factorial(transformed_param_matrix_array,
                                                     d_int_array_3d);
  transformed_param_matrix_array_2d = falling_factorial(transformed_param_matrix_array_2d,
                                                        d_int);
  transformed_param_matrix_array_2d = falling_factorial(transformed_param_matrix_array_2d,
                                                        d_int_array_4d);
  transformed_param_matrix_array_3d = falling_factorial(transformed_param_matrix_array_3d,
                                                        d_int);
  transformed_param_matrix_array_3d = falling_factorial(transformed_param_matrix_array_3d,
                                                        d_int_array_5d);
}
model {
  y_p ~ normal(0, 1);
}

