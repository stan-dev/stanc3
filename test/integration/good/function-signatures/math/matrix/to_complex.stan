data {
  int d_int;
  array[d_int] int d_int_array;
  array[d_int, 2] int d_int_array_2d;
  array[d_int, 2, 3] int d_int_array_3d;
  real d;
  array[d_int] real d_array;
  array[d_int, 2] real d_array_2d;
  array[d_int, 2, 3] real d_array_3d;
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
  complex transformed_data;
  array[d_int] complex transformed_data_array;
  array[d_int, 2] complex transformed_data_array_2d;
  array[d_int, 2, 3] complex transformed_data_array_3d;
  complex_matrix[d_int, d_int] transformed_data_matrix;
  array[d_int] complex_matrix[d_int, d_int] transformed_data_matrix_array;
  array[d_int, 2] complex_matrix[d_int, d_int] transformed_data_matrix_array_2d;
  array[d_int, 2, 3] complex_matrix[d_int, d_int] transformed_data_matrix_array_3d;
  complex_vector[d_int] transformed_data_vector;
  array[d_int] complex_vector[d_int] transformed_data_vector_array;
  array[d_int, 2] complex_vector[d_int] transformed_data_vector_array_2d;
  array[d_int, 2, 3] complex_vector[d_int] transformed_data_vector_array_3d;
  complex_row_vector[d_int] transformed_data_row_vector;
  array[d_int] complex_row_vector[d_int] transformed_data_row_vector_array;
  array[d_int, 2] complex_row_vector[d_int] transformed_data_row_vector_array_2d;
  array[d_int, 2, 3] complex_row_vector[d_int] transformed_data_row_vector_array_3d;

  transformed_data_array = to_complex(d, d_array);
  transformed_data_array_2d = to_complex(d, d_array_2d);
  transformed_data_array_3d = to_complex(d, d_array_3d);

  transformed_data_array = to_complex(d_array, d);
  transformed_data_array_2d = to_complex(d_array_2d, d);
  transformed_data_array_3d = to_complex(d_array_3d, d);

  transformed_data_array = to_complex(d_array, d_array);
  transformed_data_array_2d = to_complex(d_array_2d, d_array_2d);
  transformed_data_array_3d = to_complex(d_array_3d, d_array_3d);

  transformed_data_vector = to_complex(d_vector, d);
  transformed_data_vector_array = to_complex(d_vector_array, d);
  transformed_data_vector_array_2d = to_complex(d_vector_array_2d, d);
  transformed_data_vector_array_3d = to_complex(d_vector_array_3d, d);

  transformed_data_vector = to_complex(d, d_vector);
  transformed_data_vector_array = to_complex(d, d_vector_array);
  transformed_data_vector_array_2d = to_complex(d, d_vector_array_2d);
  transformed_data_vector_array_3d = to_complex(d, d_vector_array_3d);

  transformed_data_vector = to_complex(d_vector, d_vector);
  transformed_data_vector_array = to_complex(d_vector_array, d_vector_array);
  transformed_data_vector_array_2d = to_complex(d_vector_array_2d,
                                             d_vector_array_2d);
  transformed_data_vector_array_3d = to_complex(d_vector_array_3d,
                                             d_vector_array_3d);

  transformed_data_row_vector = to_complex(d_row_vector, d);
  transformed_data_row_vector_array = to_complex(d_row_vector_array, d);
  transformed_data_row_vector_array_2d = to_complex(d_row_vector_array_2d,
                                                 d);
  transformed_data_row_vector_array_3d = to_complex(d_row_vector_array_3d,
                                                 d);

  transformed_data_row_vector = to_complex(d, d_row_vector);
  transformed_data_row_vector_array = to_complex(d, d_row_vector_array);
  transformed_data_row_vector_array_2d = to_complex(d,
                                                 d_row_vector_array_2d);
  transformed_data_row_vector_array_3d = to_complex(d,
                                                 d_row_vector_array_3d);

  transformed_data_row_vector = to_complex(d_row_vector, d_row_vector);
  transformed_data_row_vector_array = to_complex(d_row_vector_array,
                                              d_row_vector_array);
  transformed_data_row_vector_array_2d = to_complex(d_row_vector_array_2d,
                                                 d_row_vector_array_2d);
  transformed_data_row_vector_array_3d = to_complex(d_row_vector_array_3d,
                                                 d_row_vector_array_3d);

  transformed_data_matrix = to_complex(d_matrix, d);
  transformed_data_matrix_array = to_complex(d_matrix_array, d);
  transformed_data_matrix_array_2d = to_complex(d_matrix_array_2d, d);
  transformed_data_matrix_array_3d = to_complex(d_matrix_array_3d, d);

  transformed_data_matrix = to_complex(d, d_matrix);
  transformed_data_matrix_array = to_complex(d, d_matrix_array);
  transformed_data_matrix_array_2d = to_complex(d, d_matrix_array_2d);
  transformed_data_matrix_array_3d = to_complex(d, d_matrix_array_3d);

  transformed_data_matrix = to_complex(d_matrix, d_matrix);
  transformed_data_matrix_array = to_complex(d_matrix_array, d_matrix_array);
  transformed_data_matrix_array_2d = to_complex(d_matrix_array_2d,
                                             d_matrix_array_2d);
  transformed_data_matrix_array_3d = to_complex(d_matrix_array_3d,
                                             d_matrix_array_3d);
}
parameters {
  real p;
  array[d_int] real p_array;
  array[d_int, 2] real p_array_2d;
  array[d_int, 2, 3] real p_array_3d;
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
  array[d_int] complex transformed_param_array;
  array[d_int, 2] complex transformed_param_array_2d;
  array[d_int, 2, 3] complex transformed_param_array_3d;
  complex_matrix[d_int, d_int] transformed_param_matrix;
  array[d_int] complex_matrix[d_int, d_int] transformed_param_matrix_array;
  array[d_int, 2] complex_matrix[d_int, d_int] transformed_param_matrix_array_2d;
  array[d_int, 2, 3] complex_matrix[d_int, d_int] transformed_param_matrix_array_3d;
  complex_vector[d_int] transformed_param_vector;
  array[d_int] complex_vector[d_int] transformed_param_vector_array;
  array[d_int, 2] complex_vector[d_int] transformed_param_vector_array_2d;
  array[d_int, 2, 3] complex_vector[d_int] transformed_param_vector_array_3d;
  complex_row_vector[d_int] transformed_param_row_vector;
  array[d_int] complex_row_vector[d_int] transformed_param_row_vector_array;
  array[d_int, 2] complex_row_vector[d_int] transformed_param_row_vector_array_2d;
  array[d_int, 2, 3] complex_row_vector[d_int] transformed_param_row_vector_array_3d;

  transformed_param_array = to_complex(p_array, d);
  transformed_param_array = to_complex(p_array, p);
  transformed_param_array = to_complex(d_array, p);
  transformed_param_array_2d = to_complex(p_array_2d, d);
  transformed_param_array_2d = to_complex(p_array_2d, p);
  transformed_param_array_2d = to_complex(d_array_2d, p);
  transformed_param_array_3d = to_complex(p_array_3d, d);
  transformed_param_array_3d = to_complex(p_array_3d, p);
  transformed_param_array_3d = to_complex(d_array_3d, p);

  transformed_param_array = to_complex(d, p_array);
  transformed_param_array = to_complex(p, p_array);
  transformed_param_array = to_complex(p, d_array);
  transformed_param_array_2d = to_complex(d, p_array_2d);
  transformed_param_array_2d = to_complex(p, p_array_2d);
  transformed_param_array_2d = to_complex(p, d_array_2d);
  transformed_param_array_3d = to_complex(d, p_array_3d);
  transformed_param_array_3d = to_complex(p, p_array_3d);
  transformed_param_array_3d = to_complex(p, d_array_3d);

  transformed_param_array = to_complex(d_array, p_array);
  transformed_param_array = to_complex(p_array, d_array);
  transformed_param_array = to_complex(p_array, p_array);
  transformed_param_array_2d = to_complex(d_array_2d, p_array_2d);
  transformed_param_array_2d = to_complex(p_array_2d, d_array_2d);
  transformed_param_array_2d = to_complex(p_array_2d, p_array_2d);
  transformed_param_array_3d = to_complex(d_array_3d, p_array_3d);
  transformed_param_array_3d = to_complex(p_array_3d, d_array_3d);
  transformed_param_array_3d = to_complex(p_array_3d, p_array_3d);

  transformed_param_vector = to_complex(p_vector, p);
  transformed_param_vector = to_complex(p_vector, d);
  transformed_param_vector = to_complex(d_vector, p);
  transformed_param_vector_array = to_complex(p_vector_array, p);
  transformed_param_vector_array = to_complex(p_vector_array, d);
  transformed_param_vector_array = to_complex(d_vector_array, p);
  transformed_param_vector_array_2d = to_complex(p_vector_array_2d, p);
  transformed_param_vector_array_2d = to_complex(p_vector_array_2d, d);
  transformed_param_vector_array_2d = to_complex(d_vector_array_2d, p);
  transformed_param_vector_array_3d = to_complex(p_vector_array_3d, p);
  transformed_param_vector_array_3d = to_complex(p_vector_array_3d, d);
  transformed_param_vector_array_3d = to_complex(d_vector_array_3d, p);

  transformed_param_vector = to_complex(p, p_vector);
  transformed_param_vector = to_complex(p, d_vector);
  transformed_param_vector = to_complex(d, p_vector);
  transformed_param_vector_array = to_complex(p, p_vector_array);
  transformed_param_vector_array = to_complex(p, d_vector_array);
  transformed_param_vector_array = to_complex(d, p_vector_array);
  transformed_param_vector_array_2d = to_complex(p, p_vector_array_2d);
  transformed_param_vector_array_2d = to_complex(p, d_vector_array_2d);
  transformed_param_vector_array_2d = to_complex(d, p_vector_array_2d);
  transformed_param_vector_array_3d = to_complex(p, p_vector_array_3d);
  transformed_param_vector_array_3d = to_complex(p, d_vector_array_3d);
  transformed_param_vector_array_3d = to_complex(d, p_vector_array_3d);

  transformed_param_vector = to_complex(p_vector, p_vector);
  transformed_param_vector = to_complex(p_vector, d_vector);
  transformed_param_vector = to_complex(d_vector, p_vector);
  transformed_param_vector_array = to_complex(p_vector_array, p_vector_array);
  transformed_param_vector_array = to_complex(p_vector_array, d_vector_array);
  transformed_param_vector_array = to_complex(d_vector_array, p_vector_array);
  transformed_param_vector_array_2d = to_complex(p_vector_array_2d,
                                              p_vector_array_2d);
  transformed_param_vector_array_2d = to_complex(p_vector_array_2d,
                                              d_vector_array_2d);
  transformed_param_vector_array_2d = to_complex(d_vector_array_2d,
                                              p_vector_array_2d);
  transformed_param_vector_array_3d = to_complex(p_vector_array_3d,
                                              p_vector_array_3d);
  transformed_param_vector_array_3d = to_complex(p_vector_array_3d,
                                              d_vector_array_3d);
  transformed_param_vector_array_3d = to_complex(d_vector_array_3d,
                                              p_vector_array_3d);

  transformed_param_row_vector = to_complex(p_row_vector, p);
  transformed_param_row_vector = to_complex(p_row_vector, d);
  transformed_param_row_vector = to_complex(d_row_vector, p);
  transformed_param_row_vector_array = to_complex(p_row_vector_array, p);
  transformed_param_row_vector_array = to_complex(p_row_vector_array, d);
  transformed_param_row_vector_array = to_complex(d_row_vector_array, p);
  transformed_param_row_vector_array_2d = to_complex(p_row_vector_array_2d,
                                                  p);
  transformed_param_row_vector_array_2d = to_complex(p_row_vector_array_2d,
                                                  d);
  transformed_param_row_vector_array_2d = to_complex(d_row_vector_array_2d,
                                                  p);
  transformed_param_row_vector_array_3d = to_complex(p_row_vector_array_3d,
                                                  p);
  transformed_param_row_vector_array_3d = to_complex(p_row_vector_array_3d,
                                                  d);
  transformed_param_row_vector_array_3d = to_complex(d_row_vector_array_3d,
                                                  p);

  transformed_param_row_vector = to_complex(p, p_row_vector);
  transformed_param_row_vector = to_complex(d, p_row_vector);
  transformed_param_row_vector = to_complex(p, d_row_vector);
  transformed_param_row_vector_array = to_complex(p, p_row_vector_array);
  transformed_param_row_vector_array = to_complex(d, p_row_vector_array);
  transformed_param_row_vector_array = to_complex(p, d_row_vector_array);
  transformed_param_row_vector_array_2d = to_complex(p,
                                                  p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = to_complex(d,
                                                  p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = to_complex(p,
                                                  d_row_vector_array_2d);
  transformed_param_row_vector_array_3d = to_complex(p,
                                                  p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = to_complex(d,
                                                  p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = to_complex(p,
                                                  d_row_vector_array_3d);

  transformed_param_row_vector = to_complex(p_row_vector, p_row_vector);
  transformed_param_row_vector = to_complex(p_row_vector, d_row_vector);
  transformed_param_row_vector = to_complex(d_row_vector, p_row_vector);
  transformed_param_row_vector_array = to_complex(p_row_vector_array,
                                               p_row_vector_array);
  transformed_param_row_vector_array = to_complex(p_row_vector_array,
                                               d_row_vector_array);
  transformed_param_row_vector_array = to_complex(d_row_vector_array,
                                               p_row_vector_array);
  transformed_param_row_vector_array_2d = to_complex(p_row_vector_array_2d,
                                                  p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = to_complex(p_row_vector_array_2d,
                                                  d_row_vector_array_2d);
  transformed_param_row_vector_array_2d = to_complex(d_row_vector_array_2d,
                                                  p_row_vector_array_2d);
  transformed_param_row_vector_array_3d = to_complex(p_row_vector_array_3d,
                                                  p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = to_complex(p_row_vector_array_3d,
                                                  d_row_vector_array_3d);
  transformed_param_row_vector_array_3d = to_complex(d_row_vector_array_3d,
                                                  p_row_vector_array_3d);

  transformed_param_matrix = to_complex(p_matrix, p);
  transformed_param_matrix = to_complex(p_matrix, d);
  transformed_param_matrix = to_complex(d_matrix, p);
  transformed_param_matrix_array = to_complex(p_matrix_array, p);
  transformed_param_matrix_array = to_complex(p_matrix_array, d);
  transformed_param_matrix_array = to_complex(d_matrix_array, p);
  transformed_param_matrix_array_2d = to_complex(p_matrix_array_2d, p);
  transformed_param_matrix_array_2d = to_complex(p_matrix_array_2d, d);
  transformed_param_matrix_array_2d = to_complex(d_matrix_array_2d, p);
  transformed_param_matrix_array_3d = to_complex(p_matrix_array_3d, p);
  transformed_param_matrix_array_3d = to_complex(p_matrix_array_3d, d);
  transformed_param_matrix_array_3d = to_complex(d_matrix_array_3d, p);

  transformed_param_matrix = to_complex(p, p_matrix);
  transformed_param_matrix = to_complex(p, d_matrix);
  transformed_param_matrix = to_complex(d, p_matrix);
  transformed_param_matrix_array = to_complex(p, p_matrix_array);
  transformed_param_matrix_array = to_complex(p, d_matrix_array);
  transformed_param_matrix_array = to_complex(d, p_matrix_array);
  transformed_param_matrix_array_2d = to_complex(p, p_matrix_array_2d);
  transformed_param_matrix_array_2d = to_complex(p, d_matrix_array_2d);
  transformed_param_matrix_array_2d = to_complex(d, p_matrix_array_2d);
  transformed_param_matrix_array_3d = to_complex(p, p_matrix_array_3d);
  transformed_param_matrix_array_3d = to_complex(p, d_matrix_array_3d);
  transformed_param_matrix_array_3d = to_complex(d, p_matrix_array_3d);

  transformed_param_matrix = to_complex(p_matrix, p_matrix);
  transformed_param_matrix = to_complex(p_matrix, d_matrix);
  transformed_param_matrix = to_complex(d_matrix, p_matrix);
  transformed_param_matrix_array = to_complex(p_matrix_array, p_matrix_array);
  transformed_param_matrix_array = to_complex(p_matrix_array, d_matrix_array);
  transformed_param_matrix_array = to_complex(d_matrix_array, p_matrix_array);
  transformed_param_matrix_array_2d = to_complex(p_matrix_array_2d,
                                              p_matrix_array_2d);
  transformed_param_matrix_array_2d = to_complex(p_matrix_array_2d,
                                              d_matrix_array_2d);
  transformed_param_matrix_array_2d = to_complex(d_matrix_array_2d,
                                              p_matrix_array_2d);
  transformed_param_matrix_array_3d = to_complex(p_matrix_array_3d,
                                              p_matrix_array_3d);
  transformed_param_matrix_array_3d = to_complex(p_matrix_array_3d,
                                              d_matrix_array_3d);
  transformed_param_matrix_array_3d = to_complex(d_matrix_array_3d,
                                              p_matrix_array_3d);
}
model {
  y_p ~ normal(0, 1);
}

