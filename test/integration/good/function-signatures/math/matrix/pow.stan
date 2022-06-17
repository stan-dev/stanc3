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

  // complex types
  complex d_complex;
  array[d_int] complex d_complex_array;
  array[d_int, 2] complex d_complex_array_2d;
  array[d_int, 2, 3] complex d_complex_array_3d;
  complex_matrix[d_int, d_int] d_cmatrix;
  array[d_int] complex_matrix[d_int, d_int] d_cmatrix_array;
  array[d_int, 2] complex_matrix[d_int, d_int] d_cmatrix_array_2d;
  array[d_int, 2, 3] complex_matrix[d_int, d_int] d_cmatrix_array_3d;
  complex_vector[d_int] d_cvector;
  array[d_int] complex_vector[d_int] d_cvector_array;
  array[d_int, 2] complex_vector[d_int] d_cvector_array_2d;
  array[d_int, 2, 3] complex_vector[d_int] d_cvector_array_3d;
  complex_row_vector[d_int] d_crow_vector;
  array[d_int] complex_row_vector[d_int] d_crow_vector_array;
  array[d_int, 2] complex_row_vector[d_int] d_crow_vector_array_2d;
  array[d_int, 2, 3] complex_row_vector[d_int] d_crow_vector_array_3d;

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

  transformed_data_real_array = pow(d_int_array, d_int);
  transformed_data_real_array = pow(d_int_array, d_real);
  transformed_data_real_array_2d = pow(d_int_array_2d, d_int);
  transformed_data_real_array_2d = pow(d_int_array_2d, d_real);
  transformed_data_real_array_3d = pow(d_int_array_3d, d_int);
  transformed_data_real_array_3d = pow(d_int_array_3d, d_real);

  transformed_data_real_array = pow(d_int, d_int_array);
  transformed_data_real_array = pow(d_real, d_int_array);
  transformed_data_real_array_2d = pow(d_int, d_int_array_2d);
  transformed_data_real_array_2d = pow(d_real, d_int_array_2d);
  transformed_data_real_array_3d = pow(d_int, d_int_array_3d);
  transformed_data_real_array_3d = pow(d_real, d_int_array_3d);

  transformed_data_real_array = pow(d_int, d_real_array);
  transformed_data_real_array = pow(d_real, d_real_array);
  transformed_data_real_array_2d = pow(d_int, d_real_array_2d);
  transformed_data_real_array_2d = pow(d_real, d_real_array_2d);
  transformed_data_real_array_3d = pow(d_int, d_real_array_3d);
  transformed_data_real_array_3d = pow(d_real, d_real_array_3d);

  transformed_data_real_array = pow(d_real_array, d_int);
  transformed_data_real_array = pow(d_real_array, d_real);
  transformed_data_real_array_2d = pow(d_real_array_2d, d_int);
  transformed_data_real_array_2d = pow(d_real_array_2d, d_real);
  transformed_data_real_array_3d = pow(d_real_array_3d, d_int);
  transformed_data_real_array_3d = pow(d_real_array_3d, d_real);

  transformed_data_real_array = pow(d_int_array, d_int_array);
  transformed_data_real_array = pow(d_real_array, d_real_array);
  transformed_data_real_array_2d = pow(d_int_array_2d, d_int_array_2d);
  transformed_data_real_array_2d = pow(d_real_array_2d, d_real_array_2d);
  transformed_data_real_array_3d = pow(d_int_array_3d, d_int_array_3d);
  transformed_data_real_array_3d = pow(d_real_array_3d, d_real_array_3d);

  transformed_data_vector = pow(d_vector, d_int);
  transformed_data_vector = pow(d_vector, d_real);
  transformed_data_vector_array = pow(d_vector_array, d_int);
  transformed_data_vector_array = pow(d_vector_array, d_real);
  transformed_data_vector_array_2d = pow(d_vector_array_2d, d_int);
  transformed_data_vector_array_2d = pow(d_vector_array_2d, d_real);
  transformed_data_vector_array_3d = pow(d_vector_array_3d, d_int);
  transformed_data_vector_array_3d = pow(d_vector_array_3d, d_real);

  transformed_data_vector = pow(d_int, d_vector);
  transformed_data_vector = pow(d_real, d_vector);
  transformed_data_vector_array = pow(d_int, d_vector_array);
  transformed_data_vector_array = pow(d_real, d_vector_array);
  transformed_data_vector_array_2d = pow(d_int, d_vector_array_2d);
  transformed_data_vector_array_2d = pow(d_real, d_vector_array_2d);
  transformed_data_vector_array_3d = pow(d_int, d_vector_array_3d);
  transformed_data_vector_array_3d = pow(d_real, d_vector_array_3d);

  transformed_data_vector = pow(d_vector, d_vector);
  transformed_data_vector_array = pow(d_vector_array, d_vector_array);
  transformed_data_vector_array_2d = pow(d_vector_array_2d,
                                          d_vector_array_2d);
  transformed_data_vector_array_3d = pow(d_vector_array_3d,
                                          d_vector_array_3d);

  transformed_data_row_vector = pow(d_row_vector, d_int);
  transformed_data_row_vector = pow(d_row_vector, d_real);
  transformed_data_row_vector_array = pow(d_row_vector_array, d_int);
  transformed_data_row_vector_array = pow(d_row_vector_array, d_real);
  transformed_data_row_vector_array_2d = pow(d_row_vector_array_2d, d_int);
  transformed_data_row_vector_array_2d = pow(d_row_vector_array_2d, d_real);
  transformed_data_row_vector_array_3d = pow(d_row_vector_array_3d, d_int);
  transformed_data_row_vector_array_3d = pow(d_row_vector_array_3d, d_real);

  transformed_data_row_vector = pow(d_int, d_row_vector);
  transformed_data_row_vector = pow(d_real, d_row_vector);
  transformed_data_row_vector_array = pow(d_int, d_row_vector_array);
  transformed_data_row_vector_array = pow(d_real, d_row_vector_array);
  transformed_data_row_vector_array_2d = pow(d_int, d_row_vector_array_2d);
  transformed_data_row_vector_array_2d = pow(d_real, d_row_vector_array_2d);
  transformed_data_row_vector_array_3d = pow(d_int, d_row_vector_array_3d);
  transformed_data_row_vector_array_3d = pow(d_real, d_row_vector_array_3d);

  transformed_data_row_vector = pow(d_row_vector, d_row_vector);
  transformed_data_row_vector_array = pow(d_row_vector_array,
                                           d_row_vector_array);
  transformed_data_row_vector_array_2d = pow(d_row_vector_array_2d,
                                              d_row_vector_array_2d);
  transformed_data_row_vector_array_3d = pow(d_row_vector_array_3d,
                                              d_row_vector_array_3d);

  transformed_data_matrix = pow(d_matrix, d_int);
  transformed_data_matrix = pow(d_matrix, d_real);
  transformed_data_matrix_array = pow(d_matrix_array, d_int);
  transformed_data_matrix_array = pow(d_matrix_array, d_real);
  transformed_data_matrix_array_2d = pow(d_matrix_array_2d, d_int);
  transformed_data_matrix_array_2d = pow(d_matrix_array_2d, d_real);
  transformed_data_matrix_array_3d = pow(d_matrix_array_3d, d_int);
  transformed_data_matrix_array_3d = pow(d_matrix_array_3d, d_real);

  transformed_data_matrix = pow(d_int, d_matrix);
  transformed_data_matrix = pow(d_real, d_matrix);
  transformed_data_matrix_array = pow(d_int, d_matrix_array);
  transformed_data_matrix_array = pow(d_real, d_matrix_array);
  transformed_data_matrix_array_2d = pow(d_int, d_matrix_array_2d);
  transformed_data_matrix_array_2d = pow(d_real, d_matrix_array_2d);
  transformed_data_matrix_array_3d = pow(d_int, d_matrix_array_3d);
  transformed_data_matrix_array_3d = pow(d_real, d_matrix_array_3d);

  transformed_data_matrix = pow(d_matrix, d_matrix);
  transformed_data_matrix_array = pow(d_matrix_array, d_matrix_array);
  transformed_data_matrix_array_2d = pow(d_matrix_array_2d,
                                          d_matrix_array_2d);
  transformed_data_matrix_array_3d = pow(d_matrix_array_3d,
                                          d_matrix_array_3d);

  // complex types
  array[d_int] complex transformed_data_carray;
  array[d_int, 2] complex transformed_data_carray_2d;
  array[d_int, 2, 3] complex transformed_data_carray_3d;
  complex_matrix[d_int, d_int] transformed_data_cmatrix;
  array[d_int] complex_matrix[d_int, d_int] transformed_data_cmatrix_array;
  array[d_int, 2] complex_matrix[d_int, d_int] transformed_data_cmatrix_array_2d;
  array[d_int, 2, 3] complex_matrix[d_int, d_int] transformed_data_cmatrix_array_3d;
  complex_vector[d_int] transformed_data_cvector;
  array[d_int] complex_vector[d_int] transformed_data_cvector_array;
  array[d_int, 2] complex_vector[d_int] transformed_data_cvector_array_2d;
  array[d_int, 2, 3] complex_vector[d_int] transformed_data_cvector_array_3d;
  complex_row_vector[d_int] transformed_data_crow_vector;
  array[d_int] complex_row_vector[d_int] transformed_data_crow_vector_array;
  array[d_int, 2] complex_row_vector[d_int] transformed_data_crow_vector_array_2d;
  array[d_int, 2, 3] complex_row_vector[d_int] transformed_data_crow_vector_array_3d;


  transformed_data_carray = pow(d_int, d_complex_array);
  transformed_data_carray = pow(d_complex, d_complex_array);
  transformed_data_carray_2d = pow(d_int, d_complex_array_2d);
  transformed_data_carray_2d = pow(d_complex, d_complex_array_2d);
  transformed_data_carray_3d = pow(d_int, d_complex_array_3d);
  transformed_data_carray_3d = pow(d_complex, d_complex_array_3d);

  transformed_data_carray = pow(d_complex_array, d_int);
  transformed_data_carray = pow(d_complex_array, d_complex);
  transformed_data_carray_2d = pow(d_complex_array_2d, d_int);
  transformed_data_carray_2d = pow(d_complex_array_2d, d_complex);
  transformed_data_carray_3d = pow(d_complex_array_3d, d_int);
  transformed_data_carray_3d = pow(d_complex_array_3d, d_complex);


  transformed_data_carray = pow(d_complex_array, d_complex_array);

  transformed_data_carray_2d = pow(d_complex_array_2d, d_complex_array_2d);

  transformed_data_carray_3d = pow(d_complex_array_3d, d_complex_array_3d);

  transformed_data_cvector = pow(d_cvector, d_int);
  transformed_data_cvector = pow(d_cvector, d_complex);
  transformed_data_cvector_array = pow(d_cvector_array, d_int);
  transformed_data_cvector_array = pow(d_cvector_array, d_complex);
  transformed_data_cvector_array_2d = pow(d_cvector_array_2d, d_int);
  transformed_data_cvector_array_2d = pow(d_cvector_array_2d, d_complex);
  transformed_data_cvector_array_3d = pow(d_cvector_array_3d, d_int);
  transformed_data_cvector_array_3d = pow(d_cvector_array_3d, d_complex);

  transformed_data_cvector = pow(d_int, d_cvector);
  transformed_data_cvector = pow(d_complex, d_cvector);
  transformed_data_cvector_array = pow(d_int, d_cvector_array);
  transformed_data_cvector_array = pow(d_complex, d_cvector_array);
  transformed_data_cvector_array_2d = pow(d_int, d_cvector_array_2d);
  transformed_data_cvector_array_2d = pow(d_complex, d_cvector_array_2d);
  transformed_data_cvector_array_3d = pow(d_int, d_cvector_array_3d);
  transformed_data_cvector_array_3d = pow(d_complex, d_cvector_array_3d);

  transformed_data_cvector = pow(d_cvector, d_cvector);
  transformed_data_cvector_array = pow(d_cvector_array, d_cvector_array);
  transformed_data_cvector_array_2d = pow(d_cvector_array_2d,
                                          d_cvector_array_2d);
  transformed_data_cvector_array_3d = pow(d_cvector_array_3d,
                                          d_cvector_array_3d);

  transformed_data_crow_vector = pow(d_crow_vector, d_int);
  transformed_data_crow_vector = pow(d_crow_vector, d_complex);
  transformed_data_crow_vector_array = pow(d_crow_vector_array, d_int);
  transformed_data_crow_vector_array = pow(d_crow_vector_array, d_complex);
  transformed_data_crow_vector_array_2d = pow(d_crow_vector_array_2d, d_int);
  transformed_data_crow_vector_array_2d = pow(d_crow_vector_array_2d, d_complex);
  transformed_data_crow_vector_array_3d = pow(d_crow_vector_array_3d, d_int);
  transformed_data_crow_vector_array_3d = pow(d_crow_vector_array_3d, d_complex);

  transformed_data_crow_vector = pow(d_int, d_crow_vector);
  transformed_data_crow_vector = pow(d_complex, d_crow_vector);
  transformed_data_crow_vector_array = pow(d_int, d_crow_vector_array);
  transformed_data_crow_vector_array = pow(d_complex, d_crow_vector_array);
  transformed_data_crow_vector_array_2d = pow(d_int, d_crow_vector_array_2d);
  transformed_data_crow_vector_array_2d = pow(d_complex, d_crow_vector_array_2d);
  transformed_data_crow_vector_array_3d = pow(d_int, d_crow_vector_array_3d);
  transformed_data_crow_vector_array_3d = pow(d_complex, d_crow_vector_array_3d);

  transformed_data_crow_vector = pow(d_crow_vector, d_crow_vector);
  transformed_data_crow_vector_array = pow(d_crow_vector_array,
                                           d_crow_vector_array);
  transformed_data_crow_vector_array_2d = pow(d_crow_vector_array_2d,
                                              d_crow_vector_array_2d);
  transformed_data_crow_vector_array_3d = pow(d_crow_vector_array_3d,
                                              d_crow_vector_array_3d);

  transformed_data_cmatrix = pow(d_cmatrix, d_int);
  transformed_data_cmatrix = pow(d_cmatrix, d_complex);
  transformed_data_cmatrix_array = pow(d_cmatrix_array, d_int);
  transformed_data_cmatrix_array = pow(d_cmatrix_array, d_complex);
  transformed_data_cmatrix_array_2d = pow(d_cmatrix_array_2d, d_int);
  transformed_data_cmatrix_array_2d = pow(d_cmatrix_array_2d, d_complex);
  transformed_data_cmatrix_array_3d = pow(d_cmatrix_array_3d, d_int);
  transformed_data_cmatrix_array_3d = pow(d_cmatrix_array_3d, d_complex);

  transformed_data_cmatrix = pow(d_int, d_cmatrix);
  transformed_data_cmatrix = pow(d_complex, d_cmatrix);
  transformed_data_cmatrix_array = pow(d_int, d_cmatrix_array);
  transformed_data_cmatrix_array = pow(d_complex, d_cmatrix_array);
  transformed_data_cmatrix_array_2d = pow(d_int, d_cmatrix_array_2d);
  transformed_data_cmatrix_array_2d = pow(d_complex, d_cmatrix_array_2d);
  transformed_data_cmatrix_array_3d = pow(d_int, d_cmatrix_array_3d);
  transformed_data_cmatrix_array_3d = pow(d_complex, d_cmatrix_array_3d);

  transformed_data_cmatrix = pow(d_cmatrix, d_cmatrix);
  transformed_data_cmatrix_array = pow(d_cmatrix_array, d_cmatrix_array);
  transformed_data_cmatrix_array_2d = pow(d_cmatrix_array_2d,
                                          d_cmatrix_array_2d);
  transformed_data_cmatrix_array_3d = pow(d_cmatrix_array_3d,
                                          d_cmatrix_array_3d);
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

  // complex types
  complex p_complex;
  array[d_int] complex p_complex_array;
  array[d_int, 2] complex p_complex_array_2d;
  array[d_int, 2, 3] complex p_complex_array_3d;
  complex_matrix[d_int, d_int] p_cmatrix;
  array[d_int] complex_matrix[d_int, d_int] p_cmatrix_array;
  array[d_int, 2] complex_matrix[d_int, d_int] p_cmatrix_array_2d;
  array[d_int, 2, 3] complex_matrix[d_int, d_int] p_cmatrix_array_3d;
  complex_vector[d_int] p_cvector;
  array[d_int] complex_vector[d_int] p_cvector_array;
  array[d_int, 2] complex_vector[d_int] p_cvector_array_2d;
  array[d_int, 2, 3] complex_vector[d_int] p_cvector_array_3d;
  complex_row_vector[d_int] p_crow_vector;
  array[d_int] complex_row_vector[d_int] p_crow_vector_array;
  array[d_int, 2] complex_row_vector[d_int] p_crow_vector_array_2d;
  array[d_int, 2, 3] complex_row_vector[d_int] p_crow_vector_array_3d;

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

  transformed_param_array = pow(d_int_array, p_real);
  transformed_param_array_2d = pow(d_int_array_2d, p_real);
  transformed_param_array_3d = pow(d_int_array_3d, p_real);

  transformed_param_array = pow(p_real, d_int_array);
  transformed_param_array_2d = pow(p_real, d_int_array_2d);
  transformed_param_array_3d = pow(p_real, d_int_array_3d);

  transformed_param_array = pow(p_real_array, d_int);
  transformed_param_array = pow(p_real_array, d_real);
  transformed_param_array = pow(p_real_array, p_real);
  transformed_param_array = pow(d_real_array, p_real);
  transformed_param_array_2d = pow(p_real_array_2d, d_int);
  transformed_param_array_2d = pow(p_real_array_2d, d_real);
  transformed_param_array_2d = pow(p_real_array_2d, p_real);
  transformed_param_array_2d = pow(d_real_array_2d, p_real);
  transformed_param_array_3d = pow(p_real_array_3d, d_int);
  transformed_param_array_3d = pow(p_real_array_3d, d_real);
  transformed_param_array_3d = pow(p_real_array_3d, p_real);
  transformed_param_array_3d = pow(d_real_array_3d, p_real);

  transformed_param_array = pow(d_int, p_real_array);
  transformed_param_array = pow(d_real, p_real_array);
  transformed_param_array = pow(p_real, p_real_array);
  transformed_param_array = pow(p_real, d_real_array);
  transformed_param_array_2d = pow(d_int, p_real_array_2d);
  transformed_param_array_2d = pow(d_real, p_real_array_2d);
  transformed_param_array_2d = pow(p_real, p_real_array_2d);
  transformed_param_array_2d = pow(p_real, d_real_array_2d);
  transformed_param_array_3d = pow(d_int, p_real_array_3d);
  transformed_param_array_3d = pow(d_real, p_real_array_3d);
  transformed_param_array_3d = pow(p_real, p_real_array_3d);
  transformed_param_array_3d = pow(p_real, d_real_array_3d);

  transformed_param_array = pow(d_real_array, p_real_array);
  transformed_param_array = pow(p_real_array, d_real_array);
  transformed_param_array = pow(p_real_array, p_real_array);
  transformed_param_array_2d = pow(d_real_array_2d, p_real_array_2d);
  transformed_param_array_2d = pow(p_real_array_2d, d_real_array_2d);
  transformed_param_array_2d = pow(p_real_array_2d, p_real_array_2d);
  transformed_param_array_3d = pow(d_real_array_3d, p_real_array_3d);
  transformed_param_array_3d = pow(p_real_array_3d, d_real_array_3d);
  transformed_param_array_3d = pow(p_real_array_3d, p_real_array_3d);

  transformed_param_vector = pow(p_vector, p_real);
  transformed_param_vector = pow(p_vector, d_real);
  transformed_param_vector = pow(p_vector, d_int);
  transformed_param_vector = pow(d_vector, p_real);
  transformed_param_vector_array = pow(p_vector_array, p_real);
  transformed_param_vector_array = pow(p_vector_array, d_real);
  transformed_param_vector_array = pow(p_vector_array, d_int);
  transformed_param_vector_array = pow(d_vector_array, p_real);
  transformed_param_vector_array_2d = pow(p_vector_array_2d, p_real);
  transformed_param_vector_array_2d = pow(p_vector_array_2d, d_real);
  transformed_param_vector_array_2d = pow(p_vector_array_2d, d_int);
  transformed_param_vector_array_2d = pow(d_vector_array_2d, p_real);
  transformed_param_vector_array_3d = pow(p_vector_array_3d, p_real);
  transformed_param_vector_array_3d = pow(p_vector_array_3d, d_real);
  transformed_param_vector_array_3d = pow(p_vector_array_3d, d_int);
  transformed_param_vector_array_3d = pow(d_vector_array_3d, p_real);

  transformed_param_vector = pow(p_real, p_vector);
  transformed_param_vector = pow(p_real, d_vector);
  transformed_param_vector = pow(d_real, p_vector);
  transformed_param_vector = pow(d_int, p_vector);
  transformed_param_vector_array = pow(p_real, p_vector_array);
  transformed_param_vector_array = pow(p_real, d_vector_array);
  transformed_param_vector_array = pow(d_real, p_vector_array);
  transformed_param_vector_array = pow(d_int, p_vector_array);
  transformed_param_vector_array_2d = pow(p_real, p_vector_array_2d);
  transformed_param_vector_array_2d = pow(p_real, d_vector_array_2d);
  transformed_param_vector_array_2d = pow(d_real, p_vector_array_2d);
  transformed_param_vector_array_2d = pow(d_int, p_vector_array_2d);
  transformed_param_vector_array_3d = pow(p_real, p_vector_array_3d);
  transformed_param_vector_array_3d = pow(p_real, d_vector_array_3d);
  transformed_param_vector_array_3d = pow(d_real, p_vector_array_3d);
  transformed_param_vector_array_3d = pow(d_int, p_vector_array_3d);

  transformed_param_vector = pow(p_vector, p_vector);
  transformed_param_vector = pow(p_vector, d_vector);
  transformed_param_vector = pow(d_vector, p_vector);
  transformed_param_vector_array = pow(p_vector_array, p_vector_array);
  transformed_param_vector_array = pow(p_vector_array, d_vector_array);
  transformed_param_vector_array = pow(d_vector_array, p_vector_array);
  transformed_param_vector_array_2d = pow(p_vector_array_2d,
                                           p_vector_array_2d);
  transformed_param_vector_array_2d = pow(p_vector_array_2d,
                                           d_vector_array_2d);
  transformed_param_vector_array_2d = pow(d_vector_array_2d,
                                           p_vector_array_2d);
  transformed_param_vector_array_3d = pow(p_vector_array_3d,
                                           p_vector_array_3d);
  transformed_param_vector_array_3d = pow(p_vector_array_3d,
                                           d_vector_array_3d);
  transformed_param_vector_array_3d = pow(d_vector_array_3d,
                                           p_vector_array_3d);

  transformed_param_row_vector = pow(p_row_vector, p_real);
  transformed_param_row_vector = pow(p_row_vector, d_real);
  transformed_param_row_vector = pow(p_row_vector, d_int);
  transformed_param_row_vector = pow(d_row_vector, p_real);
  transformed_param_row_vector_array = pow(p_row_vector_array, p_real);
  transformed_param_row_vector_array = pow(p_row_vector_array, d_real);
  transformed_param_row_vector_array = pow(p_row_vector_array, d_int);
  transformed_param_row_vector_array = pow(d_row_vector_array, p_real);
  transformed_param_row_vector_array_2d = pow(p_row_vector_array_2d, p_real);
  transformed_param_row_vector_array_2d = pow(p_row_vector_array_2d, d_real);
  transformed_param_row_vector_array_2d = pow(p_row_vector_array_2d, d_int);
  transformed_param_row_vector_array_2d = pow(d_row_vector_array_2d, p_real);
  transformed_param_row_vector_array_3d = pow(p_row_vector_array_3d, p_real);
  transformed_param_row_vector_array_3d = pow(p_row_vector_array_3d, d_real);
  transformed_param_row_vector_array_3d = pow(p_row_vector_array_3d, d_int);
  transformed_param_row_vector_array_3d = pow(d_row_vector_array_3d, p_real);

  transformed_param_row_vector = pow(p_real, p_row_vector);
  transformed_param_row_vector = pow(d_real, p_row_vector);
  transformed_param_row_vector = pow(d_int, p_row_vector);
  transformed_param_row_vector = pow(p_real, d_row_vector);
  transformed_param_row_vector_array = pow(p_real, p_row_vector_array);
  transformed_param_row_vector_array = pow(d_real, p_row_vector_array);
  transformed_param_row_vector_array = pow(d_int, p_row_vector_array);
  transformed_param_row_vector_array = pow(p_real, d_row_vector_array);
  transformed_param_row_vector_array_2d = pow(p_real, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = pow(d_real, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = pow(d_int, p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = pow(p_real, d_row_vector_array_2d);
  transformed_param_row_vector_array_3d = pow(p_real, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = pow(d_real, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = pow(d_int, p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = pow(p_real, d_row_vector_array_3d);

  transformed_param_row_vector = pow(p_row_vector, p_row_vector);
  transformed_param_row_vector = pow(p_row_vector, d_row_vector);
  transformed_param_row_vector = pow(d_row_vector, p_row_vector);
  transformed_param_row_vector_array = pow(p_row_vector_array,
                                            p_row_vector_array);
  transformed_param_row_vector_array = pow(p_row_vector_array,
                                            d_row_vector_array);
  transformed_param_row_vector_array = pow(d_row_vector_array,
                                            p_row_vector_array);
  transformed_param_row_vector_array_2d = pow(p_row_vector_array_2d,
                                               p_row_vector_array_2d);
  transformed_param_row_vector_array_2d = pow(p_row_vector_array_2d,
                                               d_row_vector_array_2d);
  transformed_param_row_vector_array_2d = pow(d_row_vector_array_2d,
                                               p_row_vector_array_2d);
  transformed_param_row_vector_array_3d = pow(p_row_vector_array_3d,
                                               p_row_vector_array_3d);
  transformed_param_row_vector_array_3d = pow(p_row_vector_array_3d,
                                               d_row_vector_array_3d);
  transformed_param_row_vector_array_3d = pow(d_row_vector_array_3d,
                                               p_row_vector_array_3d);

  transformed_param_matrix = pow(p_matrix, p_real);
  transformed_param_matrix = pow(p_matrix, d_real);
  transformed_param_matrix = pow(p_matrix, d_int);
  transformed_param_matrix = pow(d_matrix, p_real);
  transformed_param_matrix_array = pow(p_matrix_array, p_real);
  transformed_param_matrix_array = pow(p_matrix_array, d_real);
  transformed_param_matrix_array = pow(p_matrix_array, d_int);
  transformed_param_matrix_array = pow(d_matrix_array, p_real);
  transformed_param_matrix_array_2d = pow(p_matrix_array_2d, p_real);
  transformed_param_matrix_array_2d = pow(p_matrix_array_2d, d_real);
  transformed_param_matrix_array_2d = pow(p_matrix_array_2d, d_int);
  transformed_param_matrix_array_2d = pow(d_matrix_array_2d, p_real);
  transformed_param_matrix_array_3d = pow(p_matrix_array_3d, p_real);
  transformed_param_matrix_array_3d = pow(p_matrix_array_3d, d_real);
  transformed_param_matrix_array_3d = pow(p_matrix_array_3d, d_int);
  transformed_param_matrix_array_3d = pow(d_matrix_array_3d, p_real);

  transformed_param_matrix = pow(p_real, p_matrix);
  transformed_param_matrix = pow(p_real, d_matrix);
  transformed_param_matrix = pow(d_real, p_matrix);
  transformed_param_matrix = pow(d_int, p_matrix);
  transformed_param_matrix_array = pow(p_real, p_matrix_array);
  transformed_param_matrix_array = pow(p_real, d_matrix_array);
  transformed_param_matrix_array = pow(d_real, p_matrix_array);
  transformed_param_matrix_array = pow(d_int, p_matrix_array);
  transformed_param_matrix_array_2d = pow(p_real, p_matrix_array_2d);
  transformed_param_matrix_array_2d = pow(p_real, d_matrix_array_2d);
  transformed_param_matrix_array_2d = pow(d_real, p_matrix_array_2d);
  transformed_param_matrix_array_2d = pow(d_int, p_matrix_array_2d);
  transformed_param_matrix_array_3d = pow(p_real, p_matrix_array_3d);
  transformed_param_matrix_array_3d = pow(p_real, d_matrix_array_3d);
  transformed_param_matrix_array_3d = pow(d_real, p_matrix_array_3d);
  transformed_param_matrix_array_3d = pow(d_int, p_matrix_array_3d);

  transformed_param_matrix = pow(p_matrix, p_matrix);
  transformed_param_matrix = pow(p_matrix, d_matrix);
  transformed_param_matrix = pow(d_matrix, p_matrix);
  transformed_param_matrix_array = pow(p_matrix_array, p_matrix_array);
  transformed_param_matrix_array = pow(p_matrix_array, d_matrix_array);
  transformed_param_matrix_array = pow(d_matrix_array, p_matrix_array);
  transformed_param_matrix_array_2d = pow(p_matrix_array_2d,
                                           p_matrix_array_2d);
  transformed_param_matrix_array_2d = pow(p_matrix_array_2d,
                                           d_matrix_array_2d);
  transformed_param_matrix_array_2d = pow(d_matrix_array_2d,
                                           p_matrix_array_2d);
  transformed_param_matrix_array_3d = pow(p_matrix_array_3d,
                                           p_matrix_array_3d);
  transformed_param_matrix_array_3d = pow(p_matrix_array_3d,
                                           d_matrix_array_3d);
  transformed_param_matrix_array_3d = pow(d_matrix_array_3d,
                                           p_matrix_array_3d);

  // complex types
  array[d_int] complex transformed_param_carray;
  array[d_int, 2] complex transformed_param_carray_2d;
  array[d_int, 2, 3] complex transformed_param_carray_3d;
  complex_matrix[d_int, d_int] transformed_param_cmatrix;
  array[d_int] complex_matrix[d_int, d_int] transformed_param_cmatrix_array;
  array[d_int, 2] complex_matrix[d_int, d_int] transformed_param_cmatrix_array_2d;
  array[d_int, 2, 3] complex_matrix[d_int, d_int] transformed_param_cmatrix_array_3d;
  complex_vector[d_int] transformed_param_cvector;
  array[d_int] complex_vector[d_int] transformed_param_cvector_array;
  array[d_int, 2] complex_vector[d_int] transformed_param_cvector_array_2d;
  array[d_int, 2, 3] complex_vector[d_int] transformed_param_cvector_array_3d;
  complex_row_vector[d_int] transformed_param_crow_vector;
  array[d_int] complex_row_vector[d_int] transformed_param_crow_vector_array;
  array[d_int, 2] complex_row_vector[d_int] transformed_param_crow_vector_array_2d;
  array[d_int, 2, 3] complex_row_vector[d_int] transformed_param_crow_vector_array_3d;

  transformed_param_carray = pow(p_complex_array, d_int);
  transformed_param_carray = pow(p_complex_array, d_complex);
  transformed_param_carray = pow(p_complex_array, p_complex);
  transformed_param_carray = pow(d_complex_array, p_complex);
  transformed_param_carray_2d = pow(p_complex_array_2d, d_int);
  transformed_param_carray_2d = pow(p_complex_array_2d, d_complex);
  transformed_param_carray_2d = pow(p_complex_array_2d, p_complex);
  transformed_param_carray_2d = pow(d_complex_array_2d, p_complex);
  transformed_param_carray_3d = pow(p_complex_array_3d, d_int);
  transformed_param_carray_3d = pow(p_complex_array_3d, d_complex);
  transformed_param_carray_3d = pow(p_complex_array_3d, p_complex);
  transformed_param_carray_3d = pow(d_complex_array_3d, p_complex);

  transformed_param_carray = pow(d_int, p_complex_array);
  transformed_param_carray = pow(d_complex, p_complex_array);
  transformed_param_carray = pow(p_complex, p_complex_array);
  transformed_param_carray = pow(p_complex, d_complex_array);
  transformed_param_carray_2d = pow(d_int, p_complex_array_2d);
  transformed_param_carray_2d = pow(d_complex, p_complex_array_2d);
  transformed_param_carray_2d = pow(p_complex, p_complex_array_2d);
  transformed_param_carray_2d = pow(p_complex, d_complex_array_2d);
  transformed_param_carray_3d = pow(d_int, p_complex_array_3d);
  transformed_param_carray_3d = pow(d_complex, p_complex_array_3d);
  transformed_param_carray_3d = pow(p_complex, p_complex_array_3d);
  transformed_param_carray_3d = pow(p_complex, d_complex_array_3d);

  transformed_param_carray = pow(d_complex_array, p_complex_array);
  transformed_param_carray = pow(p_complex_array, d_complex_array);
  transformed_param_carray = pow(p_complex_array, p_complex_array);
  transformed_param_carray_2d = pow(d_complex_array_2d, p_complex_array_2d);
  transformed_param_carray_2d = pow(p_complex_array_2d, d_complex_array_2d);
  transformed_param_carray_2d = pow(p_complex_array_2d, p_complex_array_2d);
  transformed_param_carray_3d = pow(d_complex_array_3d, p_complex_array_3d);
  transformed_param_carray_3d = pow(p_complex_array_3d, d_complex_array_3d);
  transformed_param_carray_3d = pow(p_complex_array_3d, p_complex_array_3d);

  transformed_param_cvector = pow(p_cvector, p_complex);
  transformed_param_cvector = pow(p_cvector, d_complex);
  transformed_param_cvector = pow(p_cvector, d_int);
  transformed_param_cvector = pow(d_cvector, p_complex);
  transformed_param_cvector_array = pow(p_cvector_array, p_complex);
  transformed_param_cvector_array = pow(p_cvector_array, d_complex);
  transformed_param_cvector_array = pow(p_cvector_array, d_int);
  transformed_param_cvector_array = pow(d_cvector_array, p_complex);
  transformed_param_cvector_array_2d = pow(p_cvector_array_2d, p_complex);
  transformed_param_cvector_array_2d = pow(p_cvector_array_2d, d_complex);
  transformed_param_cvector_array_2d = pow(p_cvector_array_2d, d_int);
  transformed_param_cvector_array_2d = pow(d_cvector_array_2d, p_complex);
  transformed_param_cvector_array_3d = pow(p_cvector_array_3d, p_complex);
  transformed_param_cvector_array_3d = pow(p_cvector_array_3d, d_complex);
  transformed_param_cvector_array_3d = pow(p_cvector_array_3d, d_int);
  transformed_param_cvector_array_3d = pow(d_cvector_array_3d, p_complex);

  transformed_param_cvector = pow(p_complex, p_cvector);
  transformed_param_cvector = pow(p_complex, d_cvector);
  transformed_param_cvector = pow(d_complex, p_cvector);
  transformed_param_cvector = pow(d_int, p_cvector);
  transformed_param_cvector_array = pow(p_complex, p_cvector_array);
  transformed_param_cvector_array = pow(p_complex, d_cvector_array);
  transformed_param_cvector_array = pow(d_complex, p_cvector_array);
  transformed_param_cvector_array = pow(d_int, p_cvector_array);
  transformed_param_cvector_array_2d = pow(p_complex, p_cvector_array_2d);
  transformed_param_cvector_array_2d = pow(p_complex, d_cvector_array_2d);
  transformed_param_cvector_array_2d = pow(d_complex, p_cvector_array_2d);
  transformed_param_cvector_array_2d = pow(d_int, p_cvector_array_2d);
  transformed_param_cvector_array_3d = pow(p_complex, p_cvector_array_3d);
  transformed_param_cvector_array_3d = pow(p_complex, d_cvector_array_3d);
  transformed_param_cvector_array_3d = pow(d_complex, p_cvector_array_3d);
  transformed_param_cvector_array_3d = pow(d_int, p_cvector_array_3d);

  transformed_param_cvector = pow(p_cvector, p_cvector);
  transformed_param_cvector = pow(p_cvector, d_cvector);
  transformed_param_cvector = pow(d_cvector, p_cvector);
  transformed_param_cvector_array = pow(p_cvector_array, p_cvector_array);
  transformed_param_cvector_array = pow(p_cvector_array, d_cvector_array);
  transformed_param_cvector_array = pow(d_cvector_array, p_cvector_array);
  transformed_param_cvector_array_2d = pow(p_cvector_array_2d,
                                           p_cvector_array_2d);
  transformed_param_cvector_array_2d = pow(p_cvector_array_2d,
                                           d_cvector_array_2d);
  transformed_param_cvector_array_2d = pow(d_cvector_array_2d,
                                           p_cvector_array_2d);
  transformed_param_cvector_array_3d = pow(p_cvector_array_3d,
                                           p_cvector_array_3d);
  transformed_param_cvector_array_3d = pow(p_cvector_array_3d,
                                           d_cvector_array_3d);
  transformed_param_cvector_array_3d = pow(d_cvector_array_3d,
                                           p_cvector_array_3d);

  transformed_param_crow_vector = pow(p_crow_vector, p_complex);
  transformed_param_crow_vector = pow(p_crow_vector, d_complex);
  transformed_param_crow_vector = pow(p_crow_vector, d_int);
  transformed_param_crow_vector = pow(d_crow_vector, p_complex);
  transformed_param_crow_vector_array = pow(p_crow_vector_array, p_complex);
  transformed_param_crow_vector_array = pow(p_crow_vector_array, d_complex);
  transformed_param_crow_vector_array = pow(p_crow_vector_array, d_int);
  transformed_param_crow_vector_array = pow(d_crow_vector_array, p_complex);
  transformed_param_crow_vector_array_2d = pow(p_crow_vector_array_2d, p_complex);
  transformed_param_crow_vector_array_2d = pow(p_crow_vector_array_2d, d_complex);
  transformed_param_crow_vector_array_2d = pow(p_crow_vector_array_2d, d_int);
  transformed_param_crow_vector_array_2d = pow(d_crow_vector_array_2d, p_complex);
  transformed_param_crow_vector_array_3d = pow(p_crow_vector_array_3d, p_complex);
  transformed_param_crow_vector_array_3d = pow(p_crow_vector_array_3d, d_complex);
  transformed_param_crow_vector_array_3d = pow(p_crow_vector_array_3d, d_int);
  transformed_param_crow_vector_array_3d = pow(d_crow_vector_array_3d, p_complex);

  transformed_param_crow_vector = pow(p_complex, p_crow_vector);
  transformed_param_crow_vector = pow(d_complex, p_crow_vector);
  transformed_param_crow_vector = pow(d_int, p_crow_vector);
  transformed_param_crow_vector = pow(p_complex, d_crow_vector);
  transformed_param_crow_vector_array = pow(p_complex, p_crow_vector_array);
  transformed_param_crow_vector_array = pow(d_complex, p_crow_vector_array);
  transformed_param_crow_vector_array = pow(d_int, p_crow_vector_array);
  transformed_param_crow_vector_array = pow(p_complex, d_crow_vector_array);
  transformed_param_crow_vector_array_2d = pow(p_complex, p_crow_vector_array_2d);
  transformed_param_crow_vector_array_2d = pow(d_complex, p_crow_vector_array_2d);
  transformed_param_crow_vector_array_2d = pow(d_int, p_crow_vector_array_2d);
  transformed_param_crow_vector_array_2d = pow(p_complex, d_crow_vector_array_2d);
  transformed_param_crow_vector_array_3d = pow(p_complex, p_crow_vector_array_3d);
  transformed_param_crow_vector_array_3d = pow(d_complex, p_crow_vector_array_3d);
  transformed_param_crow_vector_array_3d = pow(d_int, p_crow_vector_array_3d);
  transformed_param_crow_vector_array_3d = pow(p_complex, d_crow_vector_array_3d);

  transformed_param_crow_vector = pow(p_crow_vector, p_crow_vector);
  transformed_param_crow_vector = pow(p_crow_vector, d_crow_vector);
  transformed_param_crow_vector = pow(d_crow_vector, p_crow_vector);
  transformed_param_crow_vector_array = pow(p_crow_vector_array,
                                            p_crow_vector_array);
  transformed_param_crow_vector_array = pow(p_crow_vector_array,
                                            d_crow_vector_array);
  transformed_param_crow_vector_array = pow(d_crow_vector_array,
                                            p_crow_vector_array);
  transformed_param_crow_vector_array_2d = pow(p_crow_vector_array_2d,
                                               p_crow_vector_array_2d);
  transformed_param_crow_vector_array_2d = pow(p_crow_vector_array_2d,
                                               d_crow_vector_array_2d);
  transformed_param_crow_vector_array_2d = pow(d_crow_vector_array_2d,
                                               p_crow_vector_array_2d);
  transformed_param_crow_vector_array_3d = pow(p_crow_vector_array_3d,
                                               p_crow_vector_array_3d);
  transformed_param_crow_vector_array_3d = pow(p_crow_vector_array_3d,
                                               d_crow_vector_array_3d);
  transformed_param_crow_vector_array_3d = pow(d_crow_vector_array_3d,
                                               p_crow_vector_array_3d);

  transformed_param_cmatrix = pow(p_cmatrix, p_complex);
  transformed_param_cmatrix = pow(p_cmatrix, d_complex);
  transformed_param_cmatrix = pow(p_cmatrix, d_int);
  transformed_param_cmatrix = pow(d_cmatrix, p_complex);
  transformed_param_cmatrix_array = pow(p_cmatrix_array, p_complex);
  transformed_param_cmatrix_array = pow(p_cmatrix_array, d_complex);
  transformed_param_cmatrix_array = pow(p_cmatrix_array, d_int);
  transformed_param_cmatrix_array = pow(d_cmatrix_array, p_complex);
  transformed_param_cmatrix_array_2d = pow(p_cmatrix_array_2d, p_complex);
  transformed_param_cmatrix_array_2d = pow(p_cmatrix_array_2d, d_complex);
  transformed_param_cmatrix_array_2d = pow(p_cmatrix_array_2d, d_int);
  transformed_param_cmatrix_array_2d = pow(d_cmatrix_array_2d, p_complex);
  transformed_param_cmatrix_array_3d = pow(p_cmatrix_array_3d, p_complex);
  transformed_param_cmatrix_array_3d = pow(p_cmatrix_array_3d, d_complex);
  transformed_param_cmatrix_array_3d = pow(p_cmatrix_array_3d, d_int);
  transformed_param_cmatrix_array_3d = pow(d_cmatrix_array_3d, p_complex);

  transformed_param_cmatrix = pow(p_complex, p_cmatrix);
  transformed_param_cmatrix = pow(p_complex, d_cmatrix);
  transformed_param_cmatrix = pow(d_complex, p_cmatrix);
  transformed_param_cmatrix = pow(d_int, p_cmatrix);
  transformed_param_cmatrix_array = pow(p_complex, p_cmatrix_array);
  transformed_param_cmatrix_array = pow(p_complex, d_cmatrix_array);
  transformed_param_cmatrix_array = pow(d_complex, p_cmatrix_array);
  transformed_param_cmatrix_array = pow(d_int, p_cmatrix_array);
  transformed_param_cmatrix_array_2d = pow(p_complex, p_cmatrix_array_2d);
  transformed_param_cmatrix_array_2d = pow(p_complex, d_cmatrix_array_2d);
  transformed_param_cmatrix_array_2d = pow(d_complex, p_cmatrix_array_2d);
  transformed_param_cmatrix_array_2d = pow(d_int, p_cmatrix_array_2d);
  transformed_param_cmatrix_array_3d = pow(p_complex, p_cmatrix_array_3d);
  transformed_param_cmatrix_array_3d = pow(p_complex, d_cmatrix_array_3d);
  transformed_param_cmatrix_array_3d = pow(d_complex, p_cmatrix_array_3d);
  transformed_param_cmatrix_array_3d = pow(d_int, p_cmatrix_array_3d);

  transformed_param_cmatrix = pow(p_cmatrix, p_cmatrix);
  transformed_param_cmatrix = pow(p_cmatrix, d_cmatrix);
  transformed_param_cmatrix = pow(d_cmatrix, p_cmatrix);
  transformed_param_cmatrix_array = pow(p_cmatrix_array, p_cmatrix_array);
  transformed_param_cmatrix_array = pow(p_cmatrix_array, d_cmatrix_array);
  transformed_param_cmatrix_array = pow(d_cmatrix_array, p_cmatrix_array);
  transformed_param_cmatrix_array_2d = pow(p_cmatrix_array_2d,
                                           p_cmatrix_array_2d);
  transformed_param_cmatrix_array_2d = pow(p_cmatrix_array_2d,
                                           d_cmatrix_array_2d);
  transformed_param_cmatrix_array_2d = pow(d_cmatrix_array_2d,
                                           p_cmatrix_array_2d);
  transformed_param_cmatrix_array_3d = pow(p_cmatrix_array_3d,
                                           p_cmatrix_array_3d);
  transformed_param_cmatrix_array_3d = pow(p_cmatrix_array_3d,
                                           d_cmatrix_array_3d);
  transformed_param_cmatrix_array_3d = pow(d_cmatrix_array_3d,
                                           p_cmatrix_array_3d);
}
model {
  y_p ~ normal(0, 1);
}

