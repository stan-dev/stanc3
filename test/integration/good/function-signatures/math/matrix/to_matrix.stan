data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  array[d_int] row_vector[d_int] array_d_row_vector;
  array[6] real d_array;
  array[6, 2] real d_array2;
  array[6] int d_iarray;
  array[6, 2] int d_iarray2;

  complex_matrix[d_int, d_int] d_cmatrix;
  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;
  array[d_int] complex_row_vector[d_int] array_d_crow_vector;
  array[6] complex d_carray;
  array[6, 2] complex d_carray2;
}
transformed data {
  matrix[d_int, d_int] transformed_data_matrix;

  transformed_data_matrix = to_matrix(d_matrix);
  transformed_data_matrix = to_matrix(d_vector);
  transformed_data_matrix = to_matrix(d_row_vector);
  transformed_data_matrix = to_matrix(array_d_row_vector);

  transformed_data_matrix = to_matrix(d_matrix, 4, 2);
  transformed_data_matrix = to_matrix(d_vector, 4, 3);
  transformed_data_matrix = to_matrix(d_row_vector, 5, 2);

  transformed_data_matrix = to_matrix(d_matrix, 4, 2, 1);
  transformed_data_matrix = to_matrix(d_vector, 4, 3, 1);
  transformed_data_matrix = to_matrix(d_row_vector, 5, 2, 4);

  transformed_data_matrix = to_matrix(d_array, 2, 3);
  transformed_data_matrix = to_matrix(d_array, 2, 3, 1);
  transformed_data_matrix = to_matrix(d_iarray, 2, 3);
  transformed_data_matrix = to_matrix(d_iarray, 2, 3, 1);

  transformed_data_matrix = to_matrix(d_array2);
  transformed_data_matrix = to_matrix(d_iarray2);


  complex_matrix[d_int, d_int] transformed_data_cmatrix;

  transformed_data_cmatrix = to_matrix(d_cmatrix);
  transformed_data_cmatrix = to_matrix(d_cvector);
  transformed_data_cmatrix = to_matrix(d_crow_vector);
  transformed_data_cmatrix = to_matrix(array_d_row_vector);

  transformed_data_cmatrix = to_matrix(d_cmatrix, 4, 2);
  transformed_data_cmatrix = to_matrix(d_cvector, 4, 3);
  transformed_data_cmatrix = to_matrix(d_crow_vector, 5, 2);

  transformed_data_cmatrix = to_matrix(d_cmatrix, 4, 2, 1);
  transformed_data_cmatrix = to_matrix(d_cvector, 4, 3, 1);
  transformed_data_cmatrix = to_matrix(d_crow_vector, 5, 2, 4);

  transformed_data_cmatrix = to_matrix(d_carray, 2, 3);
  transformed_data_cmatrix = to_matrix(d_carray, 2, 3, 1);
}
parameters {
  real y_p;
  matrix[d_int, d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  array[d_int] row_vector[d_int] array_p_row_vector;
  array[6] real p_array;
  array[6, 7] real p_array2;

  complex_matrix[d_int, d_int] p_cmatrix;
  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;
  array[d_int] complex_row_vector[d_int] array_p_crow_vector;
  array[6] complex p_carray;
  array[6, 7] complex p_carray2;
}
transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;

  transformed_param_matrix = to_matrix(d_matrix);
  transformed_param_matrix = to_matrix(d_vector);
  transformed_param_matrix = to_matrix(d_row_vector);
  transformed_param_matrix = to_matrix(array_p_row_vector);

  transformed_param_matrix = to_matrix(d_matrix, 4, 2);
  transformed_param_matrix = to_matrix(d_vector, 3, 5);
  transformed_param_matrix = to_matrix(d_row_vector, 2, 4);

  transformed_param_matrix = to_matrix(d_matrix, 4, 2, 1);
  transformed_param_matrix = to_matrix(d_vector, 3, 5, 1);
  transformed_param_matrix = to_matrix(d_row_vector, 2, 4, 1);

  transformed_param_matrix = to_matrix(d_array, 3, 2);
  transformed_param_matrix = to_matrix(d_array, 3, 2, 1);
  transformed_param_matrix = to_matrix(d_iarray, 3, 2);
  transformed_param_matrix = to_matrix(d_iarray, 3, 2, 1);

  transformed_param_matrix = to_matrix(d_array2);
  transformed_param_matrix = to_matrix(d_iarray2);

  transformed_param_matrix = to_matrix(p_matrix);
  transformed_param_matrix = to_matrix(p_vector);
  transformed_param_matrix = to_matrix(p_row_vector);

  transformed_param_matrix = to_matrix(p_matrix, 4, 2);
  transformed_param_matrix = to_matrix(p_vector, 3, 5);
  transformed_param_matrix = to_matrix(p_row_vector, 2, 4);

  transformed_param_matrix = to_matrix(p_matrix, 4, 2, 1);
  transformed_param_matrix = to_matrix(p_vector, 3, 5, 1);
  transformed_param_matrix = to_matrix(p_row_vector, 2, 4, 1);

  transformed_param_matrix = to_matrix(p_array, 3, 2);
  transformed_param_matrix = to_matrix(p_array, 3, 2, 1);

  transformed_param_matrix = to_matrix(p_array2);


  complex_matrix[d_int, d_int] transformed_param_cmatrix;

  transformed_param_cmatrix = to_matrix(d_cmatrix);
  transformed_param_cmatrix = to_matrix(d_cvector);
  transformed_param_cmatrix = to_matrix(d_crow_vector);
  transformed_param_cmatrix = to_matrix(array_p_row_vector);

  transformed_param_cmatrix = to_matrix(d_cmatrix, 4, 2);
  transformed_param_cmatrix = to_matrix(d_cvector, 3, 5);
  transformed_param_cmatrix = to_matrix(d_crow_vector, 2, 4);

  transformed_param_cmatrix = to_matrix(d_cmatrix, 4, 2, 1);
  transformed_param_cmatrix = to_matrix(d_cvector, 3, 5, 1);
  transformed_param_cmatrix = to_matrix(d_crow_vector, 2, 4, 1);

  transformed_param_cmatrix = to_matrix(d_carray, 3, 2);
  transformed_param_cmatrix = to_matrix(d_carray, 3, 2, 1);

  transformed_param_cmatrix = to_matrix(d_carray2);

  transformed_param_cmatrix = to_matrix(p_cmatrix);
  transformed_param_cmatrix = to_matrix(p_cvector);
  transformed_param_cmatrix = to_matrix(p_crow_vector);

  transformed_param_cmatrix = to_matrix(p_cmatrix, 4, 2);
  transformed_param_cmatrix = to_matrix(p_cvector, 3, 5);
  transformed_param_cmatrix = to_matrix(p_crow_vector, 2, 4);

  transformed_param_cmatrix = to_matrix(p_cmatrix, 4, 2, 1);
  transformed_param_cmatrix = to_matrix(p_cvector, 3, 5, 1);
  transformed_param_cmatrix = to_matrix(p_crow_vector, 2, 4, 1);

  transformed_param_cmatrix = to_matrix(p_carray, 3, 2);
  transformed_param_cmatrix = to_matrix(p_carray, 3, 2, 1);

  transformed_param_cmatrix = to_matrix(p_carray2);
}
model {
  y_p ~ normal(0, 1);
}

