data {
  int d_int;
  array[d_int] int d_int_array;
  array[d_int] real d_real_array;
  matrix[d_int, d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;

  array[d_int] complex d_complex_array;
  complex_matrix[d_int, d_int] d_cmatrix;
  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;

}
transformed data {
  int transformed_data_int;
  real transformed_data_real;

  transformed_data_real = prod(d_int_array);
  transformed_data_real = prod(d_real_array);
  transformed_data_real = prod(d_matrix);
  transformed_data_real = prod(d_vector);
  transformed_data_real = prod(d_row_vector);

  complex transformed_data_complex;
  transformed_data_complex = prod(d_complex_array);
  transformed_data_complex = prod(d_cmatrix);
  transformed_data_complex = prod(d_cvector);
  transformed_data_complex = prod(d_crow_vector);
}
parameters {
  real p_real;
  real y_p;
  array[d_int] real p_real_array;
  matrix[d_int, d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;


  array[d_int] complex p_complex_array;
  complex_matrix[d_int, d_int] p_cmatrix;
  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;

}
transformed parameters {
  real transformed_param_real;

  transformed_param_real = prod(d_int_array);
  transformed_param_real = prod(d_real_array);
  transformed_param_real = prod(d_matrix);
  transformed_param_real = prod(d_vector);
  transformed_param_real = prod(d_row_vector);
  transformed_param_real = prod(p_real_array);
  transformed_param_real = prod(p_matrix);
  transformed_param_real = prod(p_vector);
  transformed_param_real = prod(p_row_vector);

  complex transformed_param_complex;
  transformed_param_complex = prod(d_complex_array);
  transformed_param_complex = prod(d_cmatrix);
  transformed_param_complex = prod(d_cvector);
  transformed_param_complex = prod(d_crow_vector);
  transformed_param_complex = prod(p_complex_array);
  transformed_param_complex = prod(p_cmatrix);
  transformed_param_complex = prod(p_cvector);
  transformed_param_complex = prod(p_crow_vector);
}
model {
  y_p ~ normal(0, 1);
}

