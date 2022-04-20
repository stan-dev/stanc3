data {
  int d_int;
  array[d_int] complex d_complex_array;
  complex_matrix[d_int, d_int] d_matrix;
  complex_vector[d_int] d_vector;
  complex_row_vector[d_int] d_row_vector;

  array[3] complex_vector[2] x3y;
  array[3] complex_row_vector[2] x4y;
  array[3] complex_matrix[2, 3] x5y;

  array[3, 4] complex x2z;
  array[3, 4] complex_vector[2] x3z;
  array[3, 4] complex_row_vector[2] x4z;
  array[3, 4] complex_matrix[2, 3] x5z;

  array[3, 4, 5] complex x2w;
  array[3, 4, 5] complex_vector[2] x3w;
  array[3, 4, 5] complex_row_vector[2] x4w;
  array[3, 4, 5] complex_matrix[2, 3] x5w;
}

transformed data {
  matrix[d_int, d_int] transformed_data_matrix;
  vector[d_int] transformed_data_vector;
  row_vector[d_int] transformed_data_row_vector;

  array[3] vector[2] trans_x3y;
  array[3] row_vector[2] trans_x4y;
  array[3] matrix[2, 3] trans_x5y;

  array[3, 4] real trans_x2z;
  array[3, 4] vector[2] trans_x3z;
  array[3, 4] row_vector[2] trans_x4z;
  array[3, 4] matrix[2, 3] trans_x5z;

  array[3, 4, 5] real trans_x2w;
  array[3, 4, 5] vector[2] trans_x3w;
  array[3, 4, 5] row_vector[2] trans_x4w;
  array[3, 4, 5] matrix[2, 3] trans_x5w;

  transformed_data_matrix = get_imag(d_matrix);
  transformed_data_vector = get_imag(d_vector);
  transformed_data_row_vector = get_imag(d_row_vector);

  trans_x3y = get_imag(x3y);
  trans_x4y = get_imag(x4y);
  trans_x5y = get_imag(x5y);

  trans_x2z = get_imag(x2z);
  trans_x3z = get_imag(x3z);
  trans_x4z = get_imag(x4z);
  trans_x5z = get_imag(x5z);

  trans_x2w = get_imag(x2w);
  trans_x3w = get_imag(x3w);
  trans_x4w = get_imag(x4w);
  trans_x5w = get_imag(x5w);
}

parameters {
  complex p_complex;
  real y_p;
  array[d_int] complex p_complex_array;
  complex_matrix[d_int, d_int] p_matrix;
  complex_vector[d_int] p_vector;
  complex_row_vector[d_int] p_row_vector;

  array[3] complex_vector[2] p_x3y;
  array[3] complex_row_vector[2] p_x4y;
  array[3] complex_matrix[2, 3] p_x5y;

  array[3, 4] complex p_x2z;
  array[3, 4] complex_vector[2] p_x3z;
  array[3, 4] complex_row_vector[2] p_x4z;
  array[3, 4] complex_matrix[2, 3] p_x5z;

  array[3, 4, 5] complex p_x2w;
  array[3, 4, 5] complex_vector[2] p_x3w;
  array[3, 4, 5] complex_row_vector[2] p_x4w;
  array[3, 4, 5] complex_matrix[2, 3] p_x5w;
}
transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;
  vector[d_int] transformed_param_vector;
  row_vector[d_int] transformed_param_row_vector;
  array[3] vector[2] trans_p_x3y;
  array[3] row_vector[2] trans_p_x4y;
  array[3] matrix[2, 3] trans_p_x5y;

  array[3, 4] real trans_p_x2z;
  array[3, 4] vector[2] trans_p_x3z;
  array[3, 4] row_vector[2] trans_p_x4z;
  array[3, 4] matrix[2, 3] trans_p_x5z;

  array[3, 4, 5] real trans_p_x2w;
  array[3, 4, 5] vector[2] trans_p_x3w;
  array[3, 4, 5] row_vector[2] trans_p_x4w;
  array[3, 4, 5] matrix[2, 3] trans_p_x5w;

  transformed_param_matrix = get_imag(d_matrix);
  transformed_param_vector = get_imag(d_vector);
  transformed_param_row_vector = get_imag(d_row_vector);
  transformed_param_matrix = get_imag(p_matrix);
  transformed_param_vector = get_imag(p_vector);
  transformed_param_row_vector = get_imag(p_row_vector);

  trans_p_x3y = get_imag(p_x3y);
  trans_p_x4y = get_imag(p_x4y);
  trans_p_x5y = get_imag(p_x5y);

  trans_p_x2z = get_imag(p_x2z);
  trans_p_x3z = get_imag(p_x3z);
  trans_p_x4z = get_imag(p_x4z);
  trans_p_x5z = get_imag(p_x5z);

  trans_p_x2w = get_imag(p_x2w);
  trans_p_x3w = get_imag(p_x3w);
  trans_p_x4w = get_imag(p_x4w);
  trans_p_x5w = get_imag(p_x5w);
}
model {
  y_p ~ normal(0, 1);
}

