data {
  int d_int;

  array[d_int] complex d_complex_array;
  complex_matrix[d_int, d_int] d_cmatrix;
  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;

  array[3] complex_vector[2] cx3y;
  array[3] complex_row_vector[2] cx4y;
  array[3] complex_matrix[2, 3] cx5y;

  array[3, 4] complex cx2z;
  array[3, 4] complex_vector[2] cx3z;
  array[3, 4] complex_row_vector[2] cx4z;
  array[3, 4] complex_matrix[2, 3] cx5z;

  array[3, 4, 5] complex cx2w;
  array[3, 4, 5] complex_vector[2] cx3w;
  array[3, 4, 5] complex_row_vector[2] cx4w;
  array[3, 4, 5] complex_matrix[2, 3] cx5w;


}
transformed data {
  complex_matrix[d_int, d_int] transformed_data_matrix;
  complex_vector[d_int] transformed_data_vector;
  complex_row_vector[d_int] transformed_data_row_vector;

  array[3] complex_vector[2] trans_x3y;
  array[3] complex_row_vector[2] trans_x4y;
  array[3] complex_matrix[2, 3] trans_x5y;

  array[3, 4] complex trans_x2z;
  array[3, 4] complex_vector[2] trans_x3z;
  array[3, 4] complex_row_vector[2] trans_x4z;
  array[3, 4] complex_matrix[2, 3] trans_x5z;

  array[3, 4, 5] complex trans_x2w;
  array[3, 4, 5] complex_vector[2] trans_x3w;
  array[3, 4, 5] complex_row_vector[2] trans_x4w;
  array[3, 4, 5] complex_matrix[2, 3] trans_x5w;

  transformed_data_matrix = conj(d_cmatrix);
  transformed_data_vector = conj(d_cvector);
  transformed_data_row_vector = conj(d_crow_vector);

  trans_x3y = conj(cx3y);
  trans_x4y = conj(cx4y);
  trans_x5y = conj(cx5y);

  trans_x2z = conj(cx2z);
  trans_x3z = conj(cx3z);
  trans_x4z = conj(cx4z);
  trans_x5z = conj(cx5z);

  trans_x2w = conj(cx2w);
  trans_x3w = conj(cx3w);
  trans_x4w = conj(cx4w);
  trans_x5w = conj(cx5w);

}
parameters {
  real y_p;

  complex p_complex;
  array[d_int] complex p_complex_array;
  complex_matrix[d_int, d_int] p_cmatrix;
  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;

  array[3] complex_vector[2] p_cx3y;
  array[3] complex_row_vector[2] p_cx4y;
  array[3] complex_matrix[2, 3] p_cx5y;

  array[3, 4] complex p_cx2z;
  array[3, 4] complex_vector[2] p_cx3z;
  array[3, 4] complex_row_vector[2] p_cx4z;
  array[3, 4] complex_matrix[2, 3] p_cx5z;

  array[3, 4, 5] complex p_cx2w;
  array[3, 4, 5] complex_vector[2] p_cx3w;
  array[3, 4, 5] complex_row_vector[2] p_cx4w;
  array[3, 4, 5] complex_matrix[2, 3] p_cx5w;

}
transformed parameters {
  complex_matrix[d_int, d_int] transformed_param_matrix;
  complex_vector[d_int] transformed_param_vector;
  complex_row_vector[d_int] transformed_param_row_vector;
  array[3] complex_vector[2] trans_p_x3y;
  array[3] complex_row_vector[2] trans_p_x4y;
  array[3] complex_matrix[2, 3] trans_p_x5y;

  array[3, 4] complex trans_p_x2z;
  array[3, 4] complex_vector[2] trans_p_x3z;
  array[3, 4] complex_row_vector[2] trans_p_x4z;
  array[3, 4] complex_matrix[2, 3] trans_p_x5z;

  array[3, 4, 5] complex trans_p_x2w;
  array[3, 4, 5] complex_vector[2] trans_p_x3w;
  array[3, 4, 5] complex_row_vector[2] trans_p_x4w;
  array[3, 4, 5] complex_matrix[2, 3] trans_p_x5w;

  transformed_param_matrix = conj(d_cmatrix);
  transformed_param_vector = conj(d_cvector);
  transformed_param_row_vector = conj(d_crow_vector);
  transformed_param_matrix = conj(p_cmatrix);
  transformed_param_vector = conj(p_cvector);
  transformed_param_row_vector = conj(p_crow_vector);

  trans_p_x3y = conj(p_cx3y);
  trans_p_x4y = conj(p_cx4y);
  trans_p_x5y = conj(p_cx5y);

  trans_p_x2z = conj(p_cx2z);
  trans_p_x3z = conj(p_cx3z);
  trans_p_x4z = conj(p_cx4z);
  trans_p_x5z = conj(p_cx5z);

  trans_p_x2w = conj(p_cx2w);
  trans_p_x3w = conj(p_cx3w);
  trans_p_x4w = conj(p_cx4w);
  trans_p_x5w = conj(p_cx5w);
}
model {
  y_p ~ normal(0, 1);
}
