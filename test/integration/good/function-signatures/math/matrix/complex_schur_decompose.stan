data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  complex_matrix[d_int, d_int] d_cmatrix;
}
transformed data {
  complex_matrix[d_int, d_int] transformed_data_cmatrix;
  transformed_data_cmatrix = complex_schur_decompose_t(d_matrix);
  transformed_data_cmatrix = complex_schur_decompose_t(d_cmatrix);
  
  transformed_data_cmatrix = complex_schur_decompose_u(d_matrix);
  transformed_data_cmatrix = complex_schur_decompose_u(d_cmatrix);
  
  tuple(complex_matrix[d_int, d_int], complex_matrix[d_int, d_int]) transformed_data_cmatrix_tuple;
  transformed_data_cmatrix_tuple = complex_schur_decompose(d_matrix);
  transformed_data_cmatrix_tuple = complex_schur_decompose(d_cmatrix);
}
parameters {
  real y_p;
  
  matrix[d_int, d_int] p_matrix;
  complex_matrix[d_int, d_int] p_cmatrix;
}
transformed parameters {
  complex_matrix[d_int, d_int] transformed_param_cmatrix;
  
  transformed_param_cmatrix = complex_schur_decompose_t(d_matrix);
  transformed_param_cmatrix = complex_schur_decompose_t(p_matrix);
  transformed_param_cmatrix = complex_schur_decompose_t(d_cmatrix);
  transformed_param_cmatrix = complex_schur_decompose_t(p_cmatrix);
  
  transformed_param_cmatrix = complex_schur_decompose_u(d_matrix);
  transformed_param_cmatrix = complex_schur_decompose_u(p_matrix);
  transformed_param_cmatrix = complex_schur_decompose_u(d_cmatrix);
  transformed_param_cmatrix = complex_schur_decompose_u(p_cmatrix);
  
  tuple(complex_matrix[d_int, d_int], complex_matrix[d_int, d_int]) transformed_param_cmatrix_tuple;
  transformed_param_cmatrix_tuple = complex_schur_decompose(d_matrix);
  transformed_param_cmatrix_tuple = complex_schur_decompose(p_matrix);
  transformed_param_cmatrix_tuple = complex_schur_decompose(d_cmatrix);
  transformed_param_cmatrix_tuple = complex_schur_decompose(p_cmatrix);
}
model {
  y_p ~ normal(0, 1);
}
