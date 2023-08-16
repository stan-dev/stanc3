data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  complex_matrix[d_int, d_int] d_cmatrix;
}
transformed data {
  tuple(matrix[d_int, d_int], vector[d_int], matrix[d_int, d_int]) transformed_data_svd_tuple;
  
  transformed_data_svd_tuple = svd(d_matrix);
  
  tuple(complex_matrix[d_int, d_int], vector[d_int],
        complex_matrix[d_int, d_int]) transformed_data_csvd_tuple;
  transformed_data_csvd_tuple = svd(d_cmatrix);
}
parameters {
  real y_p;
  matrix[d_int, d_int] p_matrix;
  complex_matrix[d_int, d_int] p_cmatrix;
}
transformed parameters {
  tuple(matrix[d_int, d_int], vector[d_int], matrix[d_int, d_int]) transformed_param_svd_tuple;
  
  transformed_param_svd_tuple = svd(d_matrix);
  transformed_param_svd_tuple = svd(p_matrix);
  
  tuple(complex_matrix[d_int, d_int], vector[d_int],
        complex_matrix[d_int, d_int]) transformed_param_csvd_tuple;
  
  transformed_param_csvd_tuple = svd(d_cmatrix);
  transformed_param_csvd_tuple = svd(p_cmatrix);
}
model {
  y_p ~ normal(0, 1);
}
