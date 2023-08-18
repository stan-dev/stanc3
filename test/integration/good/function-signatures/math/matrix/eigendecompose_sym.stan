data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  complex_matrix[d_int, d_int] d_cmatrix;
}
transformed data {
  tuple(matrix[d_int, d_int], vector[d_int]) eigendec;
  
  eigendec = eigendecompose_sym(d_matrix);
  
  tuple(complex_matrix[d_int, d_int], complex_vector[d_int]) eigendecc;
  eigendecc = eigendecompose_sym(d_cmatrix);
}
parameters {
  real y_p;
  matrix[d_int, d_int] p_matrix;
  complex_matrix[d_int, d_int] p_cmatrix;
}
transformed parameters {
  tuple(matrix[d_int, d_int], vector[d_int]) tp_eigendec;
  
  tp_eigendec = eigendecompose_sym(d_matrix);
  tp_eigendec = eigendecompose_sym(p_matrix);
  
  tuple(complex_matrix[d_int, d_int], complex_vector[d_int]) tp_eigendecc;
  
  tp_eigendecc = eigendecompose_sym(d_cmatrix);
  tp_eigendecc = eigendecompose_sym(p_cmatrix);
}
model {
  y_p ~ normal(0, 1);
}
