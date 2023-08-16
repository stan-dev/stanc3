data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  complex_matrix[d_int, d_int] d_cmatrix;
}
transformed data {
  tuple(complex_matrix[d_int, d_int], complex_vector[d_int]) eigendec;
  
  eigendec = eigendecompose(d_matrix);
  eigendec = eigendecompose(d_cmatrix);
}
parameters {
  real y_p;
  matrix[d_int, d_int] p_matrix;
  complex_matrix[d_int, d_int] p_cmatrix;
}
transformed parameters {
  tuple(complex_matrix[d_int, d_int], complex_vector[d_int]) tp_eigendec;
  
  tp_eigendec = eigendecompose(d_matrix);
  tp_eigendec = eigendecompose(p_matrix);
  
  tp_eigendec = eigendecompose(d_cmatrix);
  tp_eigendec = eigendecompose(p_cmatrix);
}
model {
  y_p ~ normal(0, 1);
}
