data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
  complex_matrix[d_int,d_int] d_cmatrix;
}

transformed data {
  real transformed_data_real;

  transformed_data_real = trace(d_matrix);

  complex transformed_data_complex = trace(d_cmatrix);
}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
  complex_matrix[d_int,d_int] p_cmatrix;
}
transformed parameters {
  real transformed_param_real;

  transformed_param_real = trace(d_matrix);
  transformed_param_real = trace(p_matrix);

  complex transformed_param_complex;
  transformed_param_complex = trace(d_cmatrix);
  transformed_param_complex = trace(p_cmatrix);

}
model {
  y_p ~ normal(0,1);
}
