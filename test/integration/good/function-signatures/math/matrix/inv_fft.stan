data {
  int d_int;
  complex_vector[d_int] d_cvector;
  complex_matrix[d_int, d_int] d_cmatrix;
}
transformed data {

  complex_vector[d_int] transformed_data_cvector = inv_fft(d_cvector);

  complex_matrix[d_int, d_int] transformed_data_cmatrix = inv_fft2(d_cmatrix);
}
parameters {
  real y_p;

  complex_vector[d_int] p_cvector;
  complex_matrix[d_int, d_int] p_cmatrix;
}
transformed parameters {

  complex_vector[d_int] transformed_param_cvector;
  transformed_param_cvector = inv_fft(d_cvector);
  transformed_param_cvector = inv_fft(p_cvector);

  complex_matrix[d_int, d_int] transformed_param_cmatrix;


  transformed_param_cmatrix = inv_fft2(d_cmatrix);
  transformed_param_cmatrix = inv_fft2(p_cmatrix);

}
model {
  y_p ~ normal(0,1);
}
