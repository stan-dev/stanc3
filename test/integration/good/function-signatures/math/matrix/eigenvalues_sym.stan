data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
  complex_matrix[d_int,d_int] d_cmatrix;
}

transformed data {
  vector[d_int] transformed_data_vector;

  transformed_data_vector = eigenvalues_sym(d_matrix);

  complex_vector[d_int] transformed_data_cvector;

  transformed_data_cvector = eigenvalues_sym(d_cmatrix);

}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
  complex_matrix[d_int,d_int] p_cmatrix;
}
transformed parameters {
  vector[d_int] transformed_param_vector;

  transformed_param_vector = eigenvalues_sym(d_matrix);
  transformed_param_vector = eigenvalues_sym(p_matrix);

  complex_vector[d_int] transformed_param_cvector;
  transformed_param_cvector = eigenvalues_sym(d_cmatrix);
  transformed_param_cvector = eigenvalues_sym(p_cmatrix);
}
model {
  y_p ~ normal(0,1);
}
