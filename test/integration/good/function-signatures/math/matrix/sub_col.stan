data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
  complex_matrix[d_int,d_int] d_cmatrix;

}

transformed data {
  vector[d_int] transformed_data_vector;

  transformed_data_vector = sub_col(d_matrix, d_int, d_int, d_int);

  complex_vector[d_int] transformed_data_cvector;

  transformed_data_cvector = sub_col(d_cmatrix, d_int, d_int, d_int);
}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
  complex_matrix[d_int,d_int] p_cmatrix;
}
transformed parameters {
  vector[d_int] transformed_param_vector;

  transformed_param_vector = sub_col(d_matrix, d_int, d_int, d_int);
  transformed_param_vector = sub_col(p_matrix, d_int, d_int, d_int);

  complex_vector[d_int] transformed_param_cvector;

  transformed_param_cvector = sub_col(d_cmatrix, d_int, d_int, d_int);
  transformed_param_cvector = sub_col(p_cmatrix, d_int, d_int, d_int);
}
model {
  y_p ~ normal(0,1);
}
