data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
  complex_matrix[d_int,d_int] d_cmatrix;

}

transformed data {
  vector[d_int] transformed_data_vector;

  transformed_data_vector = singular_values(d_matrix);
  transformed_data_vector = singular_values(d_cmatrix);
}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
  complex_matrix[d_int,d_int] p_cmatrix;
}
transformed parameters {
  vector[d_int] transformed_param_vector;

  transformed_param_vector = singular_values(d_matrix);
  transformed_param_vector = singular_values(p_matrix);

  transformed_param_vector = singular_values(d_cmatrix);
  transformed_param_vector = singular_values(p_cmatrix);

}
model {
  y_p ~ normal(0,1);
}
