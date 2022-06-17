data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
}

transformed data {
  complex_matrix[d_int,d_int] transformed_data_matrix;

  transformed_data_matrix = eigenvectors(d_matrix);
}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
}
transformed parameters {
  complex_matrix[d_int,d_int] transformed_param_matrix;

  transformed_param_matrix = eigenvectors(d_matrix);
  transformed_param_matrix = eigenvectors(p_matrix);
}
model {
  y_p ~ normal(0,1);
}
