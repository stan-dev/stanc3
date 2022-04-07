data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
  complex_matrix[d_int,d_int] d_cmatrix;

}

transformed data {
  matrix[d_int,d_int] transformed_data_matrix;

  transformed_data_matrix = symmetrize_from_lower_tri(d_matrix);

  complex_matrix[d_int,d_int] transformed_data_cmatrix;

  transformed_data_cmatrix = symmetrize_from_lower_tri(d_cmatrix);

}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
  complex_matrix[d_int,d_int] p_cmatrix;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;

  transformed_param_matrix = symmetrize_from_lower_tri(d_matrix);
  transformed_param_matrix = symmetrize_from_lower_tri(p_matrix);

  complex_matrix[d_int,d_int] transformed_param_cmatrix;

  transformed_param_cmatrix = symmetrize_from_lower_tri(d_cmatrix);
  transformed_param_cmatrix = symmetrize_from_lower_tri(p_cmatrix);
}
model {
  y_p ~ normal(0,1);
}
