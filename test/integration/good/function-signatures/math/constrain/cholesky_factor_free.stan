data { 
  int d_int;
  matrix[d_int,d_int] d_matrix;
}
transformed data {
  vector[(d_int+1)*d_int/2] transformed_data_vector;

  transformed_data_vector = cholesky_factor_free(d_matrix);
}
parameters {
  matrix[d_int,d_int] p_matrix;
  real y_p;
}
transformed parameters {
  vector[(d_int+1)*d_int/2] transformed_param_vector;

  transformed_param_vector = cholesky_factor_free(d_matrix);
  transformed_param_vector = cholesky_factor_free(p_matrix);
}
model {  
  y_p ~ normal(0,1);
}