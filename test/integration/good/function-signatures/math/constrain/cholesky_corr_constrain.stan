data { 
  int d_int;
  vector[(d_int-1)*d_int/2] d_vector;
}

transformed data {
  matrix[d_int,d_int] transformed_data_matrix;

  transformed_data_matrix = cholesky_corr_constrain(d_vector, d_int);
}
parameters {
  real y_p;
  vector[(d_int-1)*d_int/2] p_vector;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;

  transformed_param_matrix = cholesky_corr_constrain(d_vector, d_int);
  transformed_param_matrix = cholesky_corr_constrain(p_vector, d_int);
}
model {  
  y_p ~ normal(0,1);
}