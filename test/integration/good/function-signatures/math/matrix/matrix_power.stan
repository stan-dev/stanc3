data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
}
transformed data {
  matrix[d_int,d_int] transformed_data_matrix;
  transformed_data_matrix = matrix_power(d_matrix, d_int);
}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
}
transformed parameters {
  matrix[d_int,d_int] transformed_param_matrix;
        
  transformed_param_matrix = matrix_power(p_matrix, d_int);
}
model {
  y_p ~ normal(0,1);
}
