data { 
  int d_row;
  int d_col;
  matrix[d_row,d_col] d_matrix;
}

transformed data {
  matrix[d_col,d_row] transformed_data_matrix;
  transformed_data_matrix = generalized_inverse(d_matrix);
}
parameters {
  real y_p;
  matrix[d_row,d_col] p_matrix;
}
transformed parameters {
  matrix[d_col,d_row] transformed_param_matrix;

  transformed_param_matrix = generalized_inverse(d_matrix);
  transformed_param_matrix = generalized_inverse(p_matrix);
}
model {  
  y_p ~ normal(0,1);
}
