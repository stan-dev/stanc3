data {
  int d_int;
  array[d_int] int d_int_array;
  array[d_int] real d_real_array;
  matrix[d_int, d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  
  array[3] vector[2] x3y;
  array[3] row_vector[2] x4y;
  array[3] matrix[2, 3] x5y;
  
  array[3, 4] int x1z;
  array[3, 4] real x2z;
  array[3, 4] vector[2] x3z;
  array[3, 4] row_vector[2] x4z;
  array[3, 4] matrix[2, 3] x5z;
  
  array[3, 4, 5] int x1w;
  array[3, 4, 5] real x2w;
  array[3, 4, 5] vector[2] x3w;
  array[3, 4, 5] row_vector[2] x4w;
  array[3, 4, 5] matrix[2, 3] x5w;
}
transformed data {
  matrix[d_int, d_int] transformed_data_matrix;
  vector[d_int] transformed_data_vector;
  row_vector[d_int] transformed_data_row_vector;
  
  array[3] vector[2] trans_x3y;
  array[3] row_vector[2] trans_x4y;
  array[3] matrix[2, 3] trans_x5y;
  
  array[3, 4] real trans_x2z;
  array[3, 4] vector[2] trans_x3z;
  array[3, 4] row_vector[2] trans_x4z;
  array[3, 4] matrix[2, 3] trans_x5z;
  
  array[3, 4, 5] real trans_x2w;
  array[3, 4, 5] vector[2] trans_x3w;
  array[3, 4, 5] row_vector[2] trans_x4w;
  array[3, 4, 5] matrix[2, 3] trans_x5w;
  
  transformed_data_matrix = inv_cloglog(d_matrix);
  transformed_data_vector = inv_cloglog(d_vector);
  transformed_data_row_vector = inv_cloglog(d_row_vector);
  trans_x3y = inv_cloglog(x3y);
  trans_x4y = inv_cloglog(x4y);
  trans_x5y = inv_cloglog(x5y);
  
  trans_x2z = inv_cloglog(x1z);
  trans_x2z = inv_cloglog(x2z);
  trans_x3z = inv_cloglog(x3z);
  trans_x4z = inv_cloglog(x4z);
  trans_x5z = inv_cloglog(x5z);
  
  trans_x2w = inv_cloglog(x1w);
  trans_x2w = inv_cloglog(x2w);
  trans_x3w = inv_cloglog(x3w);
  trans_x4w = inv_cloglog(x4w);
  trans_x5w = inv_cloglog(x5w);
}
parameters {
  real p_real;
  real y_p;
  array[d_int] real p_real_array;
  matrix[d_int, d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  
  array[3] vector[2] p_x3y;
  array[3] row_vector[2] p_x4y;
  array[3] matrix[2, 3] p_x5y;
  
  array[3, 4] real p_x2z;
  array[3, 4] vector[2] p_x3z;
  array[3, 4] row_vector[2] p_x4z;
  array[3, 4] matrix[2, 3] p_x5z;
  
  array[3, 4, 5] real p_x2w;
  array[3, 4, 5] vector[2] p_x3w;
  array[3, 4, 5] row_vector[2] p_x4w;
  array[3, 4, 5] matrix[2, 3] p_x5w;
}
transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;
  vector[d_int] transformed_param_vector;
  row_vector[d_int] transformed_param_row_vector;
  array[3] vector[2] trans_p_x3y;
  array[3] row_vector[2] trans_p_x4y;
  array[3] matrix[2, 3] trans_p_x5y;
  
  array[3, 4] real trans_p_x2z;
  array[3, 4] vector[2] trans_p_x3z;
  array[3, 4] row_vector[2] trans_p_x4z;
  array[3, 4] matrix[2, 3] trans_p_x5z;
  
  array[3, 4, 5] real trans_p_x2w;
  array[3, 4, 5] vector[2] trans_p_x3w;
  array[3, 4, 5] row_vector[2] trans_p_x4w;
  array[3, 4, 5] matrix[2, 3] trans_p_x5w;
  
  transformed_param_matrix = inv_cloglog(d_matrix);
  transformed_param_vector = inv_cloglog(d_vector);
  transformed_param_row_vector = inv_cloglog(d_row_vector);
  transformed_param_matrix = inv_cloglog(p_matrix);
  transformed_param_vector = inv_cloglog(p_vector);
  transformed_param_row_vector = inv_cloglog(p_row_vector);
  
  trans_p_x3y = inv_cloglog(p_x3y);
  trans_p_x4y = inv_cloglog(p_x4y);
  trans_p_x5y = inv_cloglog(p_x5y);
  
  trans_p_x2z = inv_cloglog(p_x2z);
  trans_p_x3z = inv_cloglog(p_x3z);
  trans_p_x4z = inv_cloglog(p_x4z);
  trans_p_x5z = inv_cloglog(p_x5z);
  
  trans_p_x2w = inv_cloglog(p_x2w);
  trans_p_x3w = inv_cloglog(p_x3w);
  trans_p_x4w = inv_cloglog(p_x4w);
  trans_p_x5w = inv_cloglog(p_x5w);
}
model {
  y_p ~ normal(0, 1);
}

