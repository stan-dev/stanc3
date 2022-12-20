data {
  int d_int;
  real d_real;
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
  real transformed_data_real;
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
  
  transformed_data_real = std_normal_log_qf(d_int);
  transformed_data_real = std_normal_log_qf(d_real);
  transformed_data_matrix = std_normal_log_qf(d_matrix);
  transformed_data_vector = std_normal_log_qf(d_vector);
  transformed_data_row_vector = std_normal_log_qf(d_row_vector);
  trans_x3y = std_normal_log_qf(x3y);
  trans_x4y = std_normal_log_qf(x4y);
  trans_x5y = std_normal_log_qf(x5y);
  
  trans_x2z = std_normal_log_qf(x1z);
  trans_x2z = std_normal_log_qf(x2z);
  trans_x3z = std_normal_log_qf(x3z);
  trans_x4z = std_normal_log_qf(x4z);
  trans_x5z = std_normal_log_qf(x5z);
  
  trans_x2w = std_normal_log_qf(x1w);
  trans_x2w = std_normal_log_qf(x2w);
  trans_x3w = std_normal_log_qf(x3w);
  trans_x4w = std_normal_log_qf(x4w);
  trans_x5w = std_normal_log_qf(x5w);
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
  real transformed_param_real;
  
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
  
  transformed_param_real = std_normal_log_qf(d_int);
  transformed_param_real = std_normal_log_qf(d_real);
  transformed_param_real = std_normal_log_qf(p_real);
  
  transformed_param_matrix = std_normal_log_qf(d_matrix);
  transformed_param_vector = std_normal_log_qf(d_vector);
  transformed_param_row_vector = std_normal_log_qf(d_row_vector);
  transformed_param_matrix = std_normal_log_qf(p_matrix);
  transformed_param_vector = std_normal_log_qf(p_vector);
  transformed_param_row_vector = std_normal_log_qf(p_row_vector);
  
  trans_p_x3y = std_normal_log_qf(p_x3y);
  trans_p_x4y = std_normal_log_qf(p_x4y);
  trans_p_x5y = std_normal_log_qf(p_x5y);
  
  trans_p_x2z = std_normal_log_qf(p_x2z);
  trans_p_x3z = std_normal_log_qf(p_x3z);
  trans_p_x4z = std_normal_log_qf(p_x4z);
  trans_p_x5z = std_normal_log_qf(p_x5z);
  
  trans_p_x2w = std_normal_log_qf(p_x2w);
  trans_p_x3w = std_normal_log_qf(p_x3w);
  trans_p_x4w = std_normal_log_qf(p_x4w);
  trans_p_x5w = std_normal_log_qf(p_x5w);
}
model {
  y_p ~ normal(0, 1);
}
