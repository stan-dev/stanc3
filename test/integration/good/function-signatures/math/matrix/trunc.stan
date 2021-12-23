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
  
  transformed_data_matrix = trunc(d_matrix);
  transformed_data_vector = trunc(d_vector);
  transformed_data_row_vector = trunc(d_row_vector);
  trans_x3y = trunc(x3y);
  trans_x4y = trunc(x4y);
  trans_x5y = trunc(x5y);
  
  trans_x2z = trunc(x1z);
  trans_x2z = trunc(x2z);
  trans_x3z = trunc(x3z);
  trans_x4z = trunc(x4z);
  trans_x5z = trunc(x5z);
  
  trans_x2w = trunc(x1w);
  trans_x2w = trunc(x2w);
  trans_x3w = trunc(x3w);
  trans_x4w = trunc(x4w);
  trans_x5w = trunc(x5w);
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
  
  transformed_param_matrix = trunc(d_matrix);
  transformed_param_vector = trunc(d_vector);
  transformed_param_row_vector = trunc(d_row_vector);
  transformed_param_matrix = trunc(p_matrix);
  transformed_param_vector = trunc(p_vector);
  transformed_param_row_vector = trunc(p_row_vector);
  
  trans_p_x3y = trunc(p_x3y);
  trans_p_x4y = trunc(p_x4y);
  trans_p_x5y = trunc(p_x5y);
  
  trans_p_x2z = trunc(p_x2z);
  trans_p_x3z = trunc(p_x3z);
  trans_p_x4z = trunc(p_x4z);
  trans_p_x5z = trunc(p_x5z);
  
  trans_p_x2w = trunc(p_x2w);
  trans_p_x3w = trunc(p_x3w);
  trans_p_x4w = trunc(p_x4w);
  trans_p_x5w = trunc(p_x5w);
}
model {
  y_p ~ normal(0, 1);
}

