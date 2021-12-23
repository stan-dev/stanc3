data {
  int d_int;
  array[d_int] int d_int_array;
  array[d_int] real d_real_array;
  matrix[d_int, d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
}
transformed data {
  real transformed_data_real;
  
  transformed_data_real = trace_quad_form(d_matrix, d_vector);
  transformed_data_real = trace_quad_form(d_matrix, d_matrix);
}
parameters {
  real p_real;
  real y_p;
  array[d_int] real p_real_array;
  matrix[d_int, d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
}
transformed parameters {
  real transformed_param_real;
  
  transformed_param_real = trace_quad_form(d_matrix, d_vector);
  transformed_param_real = trace_quad_form(d_matrix, d_matrix);
  
  transformed_param_real = trace_quad_form(p_matrix, d_vector);
  transformed_param_real = trace_quad_form(p_matrix, d_matrix);
  
  transformed_param_real = trace_quad_form(d_matrix, p_vector);
  transformed_param_real = trace_quad_form(d_matrix, p_matrix);
  
  transformed_param_real = trace_quad_form(p_matrix, p_vector);
  transformed_param_real = trace_quad_form(p_matrix, p_matrix);
}
model {
  y_p ~ normal(0, 1);
}

