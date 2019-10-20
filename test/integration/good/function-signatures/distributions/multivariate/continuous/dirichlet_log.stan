data { 
  int d_int;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  vector[d_int] d_vector_arr[d_int];
  row_vector[d_int] d_row_vector_arr[d_int];
}
transformed data {
  real transformed_data_real;

  transformed_data_real = dirichlet_log(d_vector, d_vector);
  transformed_data_real = dirichlet_log(d_vector, d_row_vector);
  transformed_data_real = dirichlet_log(d_row_vector, d_vector);
  transformed_data_real = dirichlet_log(d_row_vector, d_row_vector);

  transformed_data_real = dirichlet_log(d_vector, d_vector_arr);
  transformed_data_real = dirichlet_log(d_vector, d_row_vector_arr);
  transformed_data_real = dirichlet_log(d_row_vector, d_vector_arr);
  transformed_data_real = dirichlet_log(d_row_vector, d_row_vector_arr);

  transformed_data_real = dirichlet_log(d_vector_arr, d_vector);
  transformed_data_real = dirichlet_log(d_vector_arr, d_row_vector);
  transformed_data_real = dirichlet_log(d_row_vector_arr, d_vector);
  transformed_data_real = dirichlet_log(d_row_vector_arr, d_row_vector);

  transformed_data_real = dirichlet_log(d_vector_arr, d_vector_arr);
  transformed_data_real = dirichlet_log(d_vector_arr, d_row_vector_arr);
  transformed_data_real = dirichlet_log(d_row_vector_arr, d_vector_arr);
  transformed_data_real = dirichlet_log(d_row_vector_arr, d_row_vector_arr);
}
parameters {
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  vector[d_int] p_vector_arr[d_int];
  row_vector[d_int] p_row_vector_arr[d_int];

  real y_p;
}
transformed parameters {
  real transformed_param_real;

  transformed_param_real = dirichlet_log(d_vector, d_vector);
  transformed_param_real = dirichlet_log(d_vector, d_vector_arr);
  transformed_param_real = dirichlet_log(d_vector_arr, d_vector);
  transformed_param_real = dirichlet_log(d_vector_arr, d_vector_arr);

  transformed_param_real = dirichlet_log(d_vector, d_row_vector);
  transformed_param_real = dirichlet_log(d_vector, d_row_vector_arr);
  transformed_param_real = dirichlet_log(d_vector_arr, d_row_vector);
  transformed_param_real = dirichlet_log(d_vector_arr, d_row_vector_arr);

  transformed_param_real = dirichlet_log(d_row_vector, d_vector);
  transformed_param_real = dirichlet_log(d_row_vector, d_vector_arr);
  transformed_param_real = dirichlet_log(d_row_vector_arr, d_vector);
  transformed_param_real = dirichlet_log(d_row_vector_arr, d_vector_arr);

  transformed_param_real = dirichlet_log(d_row_vector, d_row_vector);
  transformed_param_real = dirichlet_log(d_row_vector, d_row_vector_arr);
  transformed_param_real = dirichlet_log(d_row_vector_arr, d_row_vector);
  transformed_param_real = dirichlet_log(d_row_vector_arr, d_row_vector_arr);

  transformed_param_real = dirichlet_log(d_vector, p_vector);
  transformed_param_real = dirichlet_log(d_vector, p_vector_arr);
  transformed_param_real = dirichlet_log(d_vector_arr, p_vector);
  transformed_param_real = dirichlet_log(d_vector_arr, p_vector_arr);

  transformed_param_real = dirichlet_log(d_vector, p_row_vector);
  transformed_param_real = dirichlet_log(d_vector, p_row_vector_arr);
  transformed_param_real = dirichlet_log(d_vector_arr, p_row_vector);
  transformed_param_real = dirichlet_log(d_vector_arr, p_row_vector_arr);

  transformed_param_real = dirichlet_log(d_row_vector, p_vector);
  transformed_param_real = dirichlet_log(d_row_vector, p_vector_arr);
  transformed_param_real = dirichlet_log(d_row_vector_arr, p_vector);
  transformed_param_real = dirichlet_log(d_row_vector_arr, p_vector_arr);

  transformed_param_real = dirichlet_log(d_row_vector, p_row_vector);
  transformed_param_real = dirichlet_log(d_row_vector, p_row_vector_arr);
  transformed_param_real = dirichlet_log(d_row_vector_arr, p_row_vector);
  transformed_param_real = dirichlet_log(d_row_vector_arr, p_row_vector_arr);

  transformed_param_real = dirichlet_log(p_vector, d_vector);
  transformed_param_real = dirichlet_log(p_vector, d_vector_arr);
  transformed_param_real = dirichlet_log(p_vector_arr, d_vector);
  transformed_param_real = dirichlet_log(p_vector_arr, d_vector_arr);

  transformed_param_real = dirichlet_log(p_vector, d_row_vector);
  transformed_param_real = dirichlet_log(p_vector, d_row_vector_arr);
  transformed_param_real = dirichlet_log(p_vector_arr, d_row_vector);
  transformed_param_real = dirichlet_log(p_vector_arr, d_row_vector_arr);

  transformed_param_real = dirichlet_log(p_row_vector, d_vector);
  transformed_param_real = dirichlet_log(p_row_vector, d_vector_arr);
  transformed_param_real = dirichlet_log(p_row_vector_arr, d_vector);
  transformed_param_real = dirichlet_log(p_row_vector_arr, d_vector_arr);

  transformed_param_real = dirichlet_log(p_row_vector, d_row_vector);
  transformed_param_real = dirichlet_log(p_row_vector, d_row_vector_arr);
  transformed_param_real = dirichlet_log(p_row_vector_arr, d_row_vector);
  transformed_param_real = dirichlet_log(p_row_vector_arr, d_row_vector_arr);

  transformed_param_real = dirichlet_log(p_vector, p_vector);
  transformed_param_real = dirichlet_log(p_vector, p_vector_arr);
  transformed_param_real = dirichlet_log(p_vector_arr, p_vector);
  transformed_param_real = dirichlet_log(p_vector_arr, p_vector_arr);

  transformed_param_real = dirichlet_log(p_vector, p_row_vector);
  transformed_param_real = dirichlet_log(p_vector, p_row_vector_arr);
  transformed_param_real = dirichlet_log(p_vector_arr, p_row_vector);
  transformed_param_real = dirichlet_log(p_vector_arr, p_row_vector_arr);

  transformed_param_real = dirichlet_log(p_row_vector, p_vector);
  transformed_param_real = dirichlet_log(p_row_vector, p_vector_arr);
  transformed_param_real = dirichlet_log(p_row_vector_arr, p_vector);
  transformed_param_real = dirichlet_log(p_row_vector_arr, p_vector_arr);
  
  transformed_param_real = dirichlet_log(p_row_vector, p_row_vector);
  transformed_param_real = dirichlet_log(p_row_vector, p_row_vector_arr);
  transformed_param_real = dirichlet_log(p_row_vector_arr, p_row_vector);
  transformed_param_real = dirichlet_log(p_row_vector_arr, p_row_vector_arr);
}
model {  
  y_p ~ normal(0, 1);
}
