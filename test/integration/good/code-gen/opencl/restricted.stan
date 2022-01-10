data {
  int d_int;
  array[d_int] int d_int_array;
  real d_real;
  array[d_int] real d_real_array;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
}
parameters {
  real p_real;
  array[d_int] real p_real_array;
  matrix[d_int, d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  real y_p;
}
transformed parameters {
  real transformed_param_real;
  
  // signatures that should not move to OpenCL
  transformed_param_real = bernoulli_lpmf(d_int_array | d_real);
  transformed_param_real = bernoulli_lpmf(d_int_array | p_real);
  transformed_param_real = exponential_lpdf(p_vector | d_real);
  transformed_param_real = exponential_lpdf(p_vector | p_real);
  transformed_param_real = std_normal_lpdf(p_vector | );
  transformed_param_real = uniform_lpdf(p_vector | d_real, d_real);
  transformed_param_real = uniform_lpdf(p_vector | d_real, p_real);
  transformed_param_real = uniform_lpdf(p_vector | p_real, d_real);
  transformed_param_real = uniform_lpdf(p_vector | p_real, p_real);
  
  // signature of the same functions that can be moved to OpenCL - sanity check
  transformed_param_real = exponential_lpdf(d_vector | d_real);
  transformed_param_real = exponential_lpdf(d_vector | p_real);
  transformed_param_real = std_normal_lpdf(d_vector | );
  transformed_param_real = uniform_lpdf(d_vector | d_real, d_real);
  transformed_param_real = uniform_lpdf(d_vector | d_real, p_real);
  transformed_param_real = uniform_lpdf(d_vector | p_real, d_real);
  transformed_param_real = uniform_lpdf(d_vector | p_real, p_real);
}
model {
  y_p ~ normal(0, 1);
}

