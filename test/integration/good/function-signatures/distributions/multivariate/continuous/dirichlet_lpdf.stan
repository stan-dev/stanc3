data {
  int d_int;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  array[d_int] vector[d_int] d_vector_arr;
  array[d_int] row_vector[d_int] d_row_vector_arr;
}
transformed data {
  real transformed_data_real;
  transformed_data_real = dirichlet_lpdf(d_vector | d_vector);
  transformed_data_real = dirichlet_lpdf(d_vector | d_row_vector);
  transformed_data_real = dirichlet_lpdf(d_row_vector | d_vector);
  transformed_data_real = dirichlet_lpdf(d_row_vector | d_row_vector);
  transformed_data_real = dirichlet_lpdf(d_vector | d_vector_arr);
  transformed_data_real = dirichlet_lpdf(d_vector | d_row_vector_arr);
  transformed_data_real = dirichlet_lpdf(d_row_vector | d_vector_arr);
  transformed_data_real = dirichlet_lpdf(d_row_vector | d_row_vector_arr);
  transformed_data_real = dirichlet_lpdf(d_vector_arr | d_vector);
  transformed_data_real = dirichlet_lpdf(d_vector_arr | d_row_vector);
  transformed_data_real = dirichlet_lpdf(d_row_vector_arr | d_vector);
  transformed_data_real = dirichlet_lpdf(d_row_vector_arr | d_row_vector);
  transformed_data_real = dirichlet_lpdf(d_vector_arr | d_vector_arr);
  transformed_data_real = dirichlet_lpdf(d_vector_arr | d_row_vector_arr);
  transformed_data_real = dirichlet_lpdf(d_row_vector_arr | d_vector_arr);
  transformed_data_real = dirichlet_lpdf(d_row_vector_arr | d_row_vector_arr);
}
parameters {
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  array[d_int] vector[d_int] p_vector_arr;
  array[d_int] row_vector[d_int] p_row_vector_arr;
  real y_p;
}
transformed parameters {
  real transformed_param_real;
  transformed_param_real = dirichlet_lpdf(d_vector | d_vector);
  transformed_param_real = dirichlet_lpdf(d_vector | d_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_vector_arr | d_vector);
  transformed_param_real = dirichlet_lpdf(d_vector_arr | d_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_vector | d_row_vector);
  transformed_param_real = dirichlet_lpdf(d_vector | d_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_vector_arr | d_row_vector);
  transformed_param_real = dirichlet_lpdf(d_vector_arr | d_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_row_vector | d_vector);
  transformed_param_real = dirichlet_lpdf(d_row_vector | d_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_row_vector_arr | d_vector);
  transformed_param_real = dirichlet_lpdf(d_row_vector_arr | d_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_row_vector | d_row_vector);
  transformed_param_real = dirichlet_lpdf(d_row_vector | d_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_row_vector_arr | d_row_vector);
  transformed_param_real = dirichlet_lpdf(d_row_vector_arr | d_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_vector | p_vector);
  transformed_param_real = dirichlet_lpdf(d_vector | p_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_vector_arr | p_vector);
  transformed_param_real = dirichlet_lpdf(d_vector_arr | p_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_vector | p_row_vector);
  transformed_param_real = dirichlet_lpdf(d_vector | p_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_vector_arr | p_row_vector);
  transformed_param_real = dirichlet_lpdf(d_vector_arr | p_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_row_vector | p_vector);
  transformed_param_real = dirichlet_lpdf(d_row_vector | p_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_row_vector_arr | p_vector);
  transformed_param_real = dirichlet_lpdf(d_row_vector_arr | p_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_row_vector | p_row_vector);
  transformed_param_real = dirichlet_lpdf(d_row_vector | p_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(d_row_vector_arr | p_row_vector);
  transformed_param_real = dirichlet_lpdf(d_row_vector_arr | p_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_vector | d_vector);
  transformed_param_real = dirichlet_lpdf(p_vector | d_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_vector_arr | d_vector);
  transformed_param_real = dirichlet_lpdf(p_vector_arr | d_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_vector | d_row_vector);
  transformed_param_real = dirichlet_lpdf(p_vector | d_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_vector_arr | d_row_vector);
  transformed_param_real = dirichlet_lpdf(p_vector_arr | d_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_row_vector | d_vector);
  transformed_param_real = dirichlet_lpdf(p_row_vector | d_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_row_vector_arr | d_vector);
  transformed_param_real = dirichlet_lpdf(p_row_vector_arr | d_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_row_vector | d_row_vector);
  transformed_param_real = dirichlet_lpdf(p_row_vector | d_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_row_vector_arr | d_row_vector);
  transformed_param_real = dirichlet_lpdf(p_row_vector_arr | d_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_vector | p_vector);
  transformed_param_real = dirichlet_lpdf(p_vector | p_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_vector_arr | p_vector);
  transformed_param_real = dirichlet_lpdf(p_vector_arr | p_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_vector | p_row_vector);
  transformed_param_real = dirichlet_lpdf(p_vector | p_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_vector_arr | p_row_vector);
  transformed_param_real = dirichlet_lpdf(p_vector_arr | p_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_row_vector | p_vector);
  transformed_param_real = dirichlet_lpdf(p_row_vector | p_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_row_vector_arr | p_vector);
  transformed_param_real = dirichlet_lpdf(p_row_vector_arr | p_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_row_vector | p_row_vector);
  transformed_param_real = dirichlet_lpdf(p_row_vector | p_row_vector_arr);
  transformed_param_real = dirichlet_lpdf(p_row_vector_arr | p_row_vector);
  transformed_param_real = dirichlet_lpdf(p_row_vector_arr | p_row_vector_arr);
}
model {
  y_p ~ normal(0, 1);
}

