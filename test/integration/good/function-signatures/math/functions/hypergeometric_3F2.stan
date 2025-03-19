data {
  int d_int;
  real d_real;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  array[d_int] real d_real_array_1d;


}

transformed data {
  int td_int;
  real td_real;
  vector[d_int] td_vector;
  row_vector[d_int] td_row_vector;
  array[d_int] real td_real_array_1d;

  td_real = hypergeometric_3F2(d_real, d_real, d_real);
  td_real = hypergeometric_3F2(d_real, d_real_array_1d, d_real);
  td_real = hypergeometric_3F2(d_real, d_row_vector, d_real);
  td_real = hypergeometric_3F2(d_real, d_vector, d_real);
  td_real = hypergeometric_3F2(d_real_array_1d, d_real, d_real);
  td_real = hypergeometric_3F2(d_real_array_1d, d_real_array_1d, d_real);
  td_real = hypergeometric_3F2(d_real_array_1d, d_row_vector, d_real);
  td_real = hypergeometric_3F2(d_real_array_1d, d_vector, d_real);
  td_real = hypergeometric_3F2(d_row_vector, d_real, d_real);
  td_real = hypergeometric_3F2(d_row_vector, d_real_array_1d, d_real);
  td_real = hypergeometric_3F2(d_row_vector, d_row_vector, d_real);
  td_real = hypergeometric_3F2(d_row_vector, d_vector, d_real);
  td_real = hypergeometric_3F2(d_vector, d_real, d_real);
  td_real = hypergeometric_3F2(d_vector, d_real_array_1d, d_real);
  td_real = hypergeometric_3F2(d_vector, d_row_vector, d_real);
  td_real = hypergeometric_3F2(d_vector, d_vector, d_real);
}

parameters {
  real p_real;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  array[d_int] real p_real_array_1d;


}

transformed parameters {
  real transformed_param_real;
  vector[d_int] transformed_param_vector;
  row_vector[d_int] transformed_param_row_vector;
  array[d_int] real transformed_param_real_array_1d;

  transformed_param_real = hypergeometric_3F2(d_real, d_real, d_real);
  transformed_param_real = hypergeometric_3F2(d_real, d_real, p_real);
  transformed_param_real = hypergeometric_3F2(d_real, d_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(d_real, d_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(d_real, d_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_real, d_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_real, d_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_real, d_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_real, p_real, d_real);
  transformed_param_real = hypergeometric_3F2(d_real, p_real, p_real);
  transformed_param_real = hypergeometric_3F2(d_real, p_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(d_real, p_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(d_real, p_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_real, p_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_real, p_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_real, p_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, d_real, d_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, d_real, p_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, d_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, d_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, d_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, d_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, d_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, d_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, p_real, d_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, p_real, p_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, p_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, p_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, p_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, p_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, p_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_real_array_1d, p_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, d_real, d_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, d_real, p_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, d_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, d_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, d_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, d_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, d_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, d_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, p_real, d_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, p_real, p_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, p_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, p_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, p_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, p_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, p_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_row_vector, p_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_vector, d_real, d_real);
  transformed_param_real = hypergeometric_3F2(d_vector, d_real, p_real);
  transformed_param_real = hypergeometric_3F2(d_vector, d_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(d_vector, d_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(d_vector, d_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_vector, d_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_vector, d_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_vector, d_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_vector, p_real, d_real);
  transformed_param_real = hypergeometric_3F2(d_vector, p_real, p_real);
  transformed_param_real = hypergeometric_3F2(d_vector, p_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(d_vector, p_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(d_vector, p_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_vector, p_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(d_vector, p_vector, d_real);
  transformed_param_real = hypergeometric_3F2(d_vector, p_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_real, d_real, d_real);
  transformed_param_real = hypergeometric_3F2(p_real, d_real, p_real);
  transformed_param_real = hypergeometric_3F2(p_real, d_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(p_real, d_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(p_real, d_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_real, d_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_real, d_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_real, d_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_real, p_real, d_real);
  transformed_param_real = hypergeometric_3F2(p_real, p_real, p_real);
  transformed_param_real = hypergeometric_3F2(p_real, p_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(p_real, p_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(p_real, p_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_real, p_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_real, p_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_real, p_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, d_real, d_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, d_real, p_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, d_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, d_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, d_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, d_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, d_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, d_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, p_real, d_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, p_real, p_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, p_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, p_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, p_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, p_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, p_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_real_array_1d, p_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, d_real, d_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, d_real, p_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, d_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, d_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, d_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, d_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, d_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, d_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, p_real, d_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, p_real, p_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, p_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, p_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, p_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, p_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, p_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_row_vector, p_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_vector, d_real, d_real);
  transformed_param_real = hypergeometric_3F2(p_vector, d_real, p_real);
  transformed_param_real = hypergeometric_3F2(p_vector, d_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(p_vector, d_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(p_vector, d_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_vector, d_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_vector, d_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_vector, d_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_vector, p_real, d_real);
  transformed_param_real = hypergeometric_3F2(p_vector, p_real, p_real);
  transformed_param_real = hypergeometric_3F2(p_vector, p_real_array_1d, d_real);
  transformed_param_real = hypergeometric_3F2(p_vector, p_real_array_1d, p_real);
  transformed_param_real = hypergeometric_3F2(p_vector, p_row_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_vector, p_row_vector, p_real);
  transformed_param_real = hypergeometric_3F2(p_vector, p_vector, d_real);
  transformed_param_real = hypergeometric_3F2(p_vector, p_vector, p_real);
}

