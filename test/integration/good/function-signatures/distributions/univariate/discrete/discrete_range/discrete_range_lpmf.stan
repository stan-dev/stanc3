data {
  int d_int;
  array[d_int] int d_int_array;
}
transformed data {
  real transformed_data_real;
  transformed_data_real = discrete_range_lpmf(d_int | d_int, d_int);
  transformed_data_real = discrete_range_lpmf(d_int | d_int, d_int_array);
  transformed_data_real = discrete_range_lpmf(d_int | d_int_array, d_int);
  transformed_data_real = discrete_range_lpmf(d_int | d_int_array, d_int_array);
  transformed_data_real = discrete_range_lpmf(d_int_array | d_int, d_int);
  transformed_data_real = discrete_range_lpmf(d_int_array | d_int, d_int_array);
  transformed_data_real = discrete_range_lpmf(d_int_array | d_int_array, d_int);
  transformed_data_real = discrete_range_lpmf(d_int_array | d_int_array, d_int_array);
}
parameters {
  real y_p;
}
transformed parameters {
  real transformed_param_real;
  transformed_param_real = discrete_range_lpmf(d_int | d_int, d_int);
  transformed_param_real = discrete_range_lpmf(d_int | d_int, d_int_array);
  transformed_param_real = discrete_range_lpmf(d_int | d_int_array, d_int);
  transformed_param_real = discrete_range_lpmf(d_int | d_int_array, d_int_array);
  transformed_param_real = discrete_range_lpmf(d_int_array | d_int, d_int);
  transformed_param_real = discrete_range_lpmf(d_int_array | d_int, d_int_array);
  transformed_param_real = discrete_range_lpmf(d_int_array | d_int_array, d_int);
  transformed_param_real = discrete_range_lpmf(d_int_array | d_int_array, d_int_array);
}
model {
  y_p ~ normal(0, 1);
}

