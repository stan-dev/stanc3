data {
  int d_int;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  array[d_int] int d_int_array_1d;
  array[d_int] real d_real_array_1d;


}

transformed data {
  int td_int;
  real td_real;
  vector[d_int] td_vector;
  row_vector[d_int] td_row_vector;
  array[d_int] int td_int_array_1d;
  array[d_int] real td_real_array_1d;

  td_real = poisson_binomial_lpmf(d_int | d_real_array_1d);
  td_real = poisson_binomial_lpmf(d_int | d_row_vector);
  td_real = poisson_binomial_lpmf(d_int | d_vector);
  td_real = poisson_binomial_lpmf(d_int_array_1d | d_real_array_1d);
  td_real = poisson_binomial_lpmf(d_int_array_1d | d_row_vector);
  td_real = poisson_binomial_lpmf(d_int_array_1d | d_vector);
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

  transformed_param_real = poisson_binomial_lpmf(d_int | d_real_array_1d);
  transformed_param_real = poisson_binomial_lpmf(d_int | d_row_vector);
  transformed_param_real = poisson_binomial_lpmf(d_int | d_vector);
  transformed_param_real = poisson_binomial_lpmf(d_int | p_real_array_1d);
  transformed_param_real = poisson_binomial_lpmf(d_int | p_row_vector);
  transformed_param_real = poisson_binomial_lpmf(d_int | p_vector);
  transformed_param_real = poisson_binomial_lpmf(d_int_array_1d | d_real_array_1d);
  transformed_param_real = poisson_binomial_lpmf(d_int_array_1d | d_row_vector);
  transformed_param_real = poisson_binomial_lpmf(d_int_array_1d | d_vector);
  transformed_param_real = poisson_binomial_lpmf(d_int_array_1d | p_real_array_1d);
  transformed_param_real = poisson_binomial_lpmf(d_int_array_1d | p_row_vector);
  transformed_param_real = poisson_binomial_lpmf(d_int_array_1d | p_vector);
}

