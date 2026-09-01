data {
  int d_int;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  array[d_int] real d_real_array_1d;


}

transformed data {
  int td_int;
  vector[d_int] td_vector;
  row_vector[d_int] td_row_vector;
  array[d_int] int td_int_array_1d;
  array[d_int] real td_real_array_1d;

  td_int_array_1d = poisson_binomial_rng(d_real_array_1d);
  td_int_array_1d = poisson_binomial_rng(d_row_vector);
  td_int_array_1d = poisson_binomial_rng(d_vector);
}

parameters {
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  array[d_int] real p_real_array_1d;


}

generated quantities {
  int gq_int;
  vector[d_int] gq_vector;
  row_vector[d_int] gq_row_vector;
  array[d_int] int gq_int_array_1d;
  array[d_int] real gq_real_array_1d;

  gq_int_array_1d = poisson_binomial_rng(d_real_array_1d);
  gq_int_array_1d = poisson_binomial_rng(d_row_vector);
  gq_int_array_1d = poisson_binomial_rng(d_vector);
  gq_int_array_1d = poisson_binomial_rng(p_real_array_1d);
  gq_int_array_1d = poisson_binomial_rng(p_row_vector);
  gq_int_array_1d = poisson_binomial_rng(p_vector);
}

