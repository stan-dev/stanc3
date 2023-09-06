data {
  int d_int;
  array[d_int] int d_int_array;
}
parameters {
  vector[d_int] p_vector;
}
transformed parameters {
  real transformed_param_real;
  transformed_param_real = bernoulli_logit_lpmf(d_int| p_vector);
  transformed_param_real = bernoulli_logit_lpmf(d_int_array| p_vector);
}