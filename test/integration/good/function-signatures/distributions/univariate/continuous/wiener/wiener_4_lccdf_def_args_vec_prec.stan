data {
  int N;
  real d_real;
  vector[N] d_vec;
}
transformed data {
  real transformed_data_real;
  transformed_data_real = wiener_lccdf_defective(d_vec, d_vec, d_vec, d_vec, d_vec, d_real);
}
parameters {
  vector[N] p_vec;
}
transformed parameters {
  real transformed_param_real;
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, p_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, d_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, p_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, p_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, p_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, d_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, p_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, d_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, d_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, d_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, d_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, d_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, d_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, d_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, p_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, p_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, d_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, p_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, d_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, d_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, d_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, p_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, d_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, p_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, p_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, d_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, d_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, p_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, p_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, d_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, p_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, d_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, p_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, p_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, p_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, d_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, p_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, d_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, p_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, p_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, d_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, p_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, d_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, p_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, d_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, p_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, p_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, d_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, d_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, d_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, p_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, d_vec, p_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(p_vec, p_vec, p_vec, p_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, p_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, p_vec, d_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, d_vec, d_vec, d_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, d_vec, p_vec, p_vec, p_vec, d_real);
  transformed_param_real = wiener_lccdf_defective(d_vec, p_vec, d_vec, p_vec, d_vec, d_real);
}
model {
  p_vec ~ normal(0, 1);
}
