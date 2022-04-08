data {
  int d_int;
  real d_real;
  complex d_complex;
}

transformed data {
  vector[d_int] transformed_data_vector;

  transformed_data_vector = rep_vector(d_real, d_int);

  complex_vector[d_int] transformed_data_cvector;
  transformed_data_cvector = rep_vector(d_complex, d_int);
}
parameters {
  real p_real;
  real y_p;
  complex p_complex;
}
transformed parameters {
  vector[d_int] transformed_param_vector;

  transformed_param_vector = rep_vector(d_real, d_int);
  transformed_param_vector = rep_vector(p_real, d_int);

  complex_vector[d_int] transformed_param_cvector;
  transformed_param_cvector = rep_vector(d_complex, d_int);
  transformed_param_cvector = rep_vector(p_complex, d_int);
}
model {
  y_p ~ normal(0,1);
}
