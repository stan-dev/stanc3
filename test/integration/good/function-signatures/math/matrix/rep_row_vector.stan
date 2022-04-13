data {
  int d_int;
  real d_real;
  complex d_complex;
}

transformed data {
  row_vector[d_int] transformed_data_row_vector;

  transformed_data_row_vector = rep_row_vector(d_real, d_int);

  complex_row_vector[d_int] transformed_data_crow_vector;
  transformed_data_crow_vector = rep_row_vector(d_complex, d_int);
}
parameters {
  real p_real;
  real y_p;
  complex p_complex;
}
transformed parameters {
  row_vector[d_int] transformed_param_row_vector;

  transformed_param_row_vector = rep_row_vector(d_real, d_int);
  transformed_param_row_vector = rep_row_vector(p_real, d_int);

  complex_row_vector[d_int] transformed_param_crow_vector;
  transformed_param_crow_vector = rep_row_vector(d_complex, d_int);
  transformed_param_crow_vector = rep_row_vector(p_complex, d_int);
}
model {
  y_p ~ normal(0,1);
}
