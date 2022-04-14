data {
  int d_int;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;

  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;

}
transformed data {
  real transformed_data_real;

  transformed_data_real = dot_self(d_vector);
  transformed_data_real = dot_self(d_row_vector);

  complex transformed_data_complex;
  transformed_data_complex = dot_self(d_cvector);
  transformed_data_complex = dot_self(d_crow_vector);
}
parameters {
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;

  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;
  real y_p;
}
transformed parameters {
  real transformed_param_real;

  transformed_param_real = dot_self(d_vector);
  transformed_param_real = dot_self(d_row_vector);
  transformed_param_real = dot_self(p_vector);
  transformed_param_real = dot_self(p_row_vector);

  complex transformed_param_complex;
  transformed_param_complex = dot_self(d_cvector);
  transformed_param_complex = dot_self(d_crow_vector);
  transformed_param_complex = dot_self(p_cvector);
  transformed_param_complex = dot_self(p_crow_vector);
}
model {
  y_p ~ normal(0,1);
}
