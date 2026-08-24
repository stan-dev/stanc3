data {
  int d_int;
  real d_real;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  array[d_int] int d_int_array_1d;
  array[d_int] real d_real_array_1d;


}

transformed data {
  real td_real;


  td_real = yule_simon_lccdf(d_int| d_real);
  td_real = yule_simon_lccdf(d_int| d_real_array_1d);
  td_real = yule_simon_lccdf(d_int| d_row_vector);
  td_real = yule_simon_lccdf(d_int| d_vector);
  td_real = yule_simon_lccdf(d_int_array_1d| d_real);
  td_real = yule_simon_lccdf(d_int_array_1d| d_real_array_1d);
  td_real = yule_simon_lccdf(d_int_array_1d| d_row_vector);
  td_real = yule_simon_lccdf(d_int_array_1d| d_vector);
}

parameters {
  real p_real;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  array[d_int] real p_real_array_1d;


}

transformed parameters {
  real transformed_param_real;

  transformed_param_real = yule_simon_lccdf(d_int| d_real);
  transformed_param_real = yule_simon_lccdf(d_int| d_real_array_1d);
  transformed_param_real = yule_simon_lccdf(d_int| d_row_vector);
  transformed_param_real = yule_simon_lccdf(d_int| d_vector);
  transformed_param_real = yule_simon_lccdf(d_int| p_real);
  transformed_param_real = yule_simon_lccdf(d_int| p_real_array_1d);
  transformed_param_real = yule_simon_lccdf(d_int| p_row_vector);
  transformed_param_real = yule_simon_lccdf(d_int| p_vector);
  transformed_param_real = yule_simon_lccdf(d_int_array_1d| d_real);
  transformed_param_real = yule_simon_lccdf(d_int_array_1d| d_real_array_1d);
  transformed_param_real = yule_simon_lccdf(d_int_array_1d| d_row_vector);
  transformed_param_real = yule_simon_lccdf(d_int_array_1d| d_vector);
  transformed_param_real = yule_simon_lccdf(d_int_array_1d| p_real);
  transformed_param_real = yule_simon_lccdf(d_int_array_1d| p_real_array_1d);
  transformed_param_real = yule_simon_lccdf(d_int_array_1d| p_row_vector);
  transformed_param_real = yule_simon_lccdf(d_int_array_1d| p_vector);
}

