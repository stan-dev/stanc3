data {
  int d_int;
  real d_real;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  array[d_int] int d_int_array_1d;
  array[d_int] real d_real_array_1d;


}

transformed data {
  int td_int;
  array[d_int] int td_int_array_1d;

  td_int = yule_simon_rng(d_int);
  td_int = yule_simon_rng(d_real);
  td_int_array_1d = yule_simon_rng(d_int_array_1d);
  td_int_array_1d = yule_simon_rng(d_real_array_1d);
  td_int_array_1d = yule_simon_rng(d_row_vector);
  td_int_array_1d = yule_simon_rng(d_vector);
}

parameters {
  real p_real;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  array[d_int] real p_real_array_1d;


}

generated quantities {
  int gq_int;
  array[d_int] int gq_int_array_1d;

  gq_int = yule_simon_rng(d_int);
  gq_int = yule_simon_rng(d_real);
  gq_int = yule_simon_rng(p_real);
  gq_int_array_1d = yule_simon_rng(d_int_array_1d);
  gq_int_array_1d = yule_simon_rng(d_real_array_1d);
  gq_int_array_1d = yule_simon_rng(d_row_vector);
  gq_int_array_1d = yule_simon_rng(d_vector);
  gq_int_array_1d = yule_simon_rng(p_real_array_1d);
  gq_int_array_1d = yule_simon_rng(p_row_vector);
  gq_int_array_1d = yule_simon_rng(p_vector);
}

