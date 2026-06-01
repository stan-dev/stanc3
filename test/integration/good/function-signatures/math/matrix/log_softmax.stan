data {
  int d_int;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
  array[d_int] vector[d_int] d_vector_array_1d;
  array[d_int] row_vector[d_int] d_row_vector_array_1d;


}

transformed data {
  int td_int;
  vector[d_int] td_vector;
  row_vector[d_int] td_row_vector;
  array[d_int] vector[d_int] td_vector_array_1d;
  array[d_int] row_vector[d_int] td_row_vector_array_1d;

  td_row_vector = log_softmax(d_row_vector);
  td_row_vector_array_1d = log_softmax(d_row_vector_array_1d);
  td_vector = log_softmax(d_vector);
  td_vector_array_1d = log_softmax(d_vector_array_1d);
}

parameters {
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  array[d_int] vector[d_int] p_vector_array_1d;
  array[d_int] row_vector[d_int] p_row_vector_array_1d;


}

transformed parameters {
  vector[d_int] transformed_param_vector;
  row_vector[d_int] transformed_param_row_vector;
  array[d_int] vector[d_int] transformed_param_vector_array_1d;
  array[d_int] row_vector[d_int] transformed_param_row_vector_array_1d;

  transformed_param_row_vector = log_softmax(d_row_vector);
  transformed_param_row_vector = log_softmax(p_row_vector);
  transformed_param_row_vector_array_1d = log_softmax(d_row_vector_array_1d);
  transformed_param_row_vector_array_1d = log_softmax(p_row_vector_array_1d);
  transformed_param_vector = log_softmax(d_vector);
  transformed_param_vector = log_softmax(p_vector);
  transformed_param_vector_array_1d = log_softmax(d_vector_array_1d);
  transformed_param_vector_array_1d = log_softmax(p_vector_array_1d);
}

