data {
  int d_int;
  vector[d_int+1] d_vector;
  array[d_int] vector[d_int+1] d_vector_array_1d;
  array[d_int, 2] vector[d_int+1] d_vector_array_2d;
  array[d_int, 2, 3] vector[d_int+1] d_vector_array_3d;


}

transformed data {
  int td_int;
  vector[d_int] td_vector;
  array[d_int] vector[d_int] td_vector_array_1d;
  array[d_int, 2] vector[d_int] td_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] td_vector_array_3d;

  td_vector = sum_to_zero_unconstrain(d_vector);
  td_vector_array_1d = sum_to_zero_unconstrain(d_vector_array_1d);
  td_vector_array_2d = sum_to_zero_unconstrain(d_vector_array_2d);
  td_vector_array_3d = sum_to_zero_unconstrain(d_vector_array_3d);
}

parameters {
  vector[d_int+1] p_vector;
  array[d_int] vector[d_int+1] p_vector_array_1d;
  array[d_int, 2] vector[d_int+1] p_vector_array_2d;
  array[d_int, 2, 3] vector[d_int+1] p_vector_array_3d;


}

transformed parameters {
  vector[d_int] transformed_param_vector;
  array[d_int] vector[d_int] transformed_param_vector_array_1d;
  array[d_int, 2] vector[d_int] transformed_param_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] transformed_param_vector_array_3d;

  transformed_param_vector = sum_to_zero_unconstrain(d_vector);
  transformed_param_vector = sum_to_zero_unconstrain(p_vector);
  transformed_param_vector_array_1d = sum_to_zero_unconstrain(d_vector_array_1d);
  transformed_param_vector_array_1d = sum_to_zero_unconstrain(p_vector_array_1d);
  transformed_param_vector_array_2d = sum_to_zero_unconstrain(d_vector_array_2d);
  transformed_param_vector_array_2d = sum_to_zero_unconstrain(p_vector_array_2d);
  transformed_param_vector_array_3d = sum_to_zero_unconstrain(d_vector_array_3d);
  transformed_param_vector_array_3d = sum_to_zero_unconstrain(p_vector_array_3d);
}

