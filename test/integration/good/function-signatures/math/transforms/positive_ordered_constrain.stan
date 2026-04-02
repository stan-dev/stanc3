data {
  int d_int;
  vector[d_int] d_vector;
  array[d_int] vector[d_int] d_vector_array_1d;
  array[d_int, 2] vector[d_int] d_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] d_vector_array_3d;


}

transformed data {
  int td_int;
  vector[d_int] td_vector;
  array[d_int] vector[d_int] td_vector_array_1d;
  array[d_int, 2] vector[d_int] td_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] td_vector_array_3d;

  td_vector = positive_ordered_constrain(d_vector);
  td_vector_array_1d = positive_ordered_constrain(d_vector_array_1d);
  td_vector_array_2d = positive_ordered_constrain(d_vector_array_2d);
  td_vector_array_3d = positive_ordered_constrain(d_vector_array_3d);
}

parameters {
  vector[d_int] p_vector;
  array[d_int] vector[d_int] p_vector_array_1d;
  array[d_int, 2] vector[d_int] p_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] p_vector_array_3d;


}

transformed parameters {
  vector[d_int] transformed_param_vector;
  array[d_int] vector[d_int] transformed_param_vector_array_1d;
  array[d_int, 2] vector[d_int] transformed_param_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] transformed_param_vector_array_3d;

  transformed_param_vector = positive_ordered_constrain(d_vector);
  transformed_param_vector = positive_ordered_constrain(p_vector);
  transformed_param_vector_array_1d = positive_ordered_constrain(d_vector_array_1d);
  transformed_param_vector_array_1d = positive_ordered_constrain(p_vector_array_1d);
  transformed_param_vector_array_2d = positive_ordered_constrain(d_vector_array_2d);
  transformed_param_vector_array_2d = positive_ordered_constrain(p_vector_array_2d);
  transformed_param_vector_array_3d = positive_ordered_constrain(d_vector_array_3d);
  transformed_param_vector_array_3d = positive_ordered_constrain(p_vector_array_3d);
}

