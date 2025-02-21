data {
  int d_int;
  vector[d_int] d_vector;
  array[d_int] vector[d_int] d_vector_array;
  array[d_int, 2] vector[d_int] d_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] d_vector_array_3d;

}

parameters {
  real p_real;
  vector[d_int] p_vector;
  array[d_int] vector[d_int] p_vector_array;
  array[d_int, 2] vector[d_int] p_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] p_vector_array_3d;
  real y_p;
}
transformed parameters {
  real transformed_real;

  vector[d_int] transformed_param_vector;
  array[d_int] vector[d_int] transformed_param_vector_array;
  array[d_int, 2] vector[d_int] transformed_param_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] transformed_param_vector_array_3d;

  transformed_param_vector = ordered_constrain(d_vector);
  transformed_param_vector_array = ordered_constrain(d_vector_array);
  transformed_param_vector_array_2d = ordered_constrain(d_vector_array_2d);
  transformed_param_vector_array_3d = ordered_constrain(d_vector_array_3d);

  transformed_param_vector = ordered_constrain(p_vector);
  transformed_param_vector_array = ordered_constrain(p_vector_array);
  transformed_param_vector_array_2d = ordered_constrain(p_vector_array_2d);
  transformed_param_vector_array_3d = ordered_constrain(p_vector_array_3d);
}
model {
  y_p ~ normal(0, 1);
}

