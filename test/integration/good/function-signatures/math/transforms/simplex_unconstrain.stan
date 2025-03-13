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

  td_vector = simplex_unconstrain(d_vector);
  td_vector_array_1d = simplex_unconstrain(d_vector_array_1d);
  td_vector_array_2d = simplex_unconstrain(d_vector_array_2d);
  td_vector_array_3d = simplex_unconstrain(d_vector_array_3d);
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

  transformed_param_vector = simplex_unconstrain(d_vector);
  transformed_param_vector = simplex_unconstrain(p_vector);
  transformed_param_vector_array_1d = simplex_unconstrain(d_vector_array_1d);
  transformed_param_vector_array_1d = simplex_unconstrain(p_vector_array_1d);
  transformed_param_vector_array_2d = simplex_unconstrain(d_vector_array_2d);
  transformed_param_vector_array_2d = simplex_unconstrain(p_vector_array_2d);
  transformed_param_vector_array_3d = simplex_unconstrain(d_vector_array_3d);
  transformed_param_vector_array_3d = simplex_unconstrain(p_vector_array_3d);
}

