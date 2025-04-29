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

  vector[d_int+1] transformed_param_vector;
  array[d_int] vector[d_int+1] transformed_param_vector_array;
  array[d_int, 2] vector[d_int+1] transformed_param_vector_array_2d;
  array[d_int, 2, 3] vector[d_int+1] transformed_param_vector_array_3d;

  transformed_param_vector = simplex_jacobian(d_vector);
  transformed_param_vector_array = simplex_jacobian(d_vector_array);
  transformed_param_vector_array_2d = simplex_jacobian(d_vector_array_2d);
  transformed_param_vector_array_3d = simplex_jacobian(d_vector_array_3d);

  transformed_param_vector = simplex_jacobian(p_vector);
  transformed_param_vector_array = simplex_jacobian(p_vector_array);
  transformed_param_vector_array_2d = simplex_jacobian(p_vector_array_2d);
  transformed_param_vector_array_3d = simplex_jacobian(p_vector_array_3d);
}
model {
  y_p ~ normal(0, 1);
}

