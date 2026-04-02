data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  array[d_int] matrix[d_int, d_int] d_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] d_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] d_matrix_array_3d;


}

transformed data {
  int td_int;
  matrix[d_int, d_int] td_matrix;
  vector[choose(d_int, 2)] td_vector;
  array[d_int] matrix[d_int, d_int] td_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] td_matrix_array_2d;
  array[d_int] vector[choose(d_int, 2)] td_vector_array_1d;
  array[d_int, 2, 3] matrix[d_int, d_int] td_matrix_array_3d;
  array[d_int, 2] vector[choose(d_int, 2)] td_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int, 2)] td_vector_array_3d;

  td_vector = corr_matrix_unconstrain(d_matrix);
  td_vector_array_1d = corr_matrix_unconstrain(d_matrix_array_1d);
  td_vector_array_2d = corr_matrix_unconstrain(d_matrix_array_2d);
  td_vector_array_3d = corr_matrix_unconstrain(d_matrix_array_3d);
}

parameters {
  matrix[d_int, d_int] p_matrix;
  vector[choose(d_int, 2)] p_vector;
  array[d_int] matrix[d_int, d_int] p_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] p_matrix_array_2d;
  array[d_int] vector[choose(d_int, 2)] p_vector_array_1d;
  array[d_int, 2, 3] matrix[d_int, d_int] p_matrix_array_3d;
  array[d_int, 2] vector[choose(d_int, 2)] p_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int, 2)] p_vector_array_3d;


}

transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;
  vector[choose(d_int, 2)] transformed_param_vector;
  array[d_int] matrix[d_int, d_int] transformed_param_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] transformed_param_matrix_array_2d;
  array[d_int] vector[choose(d_int, 2)] transformed_param_vector_array_1d;
  array[d_int, 2, 3] matrix[d_int, d_int] transformed_param_matrix_array_3d;
  array[d_int, 2] vector[choose(d_int, 2)] transformed_param_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int, 2)] transformed_param_vector_array_3d;

  transformed_param_vector = corr_matrix_unconstrain(d_matrix);
  transformed_param_vector = corr_matrix_unconstrain(p_matrix);
  transformed_param_vector_array_1d = corr_matrix_unconstrain(d_matrix_array_1d);
  transformed_param_vector_array_1d = corr_matrix_unconstrain(p_matrix_array_1d);
  transformed_param_vector_array_2d = corr_matrix_unconstrain(d_matrix_array_2d);
  transformed_param_vector_array_2d = corr_matrix_unconstrain(p_matrix_array_2d);
  transformed_param_vector_array_3d = corr_matrix_unconstrain(d_matrix_array_3d);
  transformed_param_vector_array_3d = corr_matrix_unconstrain(p_matrix_array_3d);
}

