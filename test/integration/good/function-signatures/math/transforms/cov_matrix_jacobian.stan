data {
  int d_int;
  vector[choose(d_int,2)] d_vector;
  array[d_int] vector[choose(d_int,2)] d_vector_array_1d;
  array[d_int, 2] vector[choose(d_int,2)] d_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int,2)] d_vector_array_3d;


}

parameters {
  vector[choose(d_int,2)] p_vector;
  array[d_int] vector[choose(d_int,2)] p_vector_array_1d;
  array[d_int, 2] vector[choose(d_int,2)] p_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int,2)] p_vector_array_3d;


}

transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;
  vector[choose(d_int,2)] transformed_param_vector;
  array[d_int] matrix[d_int, d_int] transformed_param_matrix_array_1d;
  array[d_int] vector[choose(d_int,2)] transformed_param_vector_array_1d;
  array[d_int, 2] matrix[d_int, d_int] transformed_param_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] transformed_param_matrix_array_3d;
  array[d_int, 2] vector[choose(d_int,2)] transformed_param_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int,2)] transformed_param_vector_array_3d;

  transformed_param_matrix = cov_matrix_jacobian(d_vector, d_int);
  transformed_param_matrix = cov_matrix_jacobian(p_vector, d_int);
  transformed_param_matrix_array_1d = cov_matrix_jacobian(d_vector_array_1d, d_int);
  transformed_param_matrix_array_1d = cov_matrix_jacobian(p_vector_array_1d, d_int);
  transformed_param_matrix_array_2d = cov_matrix_jacobian(d_vector_array_2d, d_int);
  transformed_param_matrix_array_2d = cov_matrix_jacobian(p_vector_array_2d, d_int);
  transformed_param_matrix_array_3d = cov_matrix_jacobian(d_vector_array_3d, d_int);
  transformed_param_matrix_array_3d = cov_matrix_jacobian(p_vector_array_3d, d_int);
}

