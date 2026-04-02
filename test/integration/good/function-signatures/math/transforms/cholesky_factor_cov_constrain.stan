data {
  int d_int;
  vector[choose(d_int, 2) + d_int] d_vector;
  array[d_int] vector[choose(d_int, 2) + d_int] d_vector_array_1d;
  array[d_int, 2] vector[choose(d_int, 2) + d_int] d_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int, 2) + d_int] d_vector_array_3d;


}

transformed data {
  int td_int;
  matrix[d_int, d_int] td_matrix;
  vector[choose(d_int, 2) + d_int] td_vector;
  array[d_int] matrix[d_int, d_int] td_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] td_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] td_matrix_array_3d;
  array[d_int] vector[choose(d_int, 2) + d_int] td_vector_array_1d;
  array[d_int, 2] vector[choose(d_int, 2) + d_int] td_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int, 2) + d_int] td_vector_array_3d;

  td_matrix = cholesky_factor_cov_constrain(d_vector, d_int, d_int);
  td_matrix_array_1d = cholesky_factor_cov_constrain(d_vector_array_1d, d_int, d_int);
  td_matrix_array_2d = cholesky_factor_cov_constrain(d_vector_array_2d, d_int, d_int);
  td_matrix_array_3d = cholesky_factor_cov_constrain(d_vector_array_3d, d_int, d_int);
}

parameters {
  matrix[d_int, d_int] p_matrix;
  vector[choose(d_int, 2) + d_int] p_vector;
  array[d_int] matrix[d_int, d_int] p_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] p_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] p_matrix_array_3d;
  array[d_int] vector[choose(d_int, 2) + d_int] p_vector_array_1d;
  array[d_int, 2] vector[choose(d_int, 2) + d_int] p_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int, 2) + d_int] p_vector_array_3d;


}

transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;
  vector[choose(d_int, 2) + d_int] transformed_param_vector;
  array[d_int] matrix[d_int, d_int] transformed_param_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] transformed_param_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] transformed_param_matrix_array_3d;
  array[d_int] vector[choose(d_int, 2) + d_int] transformed_param_vector_array_1d;
  array[d_int, 2] vector[choose(d_int, 2) + d_int] transformed_param_vector_array_2d;
  array[d_int, 2, 3] vector[choose(d_int, 2) + d_int] transformed_param_vector_array_3d;

  transformed_param_matrix = cholesky_factor_cov_constrain(d_vector, d_int, d_int);
  transformed_param_matrix = cholesky_factor_cov_constrain(p_vector, d_int, d_int);
  transformed_param_matrix_array_1d = cholesky_factor_cov_constrain(d_vector_array_1d, d_int, d_int);
  transformed_param_matrix_array_1d = cholesky_factor_cov_constrain(p_vector_array_1d, d_int, d_int);
  transformed_param_matrix_array_2d = cholesky_factor_cov_constrain(d_vector_array_2d, d_int, d_int);
  transformed_param_matrix_array_2d = cholesky_factor_cov_constrain(p_vector_array_2d, d_int, d_int);
  transformed_param_matrix_array_3d = cholesky_factor_cov_constrain(d_vector_array_3d, d_int, d_int);
  transformed_param_matrix_array_3d = cholesky_factor_cov_constrain(p_vector_array_3d, d_int, d_int);
}

