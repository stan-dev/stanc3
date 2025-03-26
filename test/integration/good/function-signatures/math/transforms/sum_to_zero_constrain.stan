data {
  int d_int;
  vector[d_int] d_vector;
  matrix[d_int, d_int] d_matrix;
  array[d_int] vector[d_int] d_vector_array_1d;
  array[d_int, 2] vector[d_int] d_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] d_vector_array_3d;
  array[d_int] matrix[d_int, d_int] d_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] d_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] d_matrix_array_3d;


}

transformed data {
  int td_int;
  vector[d_int+1] td_vector;
  matrix[d_int+1, d_int+1] td_matrix;
  array[d_int] vector[d_int+1] td_vector_array_1d;
  array[d_int, 2] vector[d_int+1] td_vector_array_2d;
  array[d_int, 2, 3] vector[d_int+1] td_vector_array_3d;
  array[d_int] matrix[d_int+1, d_int+1] td_matrix_array_1d;
  array[d_int, 2] matrix[d_int+1, d_int+1] td_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int+1, d_int+1] td_matrix_array_3d;

  td_matrix = sum_to_zero_constrain(d_matrix);
  td_matrix_array_1d = sum_to_zero_constrain(d_matrix_array_1d);
  td_matrix_array_2d = sum_to_zero_constrain(d_matrix_array_2d);
  td_matrix_array_3d = sum_to_zero_constrain(d_matrix_array_3d);
  td_vector = sum_to_zero_constrain(d_vector);
  td_vector_array_1d = sum_to_zero_constrain(d_vector_array_1d);
  td_vector_array_2d = sum_to_zero_constrain(d_vector_array_2d);
  td_vector_array_3d = sum_to_zero_constrain(d_vector_array_3d);
}

parameters {
  vector[d_int] p_vector;
  matrix[d_int, d_int] p_matrix;
  array[d_int] vector[d_int] p_vector_array_1d;
  array[d_int, 2] vector[d_int] p_vector_array_2d;
  array[d_int, 2, 3] vector[d_int] p_vector_array_3d;
  array[d_int] matrix[d_int, d_int] p_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] p_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] p_matrix_array_3d;


}

transformed parameters {
  vector[d_int+1] transformed_param_vector;
  matrix[d_int+1, d_int+1] transformed_param_matrix;
  array[d_int] vector[d_int+1] transformed_param_vector_array_1d;
  array[d_int, 2] vector[d_int+1] transformed_param_vector_array_2d;
  array[d_int, 2, 3] vector[d_int+1] transformed_param_vector_array_3d;
  array[d_int] matrix[d_int+1, d_int+1] transformed_param_matrix_array_1d;
  array[d_int, 2] matrix[d_int+1, d_int+1] transformed_param_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int+1, d_int+1] transformed_param_matrix_array_3d;

  transformed_param_matrix = sum_to_zero_constrain(d_matrix);
  transformed_param_matrix = sum_to_zero_constrain(p_matrix);
  transformed_param_matrix_array_1d = sum_to_zero_constrain(d_matrix_array_1d);
  transformed_param_matrix_array_1d = sum_to_zero_constrain(p_matrix_array_1d);
  transformed_param_matrix_array_2d = sum_to_zero_constrain(d_matrix_array_2d);
  transformed_param_matrix_array_2d = sum_to_zero_constrain(p_matrix_array_2d);
  transformed_param_matrix_array_3d = sum_to_zero_constrain(d_matrix_array_3d);
  transformed_param_matrix_array_3d = sum_to_zero_constrain(p_matrix_array_3d);
  transformed_param_vector = sum_to_zero_constrain(d_vector);
  transformed_param_vector = sum_to_zero_constrain(p_vector);
  transformed_param_vector_array_1d = sum_to_zero_constrain(d_vector_array_1d);
  transformed_param_vector_array_1d = sum_to_zero_constrain(p_vector_array_1d);
  transformed_param_vector_array_2d = sum_to_zero_constrain(d_vector_array_2d);
  transformed_param_vector_array_2d = sum_to_zero_constrain(p_vector_array_2d);
  transformed_param_vector_array_3d = sum_to_zero_constrain(d_vector_array_3d);
  transformed_param_vector_array_3d = sum_to_zero_constrain(p_vector_array_3d);
}

