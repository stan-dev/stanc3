data {
  int d_int;
  matrix[d_int, d_int+1] d_matrix;
  array[d_int] matrix[d_int, d_int+1] d_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int+1] d_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int+1] d_matrix_array_3d;


}

transformed data {
  int td_int;
  matrix[d_int, d_int] td_matrix;
  array[d_int] matrix[d_int, d_int] td_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] td_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] td_matrix_array_3d;

  td_matrix = stochastic_row_unconstrain(d_matrix);
  td_matrix_array_1d = stochastic_row_unconstrain(d_matrix_array_1d);
  td_matrix_array_2d = stochastic_row_unconstrain(d_matrix_array_2d);
  td_matrix_array_3d = stochastic_row_unconstrain(d_matrix_array_3d);
}

parameters {
  matrix[d_int, d_int+1] p_matrix;
  array[d_int] matrix[d_int, d_int+1] p_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int+1] p_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int+1] p_matrix_array_3d;


}

transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;
  array[d_int] matrix[d_int, d_int] transformed_param_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] transformed_param_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] transformed_param_matrix_array_3d;

  transformed_param_matrix = stochastic_row_unconstrain(d_matrix);
  transformed_param_matrix = stochastic_row_unconstrain(p_matrix);
  transformed_param_matrix_array_1d = stochastic_row_unconstrain(d_matrix_array_1d);
  transformed_param_matrix_array_1d = stochastic_row_unconstrain(p_matrix_array_1d);
  transformed_param_matrix_array_2d = stochastic_row_unconstrain(d_matrix_array_2d);
  transformed_param_matrix_array_2d = stochastic_row_unconstrain(p_matrix_array_2d);
  transformed_param_matrix_array_3d = stochastic_row_unconstrain(d_matrix_array_3d);
  transformed_param_matrix_array_3d = stochastic_row_unconstrain(p_matrix_array_3d);
}

