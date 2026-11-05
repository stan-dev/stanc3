data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  complex_matrix[d_int, d_int] d_complex_matrix;


}

transformed data {
  int td_int;
  matrix[d_int, d_int] td_matrix;
  complex_matrix[d_int, d_int] td_complex_matrix;

  td_complex_matrix = solve_sylvester(d_complex_matrix, d_complex_matrix, d_complex_matrix);
  td_complex_matrix = solve_sylvester(d_complex_matrix, d_complex_matrix, d_matrix);
  td_complex_matrix = solve_sylvester(d_complex_matrix, d_matrix, d_complex_matrix);
  td_complex_matrix = solve_sylvester(d_complex_matrix, d_matrix, d_matrix);
  td_complex_matrix = solve_sylvester(d_matrix, d_complex_matrix, d_complex_matrix);
  td_complex_matrix = solve_sylvester(d_matrix, d_complex_matrix, d_matrix);
  td_complex_matrix = solve_sylvester(d_matrix, d_matrix, d_complex_matrix);
  td_complex_matrix = solve_sylvester(d_matrix, d_matrix, d_matrix);
}

parameters {
  matrix[d_int, d_int] p_matrix;
  complex_matrix[d_int, d_int] p_complex_matrix;


}

transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;
  complex_matrix[d_int, d_int] transformed_param_complex_matrix;

  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, d_complex_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, d_complex_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, d_complex_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, d_complex_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, d_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, d_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, d_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, d_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, p_complex_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, p_complex_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, p_complex_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, p_complex_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, p_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, p_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, p_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_complex_matrix, p_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, d_complex_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, d_complex_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, d_complex_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, d_complex_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, d_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, d_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, d_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, d_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, p_complex_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, p_complex_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, p_complex_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, p_complex_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, p_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, p_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, p_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(d_matrix, p_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, d_complex_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, d_complex_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, d_complex_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, d_complex_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, d_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, d_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, d_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, d_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, p_complex_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, p_complex_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, p_complex_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, p_complex_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, p_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, p_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, p_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_complex_matrix, p_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, d_complex_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, d_complex_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, d_complex_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, d_complex_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, d_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, d_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, d_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, d_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, p_complex_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, p_complex_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, p_complex_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, p_complex_matrix, p_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, p_matrix, d_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, p_matrix, d_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, p_matrix, p_complex_matrix);
  transformed_param_complex_matrix = solve_sylvester(p_matrix, p_matrix, p_matrix);
}

