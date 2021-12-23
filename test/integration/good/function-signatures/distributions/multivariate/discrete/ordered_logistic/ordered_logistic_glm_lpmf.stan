transformed data {
  int N = 2;
  int M = 3;
  int d_y = 1;
  array[N] int d_y_a = {1, 0};
  matrix[N, M] d_x_m = [[1, 2, 3], [4, 5, 6]];
  row_vector[M] d_x_rv = [1, 2, 3];
  vector[N] d_cuts_v = [1, 2]';
  vector[N] d_alpha_v = [0.5, 0.6]';
  real transformed_data_real;
  transformed_data_real = ordered_logistic_glm_lpmf(d_y | d_x_m, d_alpha_v, d_cuts_v);
  transformed_data_real = ordered_logistic_glm_lpmf(d_y | d_x_rv, d_alpha_v, d_cuts_v);
  transformed_data_real = ordered_logistic_glm_lpmf(d_y_a | d_x_m, d_alpha_v, d_cuts_v);
  transformed_data_real = ordered_logistic_glm_lpmf(d_y_a | d_x_rv, d_alpha_v, d_cuts_v);
}
parameters {
  matrix[N, M] p_x_m;
  row_vector[M] p_x_rv;
  vector[N] p_cuts_v;
  vector[N] p_alpha_v;
  real y_p;
}
transformed parameters {
  real transformed_param_real;
  transformed_param_real = ordered_logistic_glm_lpmf(d_y | p_x_m, p_alpha_v, p_cuts_v);
  transformed_param_real = ordered_logistic_glm_lpmf(d_y | p_x_rv, p_alpha_v, p_cuts_v);
  transformed_param_real = ordered_logistic_glm_lpmf(d_y_a | p_x_m, p_alpha_v, p_cuts_v);
  transformed_param_real = ordered_logistic_glm_lpmf(d_y_a | p_x_rv, p_alpha_v, p_cuts_v);
}
model {
  y_p ~ normal(0, 1);
}

