data {
  int n;
  real x;
  array[3, 2] int nn;
  array[5, 2] real y;
  vector[3] v;
  row_vector[3] rv;
  simplex[5] sv;
  unit_vector[7] uv;
  sum_to_zero_vector[8] stzv;
  sum_to_zero_matrix[2,4] stzm;
  ordered[3] ov;
  matrix[4, 5] m;
  cov_matrix[3] covm;
  corr_matrix[3] corrm;
  row_stochastic_matrix[3,4] rowstoch;
  column_stochastic_matrix[3,4] colstoch;
}
transformed data {
  int n_td;
  real x_td;
  array[3, 2] int nn_td;
  array[5, 2] real y_td;
  vector[3] v_td;
  row_vector[3] rv_td;
  simplex[5] sv_td;
  unit_vector[7] uv_td;
  sum_to_zero_vector[8] stzv_td;
  sum_to_zero_matrix[2,4] stzm_td;
  ordered[3] ov_td;
  matrix[4, 5] m_td;
  cov_matrix[3] covm_td;
  corr_matrix[3] corrm_td;
  row_stochastic_matrix[3,4] rowstoch_td;
  column_stochastic_matrix[3,4] colstoch_td;
}
parameters {
  real x_p;
  array[5, 2] real y_p;
  vector[3] v_p;
  row_vector[3] rv_p;
  simplex[5] sv_p;
  unit_vector[7] uv_p;
  sum_to_zero_vector[8] stzv_p;
  sum_to_zero_matrix[2,4] stzm_p;
  ordered[3] ov_p;
  matrix[4, 5] m_p;
  cov_matrix[3] covm_p;
  corr_matrix[3] corrm_p;
  row_stochastic_matrix[3,4] rowstoch_p;
  column_stochastic_matrix[3,4] colstoch_p;
}
transformed parameters {
  real x_tp;
  array[5, 2] real y_tp;
  vector[3] v_tp;
  row_vector[3] rv_tp;
  simplex[5] sv_tp;
  unit_vector[7] uv_tp;
  sum_to_zero_vector[8] stzv_tp;
  sum_to_zero_matrix[2,4] stzm_tp;
  ordered[3] ov_tp;
  matrix[4, 5] m_tp;
  cov_matrix[3] covm_tp;
  corr_matrix[3] corrm_tp;
  row_stochastic_matrix[3,4] rowstoch_tp;
  column_stochastic_matrix[3,4] colstoch_tp;
}
model {
  int n_l;
  real x_l;
  array[3, 2] int nn_l;
  array[5, 2] real y_l;
  vector[3] v_l;
  row_vector[3] rv_l;
  matrix[4, 5] m_l;

  x_p ~ normal(0, 1);
}
generated quantities {
  int n_gq;
  real x_gq;
  array[3, 2] int nn_gq;
  array[5, 2] real y_gq;
  vector[3] v_gq;
  row_vector[3] rv_gq;
  simplex[5] sv_gq;
  unit_vector[7] uv_gq;
  sum_to_zero_vector[8] stzv_gq;
  sum_to_zero_matrix[2,4] stzm_gq;
  ordered[3] ov_gq;
  matrix[4, 5] m_gq;
  cov_matrix[3] covm_gq;
  corr_matrix[3] corrm_gq;
  row_stochastic_matrix[3,4] rowstoch_gq;
  column_stochastic_matrix[3,4] colstoch_gq;
}

