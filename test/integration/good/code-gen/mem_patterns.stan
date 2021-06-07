data {
 int N;
 int M;
 matrix[N, M] dat_x;
 vector[N] y;
}

parameters {
  real alpha;
  // SoA params
  vector[M] p_soa_vec_v;
  array[10] vector[N] p_soa_arr_vec_v;
  matrix[N,M] p_soa_mat_uni_col_idx;
  vector[N] p_soa_vec_uni_idx;
  matrix[N,M] p_soa_loop_mat_uni_col_idx;
  // Should not be SoA
    // because of failed function
  vector[M] p_aos_vec_v_assign_to_aos;
    // because tp assigned to is used in non-supported 
  vector[M] p_aos_vec_v_tp_fails_func;
    // Because of uni idx in loop
  vector[M] p_aos_loop_vec_v_uni_idx;
  matrix[N,M] p_aos_loop_mat_uni_uni_idx;
}

transformed parameters {
  // should be SoA
  real tp_real_from_aos = p_soa_vec_v[1];
  // should be AoS
    // because assigning AoS
  vector[M] tp_aos_vec_v = inv(p_aos_vec_v_assign_to_aos);
    // because tp vec is used in not supported func
    // both should fail
  vector[M] tp_aos_fail_func_vec_v = p_aos_vec_v_tp_fails_func;
}

model {
  y ~ normal(alpha + multiply(dat_x, p_soa_vec_v), 1.0);
  y ~ normal(multiply(dat_x, p_soa_mat_uni_col_idx[,N]), 1.0);
  y ~ normal(multiply(dat_x, p_soa_mat_uni_col_idx[,N]), p_soa_vec_uni_idx[N]);
  y ~ normal(multiply(dat_x, inv(tp_aos_fail_func_vec_v)), p_soa_vec_uni_idx[N]);
  // These should be fine
  for (i in 1:10) {
    y ~ normal(multiply(dat_x, p_soa_arr_vec_v[i]), 1.0);
    y ~ normal(multiply(dat_x, p_soa_loop_mat_uni_col_idx[,i]), 1.0);
    y ~ normal(multiply(p_soa_loop_mat_uni_col_idx[i,], dat_x), 1.0);
    y ~ normal(multiply(dat_x, p_soa_loop_mat_uni_col_idx[i,]'), 1.0);
    y ~ normal(multiply(dat_x, p_soa_loop_mat_uni_col_idx[i]'), 1.0);
  }
  // SHOULD NOT BE SOA
  for (i in 1:N) {
    y ~ normal(multiply(dat_x[,i], p_aos_loop_vec_v_uni_idx[i]), 1.0);
    y ~ normal(multiply(dat_x[,i], p_aos_loop_mat_uni_uni_idx[i, i]), 1.0);
  }

}