data {
 int N;
 int M;
 matrix[N, M] dat_x;
 vector[N] y;
}

parameters {
  real alpha;
  // SoA params
  vector[M] soa_vec_v;
  array[10] vector[N] soa_arr_vec_v;
  matrix[N,M] soa_mat_uni_col_idx;
  vector[N] soa_vec_uni_idx;
  matrix[N,M] soa_loop_mat_uni_col_idx;
  // Should not be SoA
    // because of failed function
  vector[M] aos_vec_v_assign_to_aos;
    // Because of uni idx in loop
  vector[M] aos_loop_vec_v_uni_idx;
  matrix[N,M] aos_loop_mat_uni_uni_idx;
}

transformed parameters {
  // should be SoA
  real tp_real_from_aos = soa_vec_v[1];
  // should be AoS
    // because assigning AoS
  vector[M] tp_aos_vec_v = inv(aos_vec_v_assign_to_aos);
}

model {
  y ~ normal(alpha + multiply(dat_x, soa_vec_v), 1.0);
  y ~ normal(multiply(dat_x, soa_mat_uni_col_idx[,N]), 1.0);
  y ~ normal(multiply(dat_x, soa_mat_uni_col_idx[,N]), soa_vec_uni_idx[N]);
  // These should be fine
  for (i in 1:10) {
    y ~ normal(multiply(dat_x, soa_arr_vec_v[i]), 1.0);
    y ~ normal(multiply(dat_x, soa_loop_mat_uni_col_idx[,i]), 1.0);
    y ~ normal(multiply(soa_loop_mat_uni_col_idx[i,], dat_x), 1.0);
    y ~ normal(multiply(dat_x, soa_loop_mat_uni_col_idx[i,]'), 1.0);
  }
  // SHOULD NOT BE SOA
  for (i in 1:N) {
    y ~ normal(multiply(dat_x[,i], aos_loop_vec_v_uni_idx[i]), 1.0);
    y ~ normal(multiply(dat_x[,i], aos_loop_mat_uni_uni_idx[i, i]), 1.0);
  }

}