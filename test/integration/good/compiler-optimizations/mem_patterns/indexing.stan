functions {
  int mask_fun(int i) {
    return i;
  }
  vector udf_fun(vector A) {
    return A;
  }
}

data {
 int N;
 int M;
 matrix[N, M] dat_x;
 vector[N] y;
 array[N] int idx_tester;
}

parameters {
  real alpha;
  // SoA params
  vector[M] p_soa_vec_v;
  matrix[N, M] p_soa_mat;
  array[10] vector[N] p_soa_arr_vec_v;
  matrix[N,M] p_soa_mat_uni_col_idx;
  vector[N] p_soa_vec_uni_idx;
  matrix[N,M] p_soa_loop_mat_uni_col_idx;
  row_vector[N] p_soa_lhs_loop_mul;
  vector[N] p_soa_rhs_loop_mul;
  vector[N] p_soa_used_with_aos_in_excluded_fun;
  matrix[N,M] p_soa_loop_mat_multi_uni_uni_idx;
  // Should not be SoA
    // because of failed function
  vector[M] p_aos_vec_v_assign_to_aos;
    // because tp assigned to is used in non-supported 
  vector[M] p_aos_vec_v_tp_fails_func;
    // Because of uni idx in loop
  vector[M] p_aos_loop_vec_v_uni_idx;
  // Because of multi index assign
  vector[M] p_aos_fail_assign_from_top_idx;
  matrix[N,M] p_aos_loop_mat_uni_uni_idx;
  // Because used in sub-function that supports 
  // SoA but outer function does not
  matrix[N, M] p_aos_mat;
  // Single index called on result of multiply
  matrix[N, M] p_aos_mat_pass_func_outer_single_indexed1;
  matrix[N, M] p_aos_mat_pass_func_outer_single_indexed2;

  // Because used in single cell index from function
  matrix[N, M] p_aos_mat_fail_uni_uni_idx1;
  matrix[N, M] p_aos_mat_fail_uni_uni_idx2;

}

transformed parameters {
  // should be SoA
  real tp_real_from_aos = p_soa_vec_v[1];
  // should be AoS
    // because assigning AoS
  vector[M] tp_aos_vec_v = udf_fun(p_aos_vec_v_assign_to_aos);
  // Should be SoA because single assign out of loop
  vector[M] tp_soa_single_idx_uninit;
  tp_soa_single_idx_uninit[1] = tp_real_from_aos;
  tp_soa_single_idx_uninit[2:M] = rep_vector(tp_real_from_aos, M - 1);
    // because tp vec is used in not supported func
    // both should fail
  vector[M] tp_aos_fail_func_vec_v = p_aos_vec_v_tp_fails_func;
  // Should pass for single index on rhs out of loop
  tp_aos_fail_func_vec_v[1] = p_soa_vec_v[1];
  // Should fail
  vector[M] tp_aos_fail_assign_from_top_idx;
  tp_aos_fail_assign_from_top_idx[1:] = p_aos_fail_assign_from_top_idx[1:];
}

model {
  vector[N] tp_soa_used_with_aos_in_excluded_fun = p_soa_used_with_aos_in_excluded_fun * size(tp_aos_vec_v);
  y ~ normal(alpha + multiply(dat_x, p_soa_vec_v + tp_soa_used_with_aos_in_excluded_fun), 1.0);
  y ~ normal(alpha + multiply(dat_x, p_soa_vec_v), 1.0);
  y ~ normal(multiply(dat_x, p_soa_mat_uni_col_idx[,N]), 1.0);
  y ~ normal(multiply(dat_x, p_soa_mat_uni_col_idx[,N]), p_soa_vec_uni_idx[N]);
  y ~ normal(multiply(dat_x, udf_fun(tp_aos_fail_func_vec_v)), p_soa_vec_uni_idx[N]);
  y ~ normal(multiply(p_soa_mat, udf_fun(tp_aos_fail_func_vec_v)), p_soa_vec_uni_idx[N]);
  y ~ normal(multiply(p_soa_mat, udf_fun(tp_aos_fail_assign_from_top_idx)), p_soa_vec_uni_idx[N]);
  y ~ normal(multiply(p_soa_mat, udf_fun(multiply(p_aos_mat, tp_aos_fail_func_vec_v))), p_soa_vec_uni_idx[N]);


  vector[N] tp_soa_single_assign;
  tp_soa_single_assign[1] = 2.0;
  vector[N] tp_soa_single_assign_from_soa;
  tp_soa_single_assign_from_soa[2] = multiply(p_soa_lhs_loop_mul, p_soa_rhs_loop_mul);

  // These should be fine
  for (i in 1:10) {
    y ~ normal(multiply(dat_x, p_soa_arr_vec_v[i]), 1.0);
    y ~ normal(multiply(dat_x, p_soa_loop_mat_uni_col_idx[,i]), 1.0);
    y ~ normal(multiply(p_soa_loop_mat_uni_col_idx[i,], dat_x), 1.0);
    y ~ normal(multiply(dat_x, p_soa_loop_mat_uni_col_idx[i,]'), 1.0);
    y ~ normal(multiply(dat_x, p_soa_loop_mat_uni_col_idx[1:N, 1]), 1.0);
    y ~ normal(multiply(dat_x, p_soa_loop_mat_uni_col_idx[i]'), 1.0);
    y ~ normal(multiply(dat_x, p_soa_loop_mat_multi_uni_uni_idx[idx_tester[i],]'), 1.0);
    y ~ normal(multiply(dat_x[,i], transpose(multiply(p_aos_mat_pass_func_outer_single_indexed1,
     p_aos_mat_pass_func_outer_single_indexed2))[mask_fun(i), mask_fun(i)]), 1.0);
    y ~ normal(multiply(dat_x[,i], p_aos_mat_fail_uni_uni_idx1[i, i] *
     p_aos_mat_fail_uni_uni_idx2[i, i]), 1.0);
  }

  // SHOULD NOT BE SOA
  vector[N] tp_aos_loop_vec_v_uni_idx;
  vector[N] tp_aos_loop_vec_v_multi_uni_idx;
  for (i in 1:N) {
    // lhs should fail, rhs should succeed
    tp_aos_loop_vec_v_uni_idx[i] = multiply(p_soa_lhs_loop_mul, p_soa_rhs_loop_mul);
    tp_aos_loop_vec_v_multi_uni_idx[idx_tester[i]] = multiply(p_soa_lhs_loop_mul, p_soa_rhs_loop_mul);
    // single indexing failures
    y ~ normal(multiply(dat_x[,i], p_aos_loop_vec_v_uni_idx[i]), 1.0);
    y ~ normal(multiply(dat_x[,i], p_aos_loop_mat_uni_uni_idx[i, i]), 1.0);
    y ~ normal(dat_x[,i], p_aos_mat_fail_uni_uni_idx1[i, i] *
     p_aos_mat_fail_uni_uni_idx2[i, i]);

  }

}