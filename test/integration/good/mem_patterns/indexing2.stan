data {
 int N;
 int M;
}

parameters {
  real alpha;
  vector[10] p_aos_loop_single_idx;
}

model {
  vector[N] tp_soa_multi_idx_assign_in_loop = rep_vector(alpha, N);
  // These should be fine
  for (i in 1:10) {
    tp_soa_multi_idx_assign_in_loop = tp_soa_multi_idx_assign_in_loop * p_aos_loop_single_idx[i];
  }
}