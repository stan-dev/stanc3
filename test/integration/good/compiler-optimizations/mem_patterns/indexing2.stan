data {
 int N;
 int M;
 array[N] int Idx;
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
  vector[N] tp_soa_single_idx_in_upfrom_idx;
  for (i in 1:5) {
  tp_soa_single_idx_in_upfrom_idx = tp_soa_multi_idx_assign_in_loop[Idx[i]:];
  }

}