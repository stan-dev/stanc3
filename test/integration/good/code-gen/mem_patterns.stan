data {
 int N;
 int M;
 matrix[N, M] dat_x;
 vector[N] y;
}

parameters {
 real alpha;
 vector[M] vec_v_soa;
 array[10] vector[N] arr_vec_v_soa;
 vector[M] vec_v_aos_for_loopd;
 vector[M] vec_v_aos_assign_to_aos;
}

transformed parameters {
 vector[M] tp_vec_v_aos = inv(vec_v_aos_assign_to_aos);
 tp_vec_v_aos = vec_v_aos_assign_to_aos;
}

model {
  y ~ normal(alpha + multiply(dat_x, vec_v_soa), 1.0);
  for (i in 1:10) {
      y ~ normal(multiply(dat_x, arr_vec_v_soa[i]), 1.0);
  }
  // SHOULD NOT be SOA
  for (i in 1:10) {
      y ~ normal(multiply(dat_x, vec_v_aos_for_loopd), 1.0);
  }

}