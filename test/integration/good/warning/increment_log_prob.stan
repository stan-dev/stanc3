transformed data {
  int n;
  array[2] int nn;
  array[3, 4] int nnn;
  
  real x;
  array[5] real xx;
  array[6, 7] real xxx;
  array[8, 9, 10] real xxxx;
  
  vector[2] v;
  array[4] vector[3] vv;
  array[5, 6] vector[4] vvv;
  
  row_vector[2] rv;
  array[4] row_vector[3] rvv;
  array[5, 6] row_vector[4] rvvv;
  
  matrix[7, 8] m;
  array[2] matrix[7, 8] mm;
  array[3, 4] matrix[7, 8] mmm;
}
parameters {
  real p_x;
  array[5] real p_xx;
  array[6, 7] real p_xxx;
  array[8, 9, 10] real p_xxxx;
  
  vector[2] p_v;
  array[4] vector[3] p_vv;
  array[5, 6] vector[4] p_vvv;
  
  row_vector[2] p_rv;
  array[4] row_vector[3] p_rvv;
  array[5, 6] row_vector[4] p_rvvv;
  
  matrix[7, 8] p_m;
  array[2] matrix[7, 8] p_mm;
  array[3, 4] matrix[7, 8] p_mmm;
}
model {
  increment_log_prob(n);
  increment_log_prob(nn);
  increment_log_prob(nnn);
  
  increment_log_prob(x);
  increment_log_prob(xx);
  increment_log_prob(xxx);
  increment_log_prob(xxxx);
  
  increment_log_prob(v);
  increment_log_prob(vv);
  increment_log_prob(vvv);
  
  increment_log_prob(rv);
  increment_log_prob(rvv);
  increment_log_prob(rvvv);
  
  increment_log_prob(m);
  increment_log_prob(mm);
  increment_log_prob(mmm);
  
  increment_log_prob(p_x);
  increment_log_prob(p_xx);
  increment_log_prob(p_xxx);
  increment_log_prob(p_xxxx);
  
  increment_log_prob(p_v);
  increment_log_prob(p_vv);
  increment_log_prob(p_vvv);
  
  increment_log_prob(p_rv);
  increment_log_prob(p_rvv);
  increment_log_prob(p_rvvv);
  
  increment_log_prob(p_m);
  increment_log_prob(p_mm);
  increment_log_prob(p_mmm);
}
