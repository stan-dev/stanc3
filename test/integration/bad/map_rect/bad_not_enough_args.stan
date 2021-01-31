functions {
  vector foo(vector shared_params, vector job_params,
             real[] data_r) {
    return [1, 2, 3]';
  }
}
transformed data {
  vector[3] shared_params_d;
  vector[3] job_params_d[3];
  real data_r[3, 3];
  int data_i[3, 3];
  vector[3] y_hat_tp1
      = map_rect(foo, shared_params_d, job_params_d, data_r, data_i);
}
