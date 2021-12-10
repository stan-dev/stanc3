functions {
  vector foo(vector shared_params, vector job_params,
             array[] real data_r, array[] int data_i) {
    return [1, 2, 3]';
  }
}
data {
  vector[3] shared_params_d;
  array[3] vector[3] job_params_d;
  array[3, 3] real data_r;
  array[3, 3] int data_i;
}
parameters {
  vector[3] shared_params_p;
  array[3] vector[3] job_params_p;
  array[3, 3] real data_r_p;
}
transformed parameters {
  vector[3] y_hat_gq
      = map_rect(foo, shared_params_d, job_params_d, data_r_p, data_i);
}
