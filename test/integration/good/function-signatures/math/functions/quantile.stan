data {
  int n;
  int n_ps;
  
  array[n] real xs_array;
  vector[n] xs_vec;
  row_vector[n] xs_rowvec;
  
  array[n_ps] real<lower=0, upper=1> ps;
  real<lower=0, upper=1> p;
}
transformed data {
  real q;
  array[n_ps] real qs;
  
  q = quantile(xs_array, p);
  q = quantile(xs_vec, p);
  q = quantile(xs_rowvec, p);
  
  qs = quantile(xs_array, ps);
  qs = quantile(xs_vec, ps);
  qs = quantile(xs_rowvec, ps);
}
parameters {
  array[n] real xs_param_array;
  vector[n] xs_param_vec;
  row_vector[n] xs_param_rowvec;
  
  array[n_ps] real<lower=0, upper=1> ps_param;
  real<lower=0, upper=1> p_param;
}
transformed parameters {
  
}
model {
  
}
generated quantities {
  real q_pred;
  array[n_ps] real qs_pred;
  
  q_pred = quantile(xs_param_array, p_param);
  q_pred = quantile(xs_param_vec, p_param);
  q_pred = quantile(xs_param_rowvec, p_param);
  
  qs_pred = quantile(xs_param_array, ps_param);
  qs_pred = quantile(xs_param_vec, ps_param);
  qs_pred = quantile(xs_param_rowvec, ps_param);
}

