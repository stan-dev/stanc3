data { 
  int n;
  int n_ps;

  real xs_array[n];
  vector[n] xs_vec;
  row_vector[n] xs_rowvec;

  real<lower=0, upper=1> ps[n_ps];
  real<lower=0, upper=1> p;
}
transformed data {
  real q;
  real qs[n_ps];

  q = quantile(xs_array, p);
  q = quantile(xs_vec, p);
  q = quantile(xs_rowvec, p);

  qs = quantile(xs_array, ps);
  qs = quantile(xs_vec, ps);
  qs = quantile(xs_rowvec, ps);
}
parameters {
  real xs_param_array[n];
  vector[n] xs_param_vec;
  row_vector[n] xs_param_rowvec;

  real<lower=0, upper=1> ps_param[n_ps];
  real<lower=0, upper=1> p_param;
}
transformed parameters {
}
model {  
}
generated quantities {
  real q_pred;
  real qs_pred[n_ps];

  q_pred = quantile(xs_param_array, p_param);
  q_pred = quantile(xs_param_vec, p_param);
  q_pred = quantile(xs_param_rowvec, p_param);

  qs_pred = quantile(xs_param_array, ps_param);
  qs_pred = quantile(xs_param_vec, ps_param);
  qs_pred = quantile(xs_param_rowvec, ps_param);
}
