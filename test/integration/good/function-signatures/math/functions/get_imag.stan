data { 
  complex d_complex;
}
transformed data {
  real td_real;
  complex td_complex = 1;

  td_real = get_imag(d_complex);
  td_real = get_imag(td_complex);
}
parameters {
  complex p_complex;
  real y_p;
}
transformed parameters {
  real tp_real;
  complex tp_complex;

  tp_real = get_imag(d_complex);
  tp_real = get_imag(td_complex);
  tp_real = get_imag(p_complex);
  tp_real = get_imag(tp_complex);
}
model {  
  y_p ~ normal(0,1);
}
generated quantities {
  real gq_real;
  complex gq_complex = 1;

  gq_real = get_imag(d_complex);
  gq_real = get_imag(td_complex);
  gq_real = get_imag(p_complex);
  gq_real = get_imag(tp_complex);
  gq_real = get_imag(gq_complex);
}