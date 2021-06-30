data { 
  complex d_complex;
  
}
transformed data {
  real transformed_data_real;

  transformed_data_real = get_imag(d_complex);
}
parameters {
  complex p_complex;
  real y_p;
}
transformed parameters {
  real transformed_param_real;

  transformed_param_real = get_imag(d_complex);

  transformed_param_real = get_imag(p_complex);

}
model {  
  y_p ~ normal(0,1);
}
