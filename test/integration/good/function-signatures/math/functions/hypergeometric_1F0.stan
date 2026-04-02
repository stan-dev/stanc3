data {
  int d_int;
  real d_real;


}

transformed data {
  int td_int;
  real td_real;

  td_real = hypergeometric_1F0(d_real, d_real);
}

parameters {
  real p_real;


}

transformed parameters {
  real transformed_param_real;

  transformed_param_real = hypergeometric_1F0(d_real, d_real);
  transformed_param_real = hypergeometric_1F0(d_real, p_real);
  transformed_param_real = hypergeometric_1F0(p_real, d_real);
  transformed_param_real = hypergeometric_1F0(p_real, p_real);
}

