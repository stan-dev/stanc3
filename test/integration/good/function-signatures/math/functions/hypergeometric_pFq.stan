data {
  int d_int;
  real d_real;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;


}

transformed data {
  int td_int;
  real td_real;
  vector[d_int] td_vector;
  row_vector[d_int] td_row_vector;

  td_real = hypergeometric_pFq(d_row_vector, d_row_vector, d_real);
  td_real = hypergeometric_pFq(d_row_vector, d_vector, d_real);
  td_real = hypergeometric_pFq(d_vector, d_row_vector, d_real);
  td_real = hypergeometric_pFq(d_vector, d_vector, d_real);
}

parameters {
  real p_real;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;


}

transformed parameters {
  real transformed_param_real;
  vector[d_int] transformed_param_vector;
  row_vector[d_int] transformed_param_row_vector;

  transformed_param_real = hypergeometric_pFq(d_row_vector, d_row_vector, d_real);
  transformed_param_real = hypergeometric_pFq(d_row_vector, d_row_vector, p_real);
  transformed_param_real = hypergeometric_pFq(d_row_vector, d_vector, d_real);
  transformed_param_real = hypergeometric_pFq(d_row_vector, d_vector, p_real);
  transformed_param_real = hypergeometric_pFq(d_row_vector, p_row_vector, d_real);
  transformed_param_real = hypergeometric_pFq(d_row_vector, p_row_vector, p_real);
  transformed_param_real = hypergeometric_pFq(d_row_vector, p_vector, d_real);
  transformed_param_real = hypergeometric_pFq(d_row_vector, p_vector, p_real);
  transformed_param_real = hypergeometric_pFq(d_vector, d_row_vector, d_real);
  transformed_param_real = hypergeometric_pFq(d_vector, d_row_vector, p_real);
  transformed_param_real = hypergeometric_pFq(d_vector, d_vector, d_real);
  transformed_param_real = hypergeometric_pFq(d_vector, d_vector, p_real);
  transformed_param_real = hypergeometric_pFq(d_vector, p_row_vector, d_real);
  transformed_param_real = hypergeometric_pFq(d_vector, p_row_vector, p_real);
  transformed_param_real = hypergeometric_pFq(d_vector, p_vector, d_real);
  transformed_param_real = hypergeometric_pFq(d_vector, p_vector, p_real);
  transformed_param_real = hypergeometric_pFq(p_row_vector, d_row_vector, d_real);
  transformed_param_real = hypergeometric_pFq(p_row_vector, d_row_vector, p_real);
  transformed_param_real = hypergeometric_pFq(p_row_vector, d_vector, d_real);
  transformed_param_real = hypergeometric_pFq(p_row_vector, d_vector, p_real);
  transformed_param_real = hypergeometric_pFq(p_row_vector, p_row_vector, d_real);
  transformed_param_real = hypergeometric_pFq(p_row_vector, p_row_vector, p_real);
  transformed_param_real = hypergeometric_pFq(p_row_vector, p_vector, d_real);
  transformed_param_real = hypergeometric_pFq(p_row_vector, p_vector, p_real);
  transformed_param_real = hypergeometric_pFq(p_vector, d_row_vector, d_real);
  transformed_param_real = hypergeometric_pFq(p_vector, d_row_vector, p_real);
  transformed_param_real = hypergeometric_pFq(p_vector, d_vector, d_real);
  transformed_param_real = hypergeometric_pFq(p_vector, d_vector, p_real);
  transformed_param_real = hypergeometric_pFq(p_vector, p_row_vector, d_real);
  transformed_param_real = hypergeometric_pFq(p_vector, p_row_vector, p_real);
  transformed_param_real = hypergeometric_pFq(p_vector, p_vector, d_real);
  transformed_param_real = hypergeometric_pFq(p_vector, p_vector, p_real);
}

