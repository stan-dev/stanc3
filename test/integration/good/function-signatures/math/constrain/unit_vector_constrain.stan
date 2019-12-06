data { 
  int d_int;
  vector[d_int] d_vector;
}
transformed data {
  vector[d_int] transformed_data_vector;

  transformed_data_vector = unit_vector_constrain(d_vector);
}
parameters {
  vector[d_int] p_vector;
  real y_p;
}
transformed parameters {
  vector[d_int] transformed_param_vector;

  transformed_param_vector = unit_vector_constrain(d_vector);
  transformed_param_vector = unit_vector_constrain(p_vector);
}
model {  
  y_p ~ normal(0,1);
}