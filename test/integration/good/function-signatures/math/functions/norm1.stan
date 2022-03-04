data { 
  array[3] real a;
  vector[3] v;
  row_vector[3] rv;  
}
transformed data {
  real transformed_data_real;

  transformed_data_real = norm1(a);
  transformed_data_real = norm1(v);
  transformed_data_real = norm1(rv);
}
parameters {
  array[3] real va;
  vector[3] vv;
  row_vector[3] vrv;  
  real y_p;
}
transformed parameters {
  real transformed_param_real;

  transformed_param_real = norm1(va);
  transformed_param_real = norm1(vv);
  transformed_param_real = norm1(vrv);
}
model {  
  y_p ~ normal(0,1);
}
