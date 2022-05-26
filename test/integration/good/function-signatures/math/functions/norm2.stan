data { 
  array[3] real a;
  vector[3] v;
  row_vector[3] rv;  
}
transformed data {
  real transformed_data_real;

  transformed_data_real = norm2(a);
  transformed_data_real = norm2(v);
  transformed_data_real = norm2(rv);
}
parameters {
  array[3] real va;
  vector[3] vv;
  row_vector[3] vrv;  
  real y_p;
}
transformed parameters {
  real transformed_param_real;

  transformed_param_real = norm2(va);
  transformed_param_real = norm2(vv);
  transformed_param_real = norm2(vrv);
}
model {  
  y_p ~ normal(0,1);
}
