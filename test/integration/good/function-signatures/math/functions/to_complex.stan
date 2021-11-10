data { 
  int d_int;
  int r_int;
  real d_real;
  real r_real;

}
transformed data {
  complex transformed_data_complex;
  complex transformed_data_complex_2;

  transformed_data_complex = to_complex(d_int, r_int);
  transformed_data_complex = to_complex(d_real, r_int);
  transformed_data_complex = to_complex(d_int, r_real);
  transformed_data_complex = to_complex(d_real, r_real);
  transformed_data_complex = to_complex(d_real);
  transformed_data_complex = to_complex(d_int);
  transformed_data_complex = to_complex();


}
parameters {
  real p_real;
  real y_p;
}
transformed parameters {
  complex transformed_param_complex;
  complex transformed_param_complex_2;

  transformed_param_complex =  to_complex(d_int, r_int);
  transformed_param_complex =  to_complex(d_real, r_int);
  transformed_param_complex =  to_complex(d_int, r_real);
  transformed_param_complex =  to_complex(d_real, r_real);
  transformed_param_complex = to_complex(d_real);
  transformed_param_complex = to_complex(d_int);
  transformed_param_complex = to_complex();

  transformed_param_complex =  to_complex(p_real, r_int);
  transformed_param_complex =  to_complex(p_real, r_real);
  transformed_param_complex =  to_complex(r_int, p_real);
  transformed_param_complex =  to_complex(r_real, r_real);
  transformed_param_complex =  to_complex(p_real, p_real);
  transformed_param_complex = to_complex(p_real);

}
model {  
  y_p ~ normal(0,1);
}
