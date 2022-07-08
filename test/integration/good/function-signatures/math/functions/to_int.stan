data {
  int d_int;
  real d_real;
  array[d_int] int d_int_array;
  array[d_int] real d_real_array;

  array[3, 4] int x1z;
  array[3, 4] real x2z;

  array[3, 4, 5] int x1w;
  array[3, 4, 5] real x2w;

}
transformed data {
  int transformed_int;
  array[d_int] int transformed_int_arr;
  array[3, 4] int trans_x1z;
  array [3, 4, 5] int trans_x1w;

  transformed_int = to_int(d_int);
  transformed_int = to_int(d_real);

  transformed_int_arr = to_int(d_int_array);
  transformed_int_arr = to_int(d_real_array);

  trans_x1z = to_int(x1z);
  trans_x1z = to_int(x2z);

  trans_x1w = to_int(x1w);
  trans_x1w = to_int(x2w);
}

parameters {
  real p_real;
  array[d_int] real p_real_arr;
}
model {
  p_real ~ normal(0,1);
  to_vector(p_real_arr) ~ normal(0,1);
}
generated quantities {
  // everything is data in GQ
  int gq_int = to_int(p_real);
  array[d_int] int gq_int_arr = to_int(p_real_arr);
}
