data {
  int d_int;
  array[1] int d_int_array;
  array[1, 2] int d_int_array_2d;
  array[1, 2, 3] int d_int_array_3d;
  int r_int;
  array[1] int r_int_array;
  array[1, 2] int r_int_array_2d;
  array[1, 2, 3] int r_int_array_3d;
}
transformed data {
  array[1] int transformed_data_int_array;
  array[1, 2] int transformed_data_int_array_2d;
  array[1, 2, 3] int transformed_data_int_array_3d;
  
  transformed_data_int_array = choose(r_int, d_int_array);
  transformed_data_int_array = choose(r_int_array, d_int);
  transformed_data_int_array = choose(r_int_array, d_int_array);
  
  transformed_data_int_array_2d = choose(r_int, d_int_array_2d);
  transformed_data_int_array_2d = choose(r_int_array_2d, d_int);
  transformed_data_int_array_2d = choose(r_int_array_2d, d_int_array_2d);
  
  transformed_data_int_array_3d = choose(r_int, d_int_array_3d);
  transformed_data_int_array_3d = choose(r_int_array_3d, d_int);
  transformed_data_int_array_3d = choose(r_int_array_3d, d_int_array_3d);
}
parameters {
  real y_p;
}
model {
  y_p ~ normal(0, 1);
}

