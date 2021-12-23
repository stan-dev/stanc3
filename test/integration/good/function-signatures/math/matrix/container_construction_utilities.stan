data {
  int d_int;
  real d_real;
  array[d_int] int d_int_array;
  array[d_int] real d_real_array;
  matrix[d_int, d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;
}
transformed data {
  matrix[d_int, d_int] transformed_data_matrix;
  array[d_int] int transformed_int_array;
  array[d_int] real transformed_data_real_array;
  row_vector[d_int] transformed_data_row_vector;
  vector[d_int] transformed_data_vector;
  
  transformed_data_matrix = identity_matrix(d_int);
  
  transformed_int_array = linspaced_int_array(d_int, d_int, d_int);
  
  transformed_data_real_array = linspaced_array(d_int, d_real, d_real);
  transformed_data_row_vector = linspaced_row_vector(d_int, d_real, d_real);
  transformed_data_vector = linspaced_vector(d_int, d_real, d_real);
  
  transformed_int_array = one_hot_int_array(d_int, d_int);
  transformed_data_real_array = one_hot_array(d_int, d_int);
  transformed_data_row_vector = one_hot_row_vector(d_int, d_int);
  transformed_data_vector = one_hot_vector(d_int, d_int);
  
  transformed_int_array = ones_int_array(d_int);
  transformed_data_real_array = ones_array(d_int);
  transformed_data_row_vector = ones_row_vector(d_int);
  transformed_data_vector = ones_vector(d_int);
  
  transformed_data_vector = uniform_simplex(d_int);
  
  transformed_int_array = zeros_int_array(d_int);
  transformed_data_real_array = zeros_array(d_int);
  transformed_data_row_vector = zeros_row_vector(d_int);
  transformed_data_vector = zeros_vector(d_int);
}
parameters {
  real y_p;
}
model {
  y_p ~ normal(0, 1);
}

