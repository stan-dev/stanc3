data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
  vector[d_int] d_vector;
  row_vector[d_int] d_row_vector;

  complex_vector[d_int] d_cvector;
  complex_row_vector[d_int] d_crow_vector;
  complex_matrix[d_int, d_int] d_cmatrix;
}
transformed data {
  row_vector[d_int] transformed_data_row_vector;

  transformed_data_row_vector = columns_dot_self(d_vector);
  transformed_data_row_vector = columns_dot_self(d_row_vector);
  transformed_data_row_vector = columns_dot_self(d_matrix);


  complex_row_vector[d_int] transformed_data_crow_vector;
  transformed_data_crow_vector = columns_dot_self(d_cvector);
  transformed_data_crow_vector = columns_dot_self(d_crow_vector);
  transformed_data_crow_vector = columns_dot_self(d_cmatrix);
}
parameters {
  matrix[d_int,d_int] p_matrix;
  vector[d_int] p_vector;
  row_vector[d_int] p_row_vector;
  real y_p;


  complex_vector[d_int] p_cvector;
  complex_row_vector[d_int] p_crow_vector;
  complex_matrix[d_int, d_int] p_cmatrix;
}
transformed parameters {
  row_vector[d_int] transformed_param_row_vector;

  transformed_param_row_vector = columns_dot_self(d_vector);
  transformed_param_row_vector = columns_dot_self(d_row_vector);
  transformed_param_row_vector = columns_dot_self(d_matrix);

  transformed_param_row_vector = columns_dot_self(p_vector);
  transformed_param_row_vector = columns_dot_self(p_row_vector);
  transformed_param_row_vector = columns_dot_self(p_matrix);

  transformed_param_row_vector = columns_dot_self(d_vector);
  transformed_param_row_vector = columns_dot_self(d_row_vector);
  transformed_param_row_vector = columns_dot_self(d_matrix);

  transformed_param_row_vector = columns_dot_self(p_vector);
  transformed_param_row_vector = columns_dot_self(p_row_vector);
  transformed_param_row_vector = columns_dot_self(p_matrix);

  complex_row_vector[d_int] transformed_param_crow_vector;
    transformed_param_crow_vector = columns_dot_self(d_cvector);
  transformed_param_crow_vector = columns_dot_self(d_crow_vector);
  transformed_param_crow_vector = columns_dot_self(d_cmatrix);

  transformed_param_crow_vector = columns_dot_self(p_cvector);
  transformed_param_crow_vector = columns_dot_self(p_crow_vector);
  transformed_param_crow_vector = columns_dot_self(p_cmatrix);

  transformed_param_crow_vector = columns_dot_self(d_cvector);
  transformed_param_crow_vector = columns_dot_self(d_crow_vector);
  transformed_param_crow_vector = columns_dot_self(d_cmatrix);

  transformed_param_crow_vector = columns_dot_self(p_cvector);
  transformed_param_crow_vector = columns_dot_self(p_crow_vector);
  transformed_param_crow_vector = columns_dot_self(p_cmatrix);
}
model {
  y_p ~ normal(0,1);
}
