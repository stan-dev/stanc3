data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
  row_vector[d_int] d_row_vector;

  complex_matrix[d_int,d_int] d_cmatrix;
  complex_row_vector[d_int] d_crow_vector;
}

transformed data {
  row_vector[d_int] transformed_data_row_vector;
  matrix[d_int,d_int] transformed_data_matrix;

  transformed_data_matrix = mdivide_right(d_matrix,d_matrix);
  transformed_data_row_vector = mdivide_right(d_row_vector,d_matrix);

  complex_row_vector[d_int] transformed_data_crow_vector;
  complex_matrix[d_int,d_int] transformed_data_cmatrix;

  transformed_data_cmatrix = mdivide_right(d_cmatrix,d_cmatrix);
  transformed_data_crow_vector = mdivide_right(d_crow_vector,d_cmatrix);
}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
  row_vector[d_int] p_row_vector;
  complex_matrix[d_int,d_int] p_cmatrix;
  complex_row_vector[d_int] p_crow_vector;
}
transformed parameters {
  row_vector[d_int] transformed_param_row_vector;
  matrix[d_int,d_int] transformed_param_matrix;

  transformed_param_matrix = mdivide_right(d_matrix,d_matrix);
  transformed_param_row_vector = mdivide_right(d_row_vector,d_matrix);
  transformed_param_matrix = mdivide_right(p_matrix,d_matrix);
  transformed_param_row_vector = mdivide_right(p_row_vector,d_matrix);
  transformed_param_matrix = mdivide_right(d_matrix,p_matrix);
  transformed_param_row_vector = mdivide_right(d_row_vector,p_matrix);
  transformed_param_matrix = mdivide_right(p_matrix,p_matrix);
  transformed_param_row_vector = mdivide_right(p_row_vector,p_matrix);

  complex_row_vector[d_int] transformed_param_crow_vector;
  complex_matrix[d_int,d_int] transformed_param_cmatrix;

  transformed_param_cmatrix = mdivide_right(d_cmatrix,d_cmatrix);
  transformed_param_crow_vector = mdivide_right(d_crow_vector,d_cmatrix);
  transformed_param_cmatrix = mdivide_right(p_cmatrix,d_cmatrix);
  transformed_param_crow_vector = mdivide_right(p_crow_vector,d_cmatrix);
  transformed_param_cmatrix = mdivide_right(d_cmatrix,p_cmatrix);
  transformed_param_crow_vector = mdivide_right(d_crow_vector,p_cmatrix);
  transformed_param_cmatrix = mdivide_right(p_cmatrix,p_cmatrix);
  transformed_param_crow_vector = mdivide_right(p_crow_vector,p_cmatrix);
}
model {
  y_p ~ normal(0,1);
}
