data{
int<lower=0>row_size;
int<lower=0>col_size;
sparse_matrix<lower=0>[row_size, col_size]x_matrix;
vector[row_size]y_vector;
}

transformed data {
  sparse_matrix[row_size, col_size] tp_matrix;
 }
parameters{
sparse_matrix[row_size, 1]beta;
}
model{
y_vector~normal(x_matrix*beta,1);
}
