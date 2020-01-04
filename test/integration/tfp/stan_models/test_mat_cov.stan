data {
  cov_matrix[2] S;
} 
parameters {
  cov_matrix[2] W; 
} 
model {
  W ~ wishart(4, S); 
} 