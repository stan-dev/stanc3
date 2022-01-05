data {
  array[10] vector[5] y;
  array[10] vector[5] mu;
}
parameters {
  array[10] cov_matrix[5] Sigma;
}
model {
  for (i in 1 : 10) 
    y[i] ~ multi_normal(mu[i], Sigma[i]);
}

