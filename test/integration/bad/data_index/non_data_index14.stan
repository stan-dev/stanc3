parameters {
  array[3] real y;

}
transformed parameters {
  cov_matrix[size(y)] z;
}
