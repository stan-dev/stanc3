parameters {
  real y;
  matrix[10, 10] p_mat;
}
transformed parameters {
  real p;
  matrix[10, 10] tp_mat = rep_matrix(p, 10, 10);
  real r = y * 5.0;
  real t = p * 5.0;
  r = 6.0;
  p = 6.0;
}