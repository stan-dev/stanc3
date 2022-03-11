data {
  int N;
  real x;
}
parameters {
  vector[3] v;
  array[4,5] real a;
  cholesky_factor_corr[6] m;
  real y;
  real z;
}
model {
  // int_literal
  1 ~ normal(y,1);

  // double_literal
  2.7 ~ normal(z,1);

  // variable
  y ~ normal(0,1);

  // fun
  m ~ lkj_corr_cholesky(2.0);
  (m + m) ~ lkj_corr_cholesky(2.0);
  (m - m) ~ lkj_corr_cholesky(2.0);
  (v + v) ~ normal(0,1);
  (v - v) ~ normal(0,1);
  block(m,1,1,1,1) ~ lkj_corr_cholesky(2.0);
  col(m,1) ~ normal(0,1);
  cols(m) ~ normal(0,1);
  row(m,1) ~ normal(0,1);
  rows(m) ~ normal(0,1);
  diagonal(m) ~ normal(0,1);
  head(v,2) ~ normal(0,1);
  negative_infinity() ~ normal(0,1);
  not_a_number() ~ normal(0,1);
  rep_matrix(1,3,3) ~ lkj_corr_cholesky(2.0);
  (v')' ~ normal(0,1);
  positive_infinity() ~ normal(0,1);
  segment(v,2,4) ~ normal(0,1);
  sum(v) ~ normal(0,1);
  tail(v,3) ~ normal(0,1);
  to_vector(m) ~ normal(0,1);

  // index_op
  v[1] ~ normal(0,1);
  m[1] ~ normal(0,1);
  m[1,2] ~ normal(0,1);
  a[1,2] ~ normal(0,1);
  a[1][2] ~ normal(0,1);

  // binary_op
  y + z ~ normal(0,1);
  y - z ~ normal(0,1);
  1 * z ~ normal(0,1);
  z * 1 ~ normal(0,1);
  1 / (1 / z) ~ normal(0,1);
  y + ((z / 2) * 3) ~ normal(0,1);
  2.0 * 3 ~ normal(y,1);

  // unary_op
  (-y) ~ normal(0,1);
  -(-y) ~ normal(0,1);

  // literals
  [1] ~ normal(0,1);
  [y + y] ~ normal(0,1);
  to_vector({1}) ~ normal(0,1);

}
