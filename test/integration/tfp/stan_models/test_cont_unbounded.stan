data {
}
parameters {
  real p_normal;
  real p_cauchy;
  real p_gumbel;
  real p_student_t;
  real p_double_exponential;
}
model {
  target += normal_lpdf(p_normal | -1, 5);
  target += cauchy_lpdf(p_cauchy | -3, 2);
  target += gumbel_lpdf(p_gumbel | 3, 1);
  target += student_t_lpdf(p_student_t | 2, -1, 3);
  target += double_exponential_lpdf(p_double_exponential | 0, 3);
}