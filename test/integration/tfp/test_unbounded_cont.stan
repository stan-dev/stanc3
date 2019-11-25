data {
  int<lower=0> N;
  real y[N];
}
parameters {
  real loc_normal;
  real<lower=0> scale_normal;
  
  real loc_cauchy;
  real<lower=0> scale_cauchy;
  
  real loc_gumbel;
  real<lower=0> scale_gumbel;
  
  real loc_student_t;
  real<lower=0> scale_student_t;
  real<lower=0> nu;
  
  real loc_laplace;
  real<lower=0> scale_laplace;
}
model {
  loc_normal ~ normal(0, 5);
  scale_normal ~ normal(0, 5);
  
  loc_cauchy ~ normal(0, 5);
  scale_cauchy ~ normal(0, 5);
  
  loc_gumbel ~ normal(0, 5);
  scale_gumbel ~ normal(0, 5);
  
  loc_student_t ~ normal(0, 5);
  scale_student_t ~ normal(0, 5);
  nu ~ gamma(2,0.1);

  loc_laplace ~ normal(0, 5);
  scale_laplace ~ normal(0, 5);

  y ~ normal(loc_normal, scale_normal);
  y ~ cauchy(loc_cauchy, scale_cauchy);
  y ~ gumbel(loc_gumbel, scale_gumbel);
  y ~ student_t(nu, loc_student_t, scale_student_t);
  y ~ double_exponential(loc_laplace, scale_laplace);
}