parameters {
  real e;
  real pi;
  real log2;
  real log10;
  real sqrt2;
  real not_a_number;
  real positive_infinity;
  real negative_infinity;
  real machine_precision;
  real inv_logit;
  real logit;
  real bernoulli_logit_glm_lpmf;
  real reduce_sum;
  real ode_bdf;
  real ode_bdf_tol;
}
transformed parameters {
  real mu;
  mu = e() + pi() + log2() + log10() + sqrt2() + not_a_number()
    + positive_infinity() + negative_infinity() + machine_precision();
  mu += logit + bernoulli_logit_glm_lpmf + reduce_sum;
}
model {
}
