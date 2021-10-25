functions {
  vector rhs(real t, vector y, real alpha) {
    vector[1] yp = -alpha * y;

    return yp;
  }
}

parameters {
  // nyllary
  real e;
  real pi;
  real log2;
  real log10;
  real sqrt2;
  real not_a_number;
  real positive_infinity;
  real negative_infinity;
  real machine_precision;
  // unary
  real inv_logit;
  real logit;
  //binary
  real pow;
  real add;
  real sub;

  //more
  real bernoulli_logit_glm_lpmf;
  real reduce_sum;
  vector[4] segment;
  real ode_bdf;
  real ode_bdf_tol;
}
transformed parameters {
  real mu;
  mu = e() + pi() + log2() + log10() + sqrt2() + not_a_number()
    + positive_infinity() + negative_infinity() + machine_precision();
  mu += logit + bernoulli_logit_glm_lpmf + reduce_sum;

  mu += inv_logit * inv_logit(0.5) + logit * logit(10);

  mu += pow * pow(3,2);

  mu = add + add - sub;

  array[1] vector[4] called = ode_bdf(rhs, segment, 1.0, {3.0}, 3.5);
}
model {
}
