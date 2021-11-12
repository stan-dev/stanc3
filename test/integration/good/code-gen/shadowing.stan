functions {
  vector rhs(real t, vector y, real alpha) {
    vector[1] yp = -alpha * y;

    return yp;
  }
}

data {
   vector[4] x;
}
parameters {
  // nullary
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
  real num_elements;

  //binary
  real pow;
  real add;
  real sub;
  real multiply;
  real binomial_coefficient_log;

  // try to break internal functions
  real<lower=0> read_constrain_lb;
  real read;
  real validate_non_negative_index;
  real length;
  simplex[5] validate_positive_index;
  real profile_map;
  real assign;
  real rvalue;
  real stan_print;
  real model_base_crtp;
  real index_uni;

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

  mu += binomial_coefficient_log * lchoose(3,4) * binomial_coefficient_log(3,4);

  array[1] vector[4] called = ode_bdf(rhs, segment, 1.0, {3.0}, 3.5);


  vector[num_elements(x)] result = x * 5.0;

  profile("shadow-1") {
    mu += profile_map;
  }
}
model {
  print("hello world");
}
