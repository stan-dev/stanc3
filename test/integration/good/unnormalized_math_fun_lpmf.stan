functions {
  real foo_lpmf(int i1, real r1, matrix m1, vector v1, array[] int ai1,
                row_vector rv1) {
    real r;
    r += bernoulli_logit_glm_lupmf(i1 | m1, r1, v1);
    r += bernoulli_logit_lupmf(i1 | r1);
    r += bernoulli_lupmf(i1 | r1);
    r += beta_binomial_lupmf(i1 | i1, r1, r1);
    r += binomial_logit_lupmf(i1 | i1, r1);
    r += binomial_lupmf(i1 | i1, r1);
    r += categorical_logit_glm_lupmf(i1 | rv1, v1, m1);
    r += categorical_logit_lupmf(i1 | v1);
    r += categorical_lupmf(i1 | v1);
    r += hypergeometric_lupmf(i1 | i1, i1, i1);
    r += multinomial_lupmf(ai1 | v1);
    r += neg_binomial_2_log_glm_lupmf(i1 | m1, r1, v1, r1);
    r += neg_binomial_2_log_lupmf(i1 | r1, r1);
    r += neg_binomial_2_lupmf(i1 | r1, r1);
    r += neg_binomial_lupmf(i1 | r1, r1);
    r += ordered_logistic_glm_lupmf(i1 | rv1, v1, v1);
    r += ordered_logistic_lupmf(i1 | r1, v1);
    r += ordered_probit_lupmf(i1 | r1, v1);
    r += poisson_log_glm_lupmf(i1 | m1, r1, v1);
    r += poisson_log_lupmf(i1 | r1);
    r += poisson_lupmf(i1 | r1);
    return r;
  }
}
data {
  int i1;
  array[5] int ai1;
}
parameters {
  real r1;
  vector[5] v1;
  row_vector[5] rv1;
  matrix[5, 5] m1;
}
model {
  real r;
  r += bernoulli_logit_glm_lupmf(i1 | m1, r1, v1);
  r += bernoulli_logit_lupmf(i1 | r1);
  r += bernoulli_lupmf(i1 | r1);
  r += beta_binomial_lupmf(i1 | i1, r1, r1);
  r += binomial_logit_lupmf(i1 | i1, r1);
  r += binomial_lupmf(i1 | i1, r1);
  r += categorical_logit_glm_lupmf(i1 | rv1, v1, m1);
  r += categorical_logit_lupmf(i1 | v1);
  r += categorical_lupmf(i1 | v1);
  r += hypergeometric_lupmf(i1 | i1, i1, i1);
  r += multinomial_lupmf(ai1 | v1);
  r += neg_binomial_2_log_glm_lupmf(i1 | m1, r1, v1, r1);
  r += neg_binomial_2_log_lupmf(i1 | r1, r1);
  r += neg_binomial_2_lupmf(i1 | r1, r1);
  r += neg_binomial_lupmf(i1 | r1, r1);
  r += ordered_logistic_glm_lupmf(i1 | rv1, v1, v1);
  r += ordered_logistic_lupmf(i1 | r1, v1);
  r += ordered_probit_lupmf(i1 | r1, v1);
  r += poisson_log_glm_lupmf(i1 | m1, r1, v1);
  r += poisson_log_lupmf(i1 | r1);
  r += poisson_lupmf(i1 | r1);
  r += foo_lupmf(i1 | r1, m1, v1, ai1, rv1);
}

