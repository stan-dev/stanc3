functions {
  tuple(real, real) foo() {
    return (1.0, 2.0);
  }
}
data {
  int<lower=0, upper=1> indicator;
}
parameters {
  tuple(real<lower=0>, real<lower=0>) ps;
  array[3, 2] tuple(real<lower=0>, simplex[4]) ps2;
  real<lower=foo().1, upper=ps.2> t;
  
  array[2] tuple(real, array[indicator ? 3 : 0]
                 tuple(real, simplex[5], cholesky_factor_cov[5, 4])) complicated;
}
