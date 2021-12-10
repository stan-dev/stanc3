functions {
    real foo_log(real alpha, real beta){
     return 1.0;
   }

   real foo_t_log(real alpha, real beta){
     return 1.0;
   }
}

data {
  real a;
  vector[3] b;
  real c[7];
  real d[8, 9];
}
parameters {
  real y;
  real e;
  vector[3] f;
  real g[7];
  real h[8, 9];
}
model {
  // old distriubtion suffixes
  target += normal_log(y, 0, 1)
    + normal_cdf_log(2, 0, 1)
    + normal_ccdf_log(3, 0, 1);
  // increment log prob
  increment_log_prob(a);
  increment_log_prob(b);
  increment_log_prob(b);
  increment_log_prob(c);
  increment_log_prob(d);
  increment_log_prob(e);
  increment_log_prob(f);
  increment_log_prob(g);
  increment_log_prob(h);
  // cdf without |
  increment_log_prob(bernoulli_cdf(0,1));
  // getlp
  real x = get_lp();
  // old array syntax
  real xyz[5];
  // future reserved words
  int offset;
  int array;
  // deprecated functions
  real z = multiply_log(3.4,3.5);

  # old comment

  // old assign
  z <- 3;

  // _log UDFs
  target += foo_t_log(1,3);
  1 ~ foo(3);

}

