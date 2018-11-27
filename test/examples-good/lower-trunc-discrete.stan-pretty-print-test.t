  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//lower-trunc-discrete.stan"
  functions {
    real foo_lpmf(int y, real lambda) {
      return 1.0;
    }
    real foo_lcdf(int y, real lambda) {
      return 1.0;
    }
    real foo_lccdf(int y, real lambda) {
      return 1.0;
    }
    real bar_log(int y, real lambda) {
      return 1.0;
    }
    real bar_cdf_log(int y, real lambda) {
      return 1.0;
    }
    real bar_ccdf_log(int y, real lambda) {
      return 1.0;
    }
    real baz_lpdf(real y, real lambda) {
      return 1.0;
    }
    real baz_lcdf(real y, real lambda) {
      return 1.0;
    }
    real baz_lccdf(real y, real lambda) {
      return 1.0;
    }
    real quux_log(real y, real lambda) {
      return 1.0;
    }
    real quux_cdf_log(real y, real lambda) {
      return 1.0;
    }
    real quux_ccdf_log(real y, real lambda) {
      return 1.0;
    }
  }
  data {
    int N;
    int y[N];
    real u[N];
    int L;
    int U;
  }
  parameters {
    real<lower=0> lambda;
  }
  model {
    for (n in 1 : N) {
      y[n] ~ poisson(lambda) T[L, ];
      y[n] ~ poisson(lambda) T[L, U];
      y[n] ~ poisson(lambda) T[ , U];
      y[n] ~ foo(lambda) T[L, ];
      y[n] ~ foo(lambda) T[L, U];
      y[n] ~ foo(lambda) T[ , U];
      y[n] ~ bar(lambda) T[L, ];
      y[n] ~ bar(lambda) T[L, U];
      y[n] ~ bar(lambda) T[ , U];
      u[n] ~ normal(0, 1) T[L, ];
      u[n] ~ normal(0, 1) T[ , U];
      u[n] ~ normal(0, 1) T[L, U];
      y[n] ~ baz(lambda) T[L, ];
      y[n] ~ baz(lambda) T[L, U];
      y[n] ~ baz(lambda) T[ , U];
      y[n] ~ quux(lambda) T[L, ];
      y[n] ~ quux(lambda) T[L, U];
      y[n] ~ quux(lambda) T[ , U];
    }
  }
  

