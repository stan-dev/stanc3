  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//stat_comp_benchmarks_models/gp_pois_regr.stan"
  data {
    int<lower=1> N;
    real x[N];
    int k[N];
  }
  parameters {
    real<lower=0> rho;
    real<lower=0> alpha;
    vector[N] f_tilde;
  }
  transformed parameters {
    vector[N] f;
    {
      matrix[N, N] cov = cov_exp_quad(x, alpha, rho) + diag_matrix(rep_vector(1e-10, N));
      matrix[N, N] L_cov = cholesky_decompose(cov);
      f = L_cov * f_tilde;
    }
  }
  model {
    rho ~ gamma(25, 4);
    alpha ~ normal(0, 2);
    f_tilde ~ normal(0, 1);
    k ~ poisson_log(f);
  }
  

