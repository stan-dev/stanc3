  $ $TESTDIR/..//../../_build/default/stanc.exe --auto-format "$TESTDIR/..//services/bym2_offset_only.stan"
  data {
    int<lower=0> N;
    int<lower=0> N_edges;
    int<lower=1, upper=N> node1[N_edges];
    int<lower=1, upper=N> node2[N_edges];
    int<lower=0> y[N];
    vector<lower=0>[N] E;
    real<lower=0> scaling_factor;
  }
  transformed data {
    vector[N] log_E = log(E);
  }
  parameters {
    real beta0;
    real<lower=0> sigma;
    real<lower=0, upper=1> rho;
    vector[N] theta;
    vector[N - 1] phi_raw;
  }
  transformed parameters {
    vector[N] phi;
    vector[N] convolved_re;
    phi[1 : (N - 1)] = phi_raw;
    phi[N] = -sum(phi_raw);
    convolved_re = sqrt(1 - rho) * theta + sqrt(rho / scaling_factor) * phi;
  }
  model {
    y ~ poisson_log(log_E + beta0 + convolved_re * sigma);
    target += -0.5 * dot_self(phi[node1] - phi[node2]);
    beta0 ~ normal(0.0, 2.5);
    theta ~ normal(0.0, 1.0);
    sigma ~ normal(0, 5);
    rho ~ beta(0.5, 0.5);
  }
  generated quantities {
    real log_precision = -2.0 * log(sigma);
    real logit_rho = log(rho / (1.0 - rho));
    vector[N] eta = log_E + beta0 + convolved_re * sigma;
    vector[N] mu = exp(eta);
    vector[N] log_lik;
    int y_rep[N];
    if (max(eta) > 20) {
      print("max eta too big: ", max(eta));
      for (n in 1 : N) {
        y_rep[n] = -1;
        log_lik[n] = not_a_number();
      }
    }
    else {
      for (n in 1 : N) {
        y_rep[n] = poisson_log_rng(eta[n]);
        log_lik[n] = poisson_log_lpmf(y[n]| eta[n]);
      }
    }
  }
  

