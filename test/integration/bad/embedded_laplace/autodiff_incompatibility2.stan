functions {

vector sir(real t, vector y, real beta, real gamma, int N) {
  real S = y[1];
  real I = y[2];
  real R = y[3];

  real dS_dt = -beta * I * S / N;
  real dI_dt = beta * I * S / N - gamma * I;
  real dR_dt = gamma * I;

  return [dS_dt, dI_dt, dR_dt]';
}


  real helper(){
    // bad:
    if (0) {
      array[1] vector[3] y = ode_rk45(sir, [1.0]', 1.0, {1.0}, .5, .5, 10);
    }
    return 0;
  }

  // specify negative binomial likelihood with mean offset
  real ll_function(vector theta, // latent Gaussian
                   real eta,
                   vector log_ye, // mean offset
                   array[] int y) {
    // observed count
    return neg_binomial_2_lpmf(y | exp(log_ye + theta), eta) + helper();
  }

  // specify covariance function
  matrix K_function(array[] vector x, int n_obs, real alpha, real rho) {
    matrix[n_obs, n_obs] K = gp_exp_quad_cov(x, alpha, rho);
    for (i in 1 : n_obs)
      K[i, i] += 1e-8;
    return K;
  }
}
data {
  int n_obs;
  int n_coordinates;
  array[n_obs] int y;
  vector[n_obs] ye;
  array[n_obs] vector[n_coordinates] x;
}

transformed data {
  vector[n_obs] log_ye = log(ye);
}
parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> eta;
}

generated quantities {
  vector[n_obs] theta = laplace_latent_rng(ll_function, (eta, log_ye, y),
                        K_function, (x, n_obs, alpha, rho));
}
