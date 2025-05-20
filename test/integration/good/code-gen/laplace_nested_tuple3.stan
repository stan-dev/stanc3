functions {
  real ll_nested(vector theta,
                 array[] tuple(array[] tuple(real, vector), array[] int) p,
                 real unused) {
    return neg_binomial_2_lpmf(p[1].2 | exp(p[1].1[1].2 + theta),
             p[1].1[1].1);
  }

  matrix K_function(array[] tuple(array[] vector, array[] tuple(int, real)) p,
                    real rho) {
    matrix[p[1].2[1].1, p[1].2[1].1] K = gp_exp_quad_cov(p[1].1, p[1].2[1].2,
                                                         rho);
    for (i in 1 : p[1].2[1].1)
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
  real rho_location_prior;
  real rho_scale_prior;
  real alpha_location_prior;
  real alpha_scale_prior;
}
transformed data {
  vector[n_obs] log_ye = log(ye);

  vector[n_obs] theta_0 = rep_vector(0.0, n_obs); // initial guess

  // control parameters for Laplace approximation
  real tolerance = 1e-6;
  int max_num_steps = 100;
  int hessian_block_size = 1;
  int solver = 1;
  int max_steps_line_search = 0;
}
parameters {
  real<lower=0> alpha;
  real<lower=0> rho;
  real<lower=0> eta;
}
model {
  rho ~ inv_gamma(rho_location_prior, rho_scale_prior);
  alpha ~ inv_gamma(alpha_location_prior, alpha_scale_prior);
  eta ~ normal(0, 1);

  target += laplace_marginal(ll_nested, ({({(eta, log_ye)}, y)}, eta),
                             theta_0, K_function,
                             ({(x, {(n_obs, alpha)})}, rho));
}
generated quantities {
  vector[n_obs] theta3 = laplace_latent_rng(ll_nested,
                           ({({(eta, log_ye)}, y)}, eta), theta_0,
                           K_function, ({(x, {(n_obs, alpha)})}, rho));
}

