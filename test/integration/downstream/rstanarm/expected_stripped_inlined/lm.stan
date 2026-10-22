/* $ stanc --include-paths=.. --auto-format --allow-undefined --canonicalize=strip-comments,includes ../lm.stan : [exit 0] */
functions {
  real ll_mvn_ols_qr_lp(vector theta, vector b, real intercept, real ybar,
                        real SSR, real sigma, int N) {
    target += -0.5
              * (dot_self(theta - b) + N * square(intercept - ybar) + SSR)
              / square(sigma) - N * (log(sigma) + 0.91893853320467267);
    return target();
  }
}
data {
  int<lower=0, upper=1> has_intercept;
  int<lower=0, upper=1> prior_dist_for_intercept;
  real<lower=0> prior_scale_for_intercept;
  real prior_mean_for_intercept;
  int<lower=0, upper=1> prior_dist;
  int<lower=0, upper=1> prior_PD;
  real<lower=0> eta;
  
  int<lower=1> J;
  
  array[J] int<lower=1> N;
  int<lower=1, upper=min(N)> K;
  array[J] vector[K] xbarR_inv;
  array[J] real ybar;
  real center_y;
  array[J] real<lower=0> s_Y;
  array[J] vector[K] Rb;
  array[J] real<lower=0> SSR;
  array[J] matrix[K, K] R_inv;
}
transformed data {
  real half_K = 0.5 * K;
  array[J] real sqrt_inv_N;
  array[J] real sqrt_Nm1;
  for (j in 1 : J) {
    sqrt_inv_N[j] = sqrt(1.0 / N[j]);
    sqrt_Nm1[j] = sqrt(N[j] - 1.0);
  }
}
parameters {
  array[K > 1 ? J : 0] unit_vector[K] u;
  array[J * has_intercept] real z_alpha;
  array[J] real<lower=(K > 1 ? 0 : -1), upper=1> R2;
  vector[J * (1 - prior_PD)] log_omega;
}
transformed parameters {
  array[J * has_intercept] real alpha;
  array[J] vector[K] theta;
  array[J] real<lower=0> sigma;
  for (j in 1 : J) {
    real Delta_y = prior_PD == 0 ? s_Y[j] * exp(log_omega[j]) : 1;
    
    if (K > 1) 
      theta[j] = u[j] * sqrt(R2[j]) * sqrt_Nm1[j] * Delta_y;
    else 
      theta[j][1] = R2[j] * sqrt_Nm1[j] * Delta_y;
    
    sigma[j] = Delta_y * sqrt(1 - R2[j]);
    
    if (has_intercept == 1) {
      if (prior_dist_for_intercept == 0) 
        alpha[j] = z_alpha[j];
      else if (prior_scale_for_intercept == 0) 
        alpha[j] = z_alpha[j] * Delta_y * sqrt_inv_N[j]
                   + prior_mean_for_intercept;
      else 
      
        alpha[j] = z_alpha[j] * prior_scale_for_intercept
                   + prior_mean_for_intercept;
    }
  }
}
model {
  for (j in 1 : J) {
    if (prior_PD == 0) {
      real dummy;
      real shift;
      shift = dot_product(xbarR_inv[j], theta[j]);
      dummy = ll_mvn_ols_qr_lp(theta[j], Rb[j],
                               has_intercept == 1 ? alpha[j] + shift : shift,
                               ybar[j], SSR[j], sigma[j], N[j]);
    }
  }
  if (has_intercept == 1 && prior_dist_for_intercept > 0) 
    target += normal_lpdf(z_alpha | 0, 1);
  if (prior_dist == 1) {
    if (K > 1) 
      target += beta_lpdf(R2 | half_K, eta);
    else 
      target += beta_lpdf(square(R2) | half_K, eta) + sum(log(abs(R2)));
  }
}
generated quantities {
  array[J] real mean_PPD;
  array[J] vector[K] beta;
  for (j in 1 : J) {
    real shift;
    shift = dot_product(xbarR_inv[j], theta[j]);
    mean_PPD[j] = normal_rng(has_intercept == 1 ? alpha[j] + shift : shift,
                             sigma[j] * sqrt_inv_N[j]);
    beta[j] = R_inv[j] * theta[j];
  }
}

