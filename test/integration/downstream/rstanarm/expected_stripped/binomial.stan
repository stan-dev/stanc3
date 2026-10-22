/* $ stanc --include-paths=.. --auto-format --allow-undefined --canonicalize=strip-comments ../binomial.stan : [exit 0] */
#include /pre/Columbia_copyright.stan
#include /pre/license.stan

functions {
  #include /functions/common_functions.stan
  #include /functions/binomial_likelihoods.stan
}
data {
  #include /data/NKX.stan
  
  array[N] int<lower=0> y;
  array[N] int<lower=0> trials;
  
  #include /data/data_glm.stan
  
  #include /data/weights_offset.stan
  
  int<lower=5, upper=5> family;
  
  #include /data/hyperparameters.stan
  
  #include /data/glmer_stuff.stan
  
  #include /data/glmer_stuff2.stan
}
transformed data {
  real aux = not_a_number();
  array[special_case ? t : 0, N] int<lower=1> V = make_V(N,
                                                         special_case ? t : 0,
                                                         v);
  
  #include /tdata/tdata_glm.stan
}
parameters {
  array[has_intercept] real<upper=(link == 4 ? 0.0 : positive_infinity())> gamma;
  
  #include /parameters/parameters_glm.stan
}
transformed parameters {
  #include /tparameters/tparameters_glm.stan
  
  if (t > 0) {
    if (special_case == 1) {
      int start = 1;
      theta_L = scale .* tau;
      if (t == 1) 
        b = theta_L[1] * z_b;
      else 
        for (i in 1 : t) {
          int end = start + l[i] - 1;
          b[start : end] = theta_L[i] * z_b[start : end];
          start = end + 1;
        }
    } else {
      theta_L = make_theta_L(len_theta_L, p, 1.0, tau, scale, zeta, rho, z_T);
      b = make_b(z_b, theta_L, p, l);
    }
  }
}
model {
  #include /model/make_eta.stan
  
  if (t > 0) {
    #include /model/eta_add_Zb.stan
  }
  if (has_intercept == 1) {
    if (link != 4) 
      eta += gamma[1];
    else 
      eta += gamma[1] - max(eta);
  } else {
    #include /model/eta_no_intercept.stan
  }
  
  if (has_weights == 0 && prior_PD == 0) {
    real dummy = ll_binom_lp(y, trials, eta, link);
  } else if (prior_PD == 0) 
    target += dot_product(weights, pw_binom(y, trials, eta, link));
  
  #include /model/priors_glm.stan
  
  if (t > 0) {
    real dummy = decov_lp(z_b, z_T, rho, zeta, tau, regularization, delta,
                          shape, t, p);
  }
}
generated quantities {
  real mean_PPD = compute_mean_PPD ? 0 : negative_infinity();
  array[has_intercept] real alpha;
  
  if (has_intercept == 1) {
    if (dense_X) 
      alpha[1] = gamma[1] - dot_product(xbar, beta);
    else 
      alpha[1] = gamma[1];
  }
  
  if (compute_mean_PPD) {
    vector[N] pi;
    #include /model/make_eta.stan
    
    if (t > 0) {
      #include /model/eta_add_Zb.stan
    }
    if (has_intercept == 1) {
      if (link != 4) 
        eta += gamma[1];
      else {
        real shift = max(eta);
        eta += gamma[1] - shift;
        alpha[1] -= shift;
      }
    } else {
      #include /model/eta_no_intercept.stan
    }
    
    pi = linkinv_binom(eta, link);
    for (n in 1 : N) 
      mean_PPD += binomial_rng(trials[n], pi[n]);
    mean_PPD /= N;
  }
}

/*
warning: Unreachable statement found, is this intended?
    ┌─ ..//functions/binomial_likelihoods.stan:15:5
  8 │    vector linkinv_binom(vector eta, int link) {
  9 │      if (link == 1)      return(inv_logit(eta)); // logit
    │                          ----------------------- exits the function
 10 │      else if (link == 2) return(Phi(eta)); // probit
    │                          ----------------- exits the function
 11 │      else if (link == 3) return(atan(eta) / pi() + 0.5);  // cauchit
    │                          ------------------------------- exits the function
 12 │      else if (link == 4) return(exp(eta)); // log
    │                          ----------------- exits the function
 13 │      else if (link == 5) return(inv_cloglog(eta)); // cloglog
    │                          ------------------------- exits the function
 14 │      else reject("Invalid link");
    │           ----------------------- stops this model evaluation
 15 │      return eta; // never reached
    │      ^ Never reached
 16 │    }
    ┌─ ../binomial.stan:7:3
  6 │    #include </functions/common_functions.stan>
  7 │    #include </functions/binomial_likelihoods.stan>
    │    -------- file '..//functions/binomial_likelihoods.stan' included here
  8 │  }
*/
