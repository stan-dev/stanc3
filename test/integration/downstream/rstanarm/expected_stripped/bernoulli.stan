/* $ stanc --include-paths=.. --auto-format --allow-undefined --canonicalize=strip-comments ../bernoulli.stan : [exit 0] */
#include /pre/Columbia_copyright.stan
#include /pre/license.stan

functions {
  #include /functions/common_functions.stan
  #include /functions/bernoulli_likelihoods.stan
}
data {
  int<lower=0> K;
  array[2] int<lower=1> N;
  vector[K] xbar;
  int<lower=0, upper=1> dense_X;
  array[dense_X] matrix[N[1], K] X0;
  array[dense_X] matrix[N[2], K] X1;
  
  int<lower=0, upper=1> clogit;
  int<lower=0> J;
  array[clogit == 1 ? N[1] + N[2] : 0] int<lower=1, upper=J> strata;
  
  int<lower=0> nnz_X0;
  vector[nnz_X0] w_X0;
  array[nnz_X0] int<lower=0, upper=K - 1> v_X0;
  
  array[dense_X ? 0 : N[1] + 1] int<lower=0, upper=rows(w_X0) + 1> u_X0;
  int<lower=0> nnz_X1;
  vector[nnz_X1] w_X1;
  array[nnz_X1] int<lower=0, upper=K - 1> v_X1;
  
  array[dense_X ? 0 : N[2] + 1] int<lower=0, upper=rows(w_X1) + 1> u_X1;
  
  #include /data/data_glm.stan
  
  int<lower=0> K_smooth;
  matrix[N[1], K_smooth] S0;
  matrix[N[2], K_smooth] S1;
  array[K_smooth] int<lower=1> smooth_map;
  
  int<lower=5, upper=5> family;
  
  int<lower=0, upper=1> has_weights;
  vector[has_weights ? N[1] : 0] weights0;
  vector[has_weights ? N[2] : 0] weights1;
  
  int<lower=0, upper=1> has_offset;
  vector[has_offset ? N[1] : 0] offset0;
  vector[has_offset ? N[2] : 0] offset1;
  
  #include /data/hyperparameters.stan
  
  #include /data/glmer_stuff.stan
  
  array[2] int<lower=0> num_non_zero;
  vector[num_non_zero[1]] w0;
  vector[num_non_zero[2]] w1;
  array[num_non_zero[1]] int<lower=0, upper=q - 1> v0;
  array[num_non_zero[2]] int<lower=0, upper=q - 1> v1;
  
  array[t > 0 ? N[1] + 1 : 0] int<lower=0, upper=rows(w0) + 1> u0;
  
  array[t > 0 ? N[2] + 1 : 0] int<lower=0, upper=rows(w1) + 1> u1;
  int<lower=0, upper=1> special_case;
}
transformed data {
  int NN = N[1] + N[2];
  real aux = not_a_number();
  array[special_case ? t : 0, N[1]] int<lower=1> V0 = make_V(N[1],
                                                             special_case ? t
                                                             : 0, v0);
  array[special_case ? t : 0, N[2]] int<lower=1> V1 = make_V(N[2],
                                                             special_case ? t
                                                             : 0, v1);
  array[clogit ? J : 0] int<lower=0> successes;
  array[clogit ? J : 0] int<lower=0> failures;
  array[clogit ? J : 0] int<lower=0> observations;
  
  #include /tdata/tdata_glm.stan
  
  for (j in 1 : J) {
    successes[j] = 0;
    failures[j] = 0;
  }
  if (J > 0) 
    for (i in 1 : N[2]) 
      successes[strata[i]] += 1;
  if (J > 0) 
    for (i in (N[2] + 1) : NN) 
      failures[strata[i]] += 1;
  for (j in 1 : J) 
    observations[j] = failures[j] + successes[j];
}
parameters {
  array[has_intercept] real<upper=(link == 4 ? 0.0 : positive_infinity())> gamma;
  
  #include /parameters/parameters_glm.stan
}
transformed parameters {
  #include /tparameters/tparameters_glm.stan
  
  if (t > 0) {
    if (special_case) {
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
  #include /model/make_eta_bern.stan
  
  if (has_intercept == 1) {
    if (link != 4) {
      eta0 += gamma[1];
      eta1 += gamma[1];
    } else {
      real shift = fmax(max(eta0), max(eta1));
      eta0 += gamma[1] - shift;
      eta1 += gamma[1] - shift;
    }
  }
  
  if (clogit && prior_PD == 0) {
    real dummy = ll_clogit_lp(eta0, eta1, successes, failures, observations);
  } else if (has_weights == 0 && prior_PD == 0) {
    real dummy = ll_bern_lp(eta0, eta1, link, N);
  } else if (prior_PD == 0) {
    target += dot_product(weights0, pw_bern(0, eta0, link));
    target += dot_product(weights1, pw_bern(1, eta1, link));
  }
  
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
    vector[N[1]] pi0;
    vector[N[2]] pi1;
    
    #include /model/make_eta_bern.stan
    
    if (has_intercept == 1) {
      if (link != 4) {
        eta0 += gamma[1];
        eta1 += gamma[1];
      } else {
        real shift;
        shift = fmax(max(eta0), max(eta1));
        eta0 += gamma[1] - shift;
        eta1 += gamma[1] - shift;
        alpha[1] -= shift;
      }
    }
    if (clogit) 
      for (j in 1 : J) 
        mean_PPD += successes[j];
    else {
      pi0 = linkinv_bern(eta0, link);
      pi1 = linkinv_bern(eta1, link);
      for (n in 1 : N[1]) 
        mean_PPD += bernoulli_rng(pi0[n]);
      for (n in 1 : N[2]) 
        mean_PPD += bernoulli_rng(pi1[n]);
    }
    mean_PPD /= NN;
  }
}

/*
warning: Unreachable statement found, is this intended?
    ┌─ ..//functions/bernoulli_likelihoods.stan:16:5
  9 │    vector linkinv_bern(vector eta, int link) {
 10 │      if (link == 1)      return(inv_logit(eta)); // logit
    │                          ----------------------- exits the function
 11 │      else if (link == 2) return(Phi(eta)); // probit
    │                          ----------------- exits the function
 12 │      else if (link == 3) return(atan(eta) / pi() + 0.5);  // cauchit
    │                          ------------------------------- exits the function
 13 │      else if (link == 4) return(exp(eta)); // log
    │                          ----------------- exits the function
 14 │      else if (link == 5) return(inv_cloglog(eta)); // cloglog
    │                          ------------------------- exits the function
 15 │      else reject("Invalid link");
    │           ----------------------- stops this model evaluation
 16 │      return eta; // never reached
    │      ^ Never reached
 17 │    }
    ┌─ ../bernoulli.stan:7:3
  6 │    #include </functions/common_functions.stan>
  7 │    #include </functions/bernoulli_likelihoods.stan>
    │    -------- file '..//functions/bernoulli_likelihoods.stan' included here
  8 │  }
warning: Unreachable statement found, is this intended?
    ┌─ ..//functions/bernoulli_likelihoods.stan:97:5
 89 │          if (D_j == 1) return eta_j[N_j];
 90 │          return sum(segment(eta_j, N_j - 1, 2));
    │          --------------------------------------- exits the function
 91 │        }
 92 │        else {
 93 │          int N_jm1 = N_j - 1;
 94 │ ╭        return log_sum_exp(log_clogit_denom(N_jm1, D_j, eta_j),
 95 │ │                           log_clogit_denom(N_jm1, D_j - 1, eta_j) + eta_j[N_j]);
    │ ╰────────────────────────────────────────────────────────────────────────────────' exits the function
 96 │        }
 97 │        return not_a_number();  // never reaches
    │        ^ Never reached
 98 │      }
    ┌─ ../bernoulli.stan:7:3
  6 │      #include </functions/common_functions.stan>
  7 │      #include </functions/bernoulli_likelihoods.stan>
    │      -------- file '..//functions/bernoulli_likelihoods.stan' included here
  8 │    }
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/bernoulli_likelihoods.stan:84:8
 83 │     */
 84 │    real log_clogit_denom(int N_j, int D_j, vector eta_j);
    │         ^^^^^^^^^^^^^^^^ Forward declaration.
 85 │    real log_clogit_denom(int N_j, int D_j, vector eta_j) {
    ┌─ ../bernoulli.stan:7:3
  6 │    #include </functions/common_functions.stan>
  7 │    #include </functions/bernoulli_likelihoods.stan>
    │    -------- file '..//functions/bernoulli_likelihoods.stan' included here
  8 │  }
*/
