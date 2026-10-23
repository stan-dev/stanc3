/* $ stanc --include-paths=.. --auto-format --allow-undefined ../jm.stan : [exit 0] */
#include /pre/Columbia_copyright.stan
#include /pre/Brilleman_copyright.stan
#include /pre/license.stan

// Shared parameter joint model
functions {
  #include /functions/common_functions.stan
  #include /functions/bernoulli_likelihoods.stan
  #include /functions/binomial_likelihoods.stan
  #include /functions/continuous_likelihoods.stan
  #include /functions/count_likelihoods.stan
  #include /functions/mvmer_functions.stan
  #include /functions/jm_functions.stan
}
data {
  // declares: M, has_aux, has_weights, resp_type, intercept_type,
  //   yNobs, yNeta, yK, t, p, l, q, len_theta_L, bN1, bK1, bK1_len
  //   bK1_idx, bN2, bK2, bK2_len, bK2_idx
  #include /data/dimensions_mvmer.stan
  
  // declares: yInt{1,2,3}, yReal{1,2,3}, yX{1,2,3}, yXbar{1,2,3},
  //   family, link, y{1,2,3}_Z{1,2}, y{1,2,3}_Z{1,2}_id,
  //   y_prior_dist{_for_intercept,_for_aux,_for_cov}, prior_PD
  #include /data/data_mvmer.stan
  
  // declares: e_prior_dist{_for_intercept,_for_aux},
  //   Npat, Nevents, qnodes, Npat_times_qnodes, qwts,
  //   basehaz_{type,df,X}, nrow_e_Xq, e_has_intercept, nrow_e_Xq,
  //   e_{K,Xq,times,xbar,weights,weights_rep}
  #include /data/data_event.stan
  
  // declares: a_{K,xbar}, a_prior_dist, assoc, assoc_uses, has_assoc,
  //   {sum_}size_which_b, which_b_zindex, {sum_}size_which_coef,
  //   which_coef_{zindex,xindex}, a_K_data, y_Xq_{eta,eps,lag,auc,data},
  //   {sum_,sum_size_}which_interactions, idx_q,
  //   nrow_y_Xq{_auc}, auc_{qnodes,qwts}, has_grp, grp_assoc, grp_idx,
  //   y{1,2,3}_xq_{eta,eps,auc}, y{1,2,3}_z{1,2}q_{eta,eps,auc},
  //   y{1,2,3}_z{1,2}q_id_{eta,eps,auc}
  #include /data/data_assoc.stan
  
  // declares: e_prior_{mean,scale,df}{_for_intercept,for_aux},
  //   e_global_prior_{scale,df}
  #include /data/hyperparameters_mvmer.stan
  #include /data/hyperparameters_event.stan
  // declares: a_prior_{mean,scale,df}, a_global_prior_{scale,df}
  #include /data/hyperparameters_assoc.stan
}
transformed data {
  int<lower=0> e_hs = get_nvars_for_hs(e_prior_dist);
  int<lower=0> a_hs = get_nvars_for_hs(a_prior_dist);
  
  // declares: yHs{1,2,3}, len_{z_T,var_group,rho}, pos, delta,
  //   bCov{1,2}_idx, {sqrt,log,sum_log}_y{1,2,3},
  #include /tdata/tdata_mvmer.stan
}
parameters {
  // declares: yGamma{1,2,3}, z_yBeta{1,2,3}, z_b, z_T, rho,
  //   zeta, tau, bSd{1,2}, z_bMat{1,2}, bCholesky{1,2},
  //   yAux{1,2,3}_unscaled, yGlobal{1,2,3}, yLocal{1,2,3},
  //   yOol{1,2,3}, yMix{1,2,3}
  #include /parameters/parameters_mvmer.stan
  
  // declares e_{gamma,z_beta,aux_unscaled,global,local,mix,ool}
  #include /parameters/parameters_event.stan
  
  // declares a_{z_beta,global,local,mix,ool}
  #include /parameters/parameters_assoc.stan
}
transformed parameters {
  vector[e_K] e_beta; // log hazard ratios
  vector[a_K] a_beta; // assoc params
  vector[basehaz_df] e_aux; // basehaz params
  
  //---- Parameters for longitudinal submodels
  
  // declares and defines: yBeta{1,2,3}, yAux{1,2,3}, yAuxMaximum,
  //   theta_L, bMat{1,2}
  #include /tparameters/tparameters_mvmer.stan
  //---- Parameters for event submodel
  e_beta = make_beta(e_z_beta, e_prior_dist, e_prior_mean, e_prior_scale,
                     e_prior_df, e_global_prior_scale, e_global, e_local,
                     e_ool, e_mix, rep_array(1.0, 0), 0, e_slab_scale,
                     e_caux);
  a_beta = make_beta(a_z_beta, a_prior_dist, a_prior_mean, a_prior_scale,
                     a_prior_df, a_global_prior_scale, a_global, a_local,
                     a_ool, a_mix, rep_array(1.0, 0), 0, a_slab_scale,
                     a_caux);
  e_aux = make_basehaz_coef(e_aux_unscaled, e_prior_dist_for_aux,
            e_prior_mean_for_aux, e_prior_scale_for_aux);
}
model {
  //---- Log likelihoods for longitudinal submodels
  #include /model/mvmer_lp.stan
  
  //---- Log likelihood for event submodel (GK quadrature)
  {
    vector[nrow_e_Xq] e_eta_q; // eta for event submodel (at event and quad times)
    
    // Event submodel: linear predictor at event and quad times
    if (e_K > 0) 
      e_eta_q = e_Xq * e_beta;
    else 
      e_eta_q = rep_vector(0.0, nrow_e_Xq);
    if (assoc == 1) {
      // declares y_eta_q{_eps,_lag,_auc}, y_eta_qwide{_eps,_lag,_auc},
      //   y_q_wide{_eps,_lag,_auc}, mark{2,3}
      #include /model/assoc_evaluate.stan
    }
    
    {
      // declares log_basehaz, log_{haz_q,haz_etimes,surv_etimes,event}
      // increments target with event log-lik
      #include /model/event_lp.stan
    }
  }
  
  //---- Log priors
  // increments target with mvmer priors
  #include /model/priors_mvmer.stan
  beta_lp(e_z_beta, e_prior_dist, e_prior_scale, e_prior_df,
          e_global_prior_df, e_local, e_global, e_mix, e_ool, e_slab_df,
          e_caux);
  beta_lp(a_z_beta, a_prior_dist, a_prior_scale, a_prior_df,
          a_global_prior_df, a_local, a_global, a_mix, a_ool, a_slab_df,
          a_caux);
  basehaz_lp(e_aux_unscaled, e_prior_dist_for_aux, e_prior_scale_for_aux,
             e_prior_df_for_aux);
  if (e_has_intercept == 1) 
    gamma_lp(e_gamma[1], e_prior_dist_for_intercept,
             e_prior_mean_for_intercept, e_prior_scale_for_intercept,
             e_prior_df_for_intercept);
}
generated quantities {
  real e_alpha; // transformed intercept for event submodel
  
  // declares and defines: mean_PPD, yAlpha{1,2,3}, b{1,2}, bCov{1,2}
  #include /gqs/gen_quantities_mvmer.stan
  
  // norm_const is a constant shift in log baseline hazard
  if (e_has_intercept == 1) 
    e_alpha = e_gamma[1] + norm_const - dot_product(e_xbar, e_beta)
              - dot_product(a_xbar, a_beta);
  else 
    e_alpha = norm_const - dot_product(e_xbar, e_beta)
              - dot_product(a_xbar, a_beta);
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
    ┌─ ../jm.stan:8:1
  7 │  #include /functions/common_functions.stan
  8 │  #include /functions/bernoulli_likelihoods.stan
    │  -------- file '..//functions/bernoulli_likelihoods.stan' included here
  9 │  #include /functions/binomial_likelihoods.stan
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
    ┌─ ../jm.stan:8:1
  7 │    #include /functions/common_functions.stan
  8 │    #include /functions/bernoulli_likelihoods.stan
    │    -------- file '..//functions/bernoulli_likelihoods.stan' included here
  9 │    #include /functions/binomial_likelihoods.stan
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
    ┌─ ../jm.stan:9:1
  8 │  #include /functions/bernoulli_likelihoods.stan
  9 │  #include /functions/binomial_likelihoods.stan
    │  -------- file '..//functions/binomial_likelihoods.stan' included here
 10 │  #include /functions/continuous_likelihoods.stan
warning: Unreachable statement found, is this intended?
    ┌─ ..//functions/continuous_likelihoods.stan:13:5
  8 │    vector linkinv_gauss(vector eta, int link) {
  9 │      if (link == 1)      return eta;
    │                          ----------- exits the function
 10 │      else if (link == 2) return exp(eta); 
    │                          ---------------- exits the function
 11 │      else if (link == 3) return inv(eta);
    │                          ---------------- exits the function
 12 │      else reject("Invalid link");
    │           ----------------------- stops this model evaluation
 13 │      return eta; // never reached
    │      ^ Never reached
 14 │    }
    ┌─ ../jm.stan:10:1
  9 │  #include /functions/binomial_likelihoods.stan
 10 │  #include /functions/continuous_likelihoods.stan
    │  -------- file '..//functions/continuous_likelihoods.stan' included here
 11 │  #include /functions/count_likelihoods.stan
warning: Unreachable statement found, is this intended?
    ┌─ ..//functions/continuous_likelihoods.stan:40:5
 35 │    vector linkinv_gamma(vector eta, int link) {
 36 │      if (link == 1)      return eta;
    │                          ----------- exits the function
 37 │      else if (link == 2) return exp(eta);
    │                          ---------------- exits the function
 38 │      else if (link == 3) return inv(eta);
    │                          ---------------- exits the function
 39 │      else reject("Invalid link");
    │           ----------------------- stops this model evaluation
 40 │      return eta; // never reached
    │      ^ Never reached
 41 │    }
    ┌─ ../jm.stan:10:1
  9 │  #include /functions/binomial_likelihoods.stan
 10 │  #include /functions/continuous_likelihoods.stan
    │  -------- file '..//functions/continuous_likelihoods.stan' included here
 11 │  #include /functions/count_likelihoods.stan
warning: Unreachable statement found, is this intended?
    ┌─ ..//functions/continuous_likelihoods.stan:110:5
104 │    vector linkinv_inv_gaussian(vector eta, int link) {
105 │      if (link == 1)      return eta;
    │                          ----------- exits the function
106 │      else if (link == 2) return exp(eta);
    │                          ---------------- exits the function
107 │      else if (link == 3) return inv(eta);
    │                          ---------------- exits the function
108 │      else if (link == 4) return inv_sqrt(eta);
    │                          --------------------- exits the function
109 │      else reject("Invalid link");
    │           ----------------------- stops this model evaluation
110 │      return eta; // never reached
    │      ^ Never reached
111 │    }
    ┌─ ../jm.stan:10:1
  9 │  #include /functions/binomial_likelihoods.stan
 10 │  #include /functions/continuous_likelihoods.stan
    │  -------- file '..//functions/continuous_likelihoods.stan' included here
 11 │  #include /functions/count_likelihoods.stan
warning: Unreachable statement found, is this intended?
    ┌─ ..//functions/continuous_likelihoods.stan:182:5
174 │    vector linkinv_beta(vector eta, int link) {
175 │      if (link == 1) return inv_logit(eta);  // logit
    │                     ---------------------- exits the function
176 │      else if (link == 2) return Phi(eta);   // probit
    │                          ---------------- exits the function
177 │      else if (link == 3) return inv_cloglog(eta);  // cloglog
    │                          ------------------------ exits the function
178 │      else if (link == 4) return 0.5 + atan(eta) / pi(); // cauchy
    │                          ------------------------------ exits the function
179 │      else if (link == 5) return exp(eta); // log 
    │                          ---------------- exits the function
180 │      else if (link == 6) return 1 - inv_cloglog(-eta); // loglog
    │                          ----------------------------- exits the function
181 │      else reject("invalid link");
    │           ----------------------- stops this model evaluation
182 │      return eta; // never reached
    │      ^ Never reached
183 │    }
    ┌─ ../jm.stan:10:1
  9 │  #include /functions/binomial_likelihoods.stan
 10 │  #include /functions/continuous_likelihoods.stan
    │  -------- file '..//functions/continuous_likelihoods.stan' included here
 11 │  #include /functions/count_likelihoods.stan
warning: Unreachable statement found, is this intended?
    ┌─ ..//functions/continuous_likelihoods.stan:197:5
192 │    vector linkinv_beta_z(vector eta, int link) {
193 │      if (link == 1) return exp(eta);         // log
    │                     ---------------- exits the function
194 │      else if (link == 2) return eta;         // identity
    │                          ----------- exits the function
195 │      else if (link == 3) return square(eta); // sqrt
    │                          ------------------- exits the function
196 │      else reject("Invalid link");
    │           ----------------------- stops this model evaluation
197 │      return eta; // never reached
    │      ^ Never reached
198 │    }
    ┌─ ../jm.stan:10:1
  9 │  #include /functions/binomial_likelihoods.stan
 10 │  #include /functions/continuous_likelihoods.stan
    │  -------- file '..//functions/continuous_likelihoods.stan' included here
 11 │  #include /functions/count_likelihoods.stan
warning: Unreachable statement found, is this intended?
    ┌─ ..//functions/count_likelihoods.stan:14:5
  9 │    vector linkinv_count(vector eta, int link) {
 10 │      if (link == 1)      return exp(eta);     // log
    │                          ---------------- exits the function
 11 │      else if (link == 2) return eta;          // identity
    │                          ----------- exits the function
 12 │      else if (link == 3) return(square(eta)); // sqrt
    │                          -------------------- exits the function
 13 │      else reject("Invalid link");
    │           ----------------------- stops this model evaluation
 14 │      return eta; // never reached
    │      ^ Never reached
 15 │    }
    ┌─ ../jm.stan:11:1
 10 │  #include /functions/continuous_likelihoods.stan
 11 │  #include /functions/count_likelihoods.stan
    │  -------- file '..//functions/count_likelihoods.stan' included here
 12 │  #include /functions/mvmer_functions.stan
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/bernoulli_likelihoods.stan:84:8
 83 │     */
 84 │    real log_clogit_denom(int N_j, int D_j, vector eta_j);
    │         ^^^^^^^^^^^^^^^^ Forward declaration.
 85 │    real log_clogit_denom(int N_j, int D_j, vector eta_j) {
    ┌─ ../jm.stan:8:1
  7 │  #include /functions/common_functions.stan
  8 │  #include /functions/bernoulli_likelihoods.stan
    │  -------- file '..//functions/bernoulli_likelihoods.stan' included here
  9 │  #include /functions/binomial_likelihoods.stan
*/
