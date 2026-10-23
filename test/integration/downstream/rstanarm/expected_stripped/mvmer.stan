/* $ stanc --include-paths=.. --auto-format --allow-undefined --canonicalize=strip-comments ../mvmer.stan : [exit 0] */
#include /pre/Columbia_copyright.stan
#include /pre/Brilleman_copyright.stan
#include /pre/license.stan

functions {
  #include /functions/common_functions.stan
  #include /functions/bernoulli_likelihoods.stan
  #include /functions/binomial_likelihoods.stan
  #include /functions/continuous_likelihoods.stan
  #include /functions/count_likelihoods.stan
  #include /functions/mvmer_functions.stan
}
data {
  #include /data/dimensions_mvmer.stan
  
  #include /data/data_mvmer.stan
  
  #include /data/hyperparameters_mvmer.stan
}
transformed data {
  #include /tdata/tdata_mvmer.stan
}
parameters {
  #include /parameters/parameters_mvmer.stan
}
transformed parameters {
  #include /tparameters/tparameters_mvmer.stan
}
model {
  #include /model/mvmer_lp.stan
  
  #include /model/priors_mvmer.stan
}
generated quantities {
  #include /gqs/gen_quantities_mvmer.stan
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
    ┌─ ../mvmer.stan:8:1
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
    ┌─ ../mvmer.stan:8:1
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
    ┌─ ../mvmer.stan:9:1
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
    ┌─ ../mvmer.stan:10:1
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
    ┌─ ../mvmer.stan:10:1
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
    ┌─ ../mvmer.stan:10:1
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
    ┌─ ../mvmer.stan:10:1
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
    ┌─ ../mvmer.stan:10:1
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
    ┌─ ../mvmer.stan:11:1
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
    ┌─ ../mvmer.stan:8:1
  7 │  #include /functions/common_functions.stan
  8 │  #include /functions/bernoulli_likelihoods.stan
    │  -------- file '..//functions/bernoulli_likelihoods.stan' included here
  9 │  #include /functions/binomial_likelihoods.stan
*/
