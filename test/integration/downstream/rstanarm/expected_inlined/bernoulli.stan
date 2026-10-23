/* $ stanc --include-paths=.. --auto-format --allow-undefined --canonicalize=includes ../bernoulli.stan : [exit 0] */
//    This file is part of rstanarm.
//    Copyright (C) 2015, 2016 2017 Trustees of Columbia University

/*
    rstanarm is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    rstanarm is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with rstanarm.  If not, see <http://www.gnu.org/licenses/>.
*/

// GLM for a Bernoulli outcome
functions {
  /* for multiple .stan files */
  
  /**
   * Create group-specific block-diagonal Cholesky factor, see section 2 of
   * https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
   * @param len_theta_L An integer indicating the length of returned vector,
   *   which lme4 denotes as m
   * @param p An integer array with the number variables on the LHS of each |
   * @param dispersion Scalar standard deviation of the errors, calles sigma by lme4
   * @param tau Vector of scale parameters whose squares are proportional to the
   *   traces of the relative covariance matrices of the group-specific terms
   * @param scale Vector of prior scales that are multiplied by elements of tau
   * @param zeta Vector of positive parameters that are normalized into simplexes
   *   and multiplied by the trace of the covariance matrix to produce variances
   * @param rho Vector of radii in the onion method for creating Cholesky factors
   * @param z_T Vector used in the onion method for creating Cholesky factors
   * @return A vector that corresponds to theta in lme4
   */
  vector make_theta_L(int len_theta_L,
                      array[] int p,
                      real dispersion,
                      vector tau,
                      vector scale,
                      vector zeta,
                      vector rho,
                      vector z_T) {
    vector[len_theta_L] theta_L;
    int zeta_mark = 1;
    int rho_mark = 1;
    int z_T_mark = 1;
    int theta_L_mark = 1;
    
    // each of these is a diagonal block of the implicit Cholesky factor
    for (i in 1 : size(p)) {
      int nc = p[i];
      if (nc == 1) {
        // "block" is just a standard deviation
        theta_L[theta_L_mark] = tau[i] * scale[i] * dispersion;
        // unlike lme4, theta[theta_L_mark] includes the dispersion term in it
        theta_L_mark += 1;
      } else {
        // block is lower-triangular
        matrix[nc, nc] T_i;
        real std_dev;
        real T21;
        real trace_T_i = square(tau[i] * scale[i] * dispersion) * nc;
        vector[nc] pi = segment(zeta, zeta_mark, nc); // gamma(zeta | shape, 1)
        pi /= sum(pi); // thus dirichlet(pi | shape)
        
        // unlike lme4, T_i includes the dispersion term in it
        zeta_mark += nc;
        std_dev = sqrt(pi[1] * trace_T_i);
        T_i[1, 1] = std_dev;
        
        // Put a correlation into T_i[2,1] and scale by std_dev
        std_dev = sqrt(pi[2] * trace_T_i);
        T21 = 2.0 * rho[rho_mark] - 1.0;
        rho_mark += 1;
        T_i[2, 2] = std_dev * sqrt(1.0 - square(T21));
        T_i[2, 1] = std_dev * T21;
        
        for (r in 2 : (nc - 1)) {
          // scaled onion method to fill T_i
          int rp1 = r + 1;
          vector[r] T_row = segment(z_T, z_T_mark, r);
          real scale_factor = sqrt(rho[rho_mark] / dot_self(T_row)) * std_dev;
          z_T_mark += r;
          std_dev = sqrt(pi[rp1] * trace_T_i);
          for (c in 1 : r) 
            T_i[rp1, c] = T_row[c] * scale_factor;
          T_i[rp1, rp1] = sqrt(1.0 - rho[rho_mark]) * std_dev;
          rho_mark += 1;
        }
        
        // now vech T_i
        for (c in 1 : nc) 
          for (r in c : nc) {
            theta_L[theta_L_mark] = T_i[r, c];
            theta_L_mark += 1;
          }
      }
    }
    return theta_L;
  }
  
  /**
  * Create group-specific coefficients, see section 2 of
  * https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
  *
  * @param z_b Vector whose elements are iid normal(0,sigma) a priori
  * @param theta Vector with covariance parameters as defined in lme4
  * @param p An integer array with the number variables on the LHS of each |
  * @param l An integer array with the number of levels for the factor(s) on
  *   the RHS of each |
  * @return A vector of group-specific coefficients
  */
  vector make_b(vector z_b, vector theta_L, array[] int p, array[] int l) {
    vector[rows(z_b)] b;
    int b_mark = 1;
    int theta_L_mark = 1;
    for (i in 1 : size(p)) {
      int nc = p[i];
      if (nc == 1) {
        real theta_L_start = theta_L[theta_L_mark];
        for (s in b_mark : (b_mark + l[i] - 1)) 
          b[s] = theta_L_start * z_b[s];
        b_mark += l[i];
        theta_L_mark += 1;
      } else {
        matrix[nc, nc] T_i = rep_matrix(0, nc, nc);
        for (c in 1 : nc) {
          T_i[c, c] = theta_L[theta_L_mark];
          theta_L_mark += 1;
          for (r in (c + 1) : nc) {
            T_i[r, c] = theta_L[theta_L_mark];
            theta_L_mark += 1;
          }
        }
        for (j in 1 : l[i]) {
          vector[nc] temp = T_i * segment(z_b, b_mark, nc);
          b_mark -= 1;
          for (s in 1 : nc) 
            b[b_mark + s] = temp[s];
          b_mark += nc + 1;
        }
      }
    }
    return b;
  }
  
  /**
   * Prior on group-specific parameters
   *
   * @param z_b A vector of primitive coefficients
   * @param z_T A vector of primitives for the unit vectors in the onion method
   * @param rho A vector radii for the onion method
   * @param zeta A vector of primitives for the simplexes
   * @param tau A vector of scale parameters
   * @param regularization A real array of LKJ hyperparameters
   * @param delta A real array of concentration paramters
   * @param shape A vector of shape parameters
   * @param t An integer indicating the number of group-specific terms
   * @param p An integer array with the number variables on the LHS of each |
   * @return target()
   */
  real decov_lp(vector z_b,
                vector z_T,
                vector rho,
                vector zeta,
                vector tau,
                array[] real regularization,
                array[] real delta,
                vector shape,
                int t,
                array[] int p) {
    int pos_reg = 1;
    int pos_rho = 1;
    target += normal_lpdf(z_b | 0, 1);
    target += normal_lpdf(z_T | 0, 1);
    for (i in 1 : t) 
      if (p[i] > 1) {
        vector[p[i] - 1] shape1;
        vector[p[i] - 1] shape2;
        real nu = regularization[pos_reg] + 0.5 * (p[i] - 2);
        pos_reg += 1;
        shape1[1] = nu;
        shape2[1] = nu;
        for (j in 2 : (p[i] - 1)) {
          nu -= 0.5;
          shape1[j] = 0.5 * j;
          shape2[j] = nu;
        }
        target += beta_lpdf(rho[pos_rho : (pos_rho + p[i] - 2)] | shape1,
                            shape2);
        pos_rho += p[i] - 1;
      }
    target += gamma_lpdf(zeta | delta, 1);
    target += gamma_lpdf(tau | shape, 1);
    return target();
  }
  
  /**
   * Hierarchical shrinkage parameterization
   *
   * @param z_beta A vector of primitive coefficients
   * @param global A real array of positive numbers
   * @param local A vector array of positive numbers
   * @param global_prior_scale A positive real number
   * @param error_scale 1 or sigma in the Gaussian case
   * @param c2 A positive real number
   * @return A vector of coefficientes
   */
  vector hs_prior(vector z_beta, array[] real global, array[] vector local,
                  real global_prior_scale, real error_scale, real c2) {
    int K = rows(z_beta);
    vector[K] lambda = local[1] .* sqrt(local[2]);
    real tau = global[1] * sqrt(global[2]) * global_prior_scale * error_scale;
    vector[K] lambda2 = square(lambda);
    vector[K] lambda_tilde = sqrt(
                                  c2 * lambda2
                                  ./ (c2 + square(tau) * lambda2));
    return z_beta .* lambda_tilde * tau;
  }
  
  /**
   * Hierarchical shrinkage plus parameterization
   *
   * @param z_beta A vector of primitive coefficients
   * @param global A real array of positive numbers
   * @param local A vector array of positive numbers
   * @param global_prior_scale A positive real number
   * @param error_scale 1 or sigma in the Gaussian case
   * @param c2 A positive real number
   * @return A vector of coefficientes
   */
  vector hsplus_prior(vector z_beta, array[] real global,
                      array[] vector local, real global_prior_scale,
                      real error_scale, real c2) {
    int K = rows(z_beta);
    vector[K] lambda = local[1] .* sqrt(local[2]);
    vector[K] eta = local[3] .* sqrt(local[4]);
    real tau = global[1] * sqrt(global[2]) * global_prior_scale * error_scale;
    vector[K] lambda_eta2 = square(lambda .* eta);
    vector[K] lambda_tilde = sqrt(
                                  c2 * lambda_eta2
                                  ./ (c2 + square(tau) * lambda_eta2));
    return z_beta .* lambda_tilde * tau;
  }
  
  /**
   * Cornish-Fisher expansion for standard normal to Student t
   *
   * See result 26.7.5 of
   * http://people.math.sfu.ca/~cbm/aands/page_949.htm
   *
   * @param z A scalar distributed standard normal
   * @param df A scalar degrees of freedom
   * @return An (approximate) Student t variate with df degrees of freedom
   */
  real CFt(real z, real df) {
    real z2 = square(z);
    real z3 = z2 * z;
    real z5 = z2 * z3;
    real z7 = z2 * z5;
    real z9 = z2 * z7;
    real df2 = square(df);
    real df3 = df2 * df;
    real df4 = df2 * df2;
    return z + (z3 + z) / (4 * df) + (5 * z5 + 16 * z3 + 3 * z) / (96 * df2)
           + (3 * z7 + 19 * z5 + 17 * z3 - 15 * z) / (384 * df3)
           + (79 * z9 + 776 * z7 + 1482 * z5 - 1920 * z3 - 945 * z)
             / (92160 * df4);
  }
  
  /**
   * Return two-dimensional array of group membership
   *
   * @param N An integer indicating the number of observations
   * @param t An integer indicating the number of grouping variables
   * @param v An integer array with the indices of group membership
   * @return An two-dimensional integer array of group membership
   */
  array[,] int make_V(int N, int t, array[] int v) {
    array[t, N] int V;
    int pos = 1;
    if (t > 0) 
      for (j in 1 : N) 
        for (i in 1 : t) {
          V[i, j] = v[pos] + 1;
          pos += 1;
        }
    return V;
  }
  
  /**
  * faster version of csr_matrix_times_vector
  * declared here and defined in C++
  *
  * @param m Integer number of rows
  * @param n Integer number of columns
  * @param w Vector (see reference manual)
  * @param v Integer array (see reference manual)
  * @param u Integer array (see reference manual)
  * @param b Vector that is multiplied from the left by the CSR matrix
  * @return A vector that is the product of the CSR matrix and b
  */
  vector csr_matrix_times_vector2(int m, int n, vector w, array[] int v,
                                  array[] int u, vector b);
  
  /**
   * Calculate lower bound on intercept
   *
   * @param family Integer family code
   *   1 = gaussian
   *   2 = gamma
   *   3 = inv-gaussian
   *   4 = beta
   *   5 = binomial
   *   6 = poisson
   *   7 = neg-binom
   *   8 = poisson w/ gamma noise (not currently used but in count.stan)
   * @param link Integer link code
   * @return real lower bound
   */
  real make_lower(int family, int link) {
    if (family == 1) 
      return negative_infinity(); // Gaussian
    if (family <= 3) {
      // Gamma or inverse Gaussian
      if (link == 2) 
        return negative_infinity(); // log
      return 0;
    }
    return negative_infinity();
  }
  
  /**
   * Calculate upper bound on intercept
   *
   * @param family Integer family code (see make_lower above for codes)
   * @param link Integer link code
   * @return real upper bound
   */
  real make_upper(int family, int link) {
    if (family == 4 && link == 5) 
      return 0;
    return positive_infinity();
  }
  
  /**
   * Apply inverse link function to linear predictor
   * see help(binom) in R
   *
   * @param eta Linear predictor vector
   * @param link An integer indicating the link function
   * @return A vector, i.e. inverse-link(eta)
   */
  vector linkinv_bern(vector eta, int link) {
    if (link == 1) 
      return (inv_logit(eta)); // logit
    else if (link == 2) 
      return (Phi(eta)); // probit
    else if (link == 3) 
      return (atan(eta) / pi() + 0.5); // cauchit
    else if (link == 4) 
      return (exp(eta)); // log
    else if (link == 5) 
      return (inv_cloglog(eta)); // cloglog
    else 
      reject("Invalid link");
    return eta; // never reached
  }
  
  /**
   * Increment with the unweighted log-likelihood
   * @param link An integer indicating the link function
   * @param eta0 A vector of linear predictors | y = 0
   * @param eta1 A vector of linear predictors | y = 1
   * @param N An integer array of length 2 giving the number of
   *   observations where y = 0 and y = 1 respectively
   * @return lp__
   */
  real ll_bern_lp(vector eta0, vector eta1, int link, array[] int N) {
    if (link == 1) {
      // logit
      target += logistic_lccdf(eta0 | 0, 1);
      target += logistic_lcdf(eta1 | 0, 1);
    } else if (link == 2) {
      // probit
      target += normal_lccdf(eta0 | 0, 1);
      target += normal_lcdf(eta1 | 0, 1);
    } else if (link == 3) {
      // cauchit
      target += cauchy_lccdf(eta0 | 0, 1);
      target += cauchy_lcdf(eta1 | 0, 1);
    } else if (link == 4) {
      // log
      target += log1m_exp(eta0);
      target += eta1; // already in log form
    } else if (link == 5) {
      // cloglog
      target += log1m_exp(-exp(eta1));
      target += -exp(eta0);
    } else 
      reject("Invalid link");
    return target();
  }
  
  /**
   * Pointwise (pw) log-likelihood vector
   *
   * @param y The integer outcome variable. Note that function is
   *  called separately with y = 0 and y = 1
   * @param eta Vector of linear predictions
   * @param link An integer indicating the link function
   * @return A vector
   */
  vector pw_bern(int y, vector eta, int link) {
    int N = rows(eta);
    vector[N] ll;
    if (link == 1) {
      // logit
      for (n in 1 : N) 
        ll[n] = bernoulli_logit_lpmf(y | eta[n]);
    } else if (link <= 5) {
      // link = probit, cauchit, log, or cloglog
      vector[N] pi = linkinv_bern(eta, link); // may not be stable
      for (n in 1 : N) 
        ll[n] = bernoulli_lpmf(y | pi[n]);
    } else 
      reject("Invalid link");
    return ll;
  }
  
  /**
   * Log-normalizing constant in the clogit case
   *
   * @param N_j Integer number of observations in the j-th group
   * @param D_j Integer number of successes in the j-th group
   * @param eta_j Vector of linear predictions in the j-th group
   * @return A scalar that normalizes the probabilities on the log-scale
   */
  real log_clogit_denom(int N_j, int D_j, vector eta_j);
  real log_clogit_denom(int N_j, int D_j, vector eta_j) {
    if (D_j == 1 && N_j == rows(eta_j)) 
      return log_sum_exp(eta_j);
    if (D_j == 0) 
      return 0;
    if (N_j == D_j) {
      if (D_j == 1) 
        return eta_j[N_j];
      return sum(segment(eta_j, N_j - 1, 2));
    } else {
      int N_jm1 = N_j - 1;
      return log_sum_exp(log_clogit_denom(N_jm1, D_j, eta_j),
                         log_clogit_denom(N_jm1, D_j - 1, eta_j) + eta_j[N_j]);
    }
    return not_a_number(); // never reaches
  }
  
  /**
   * Log-likelihood for a clogit model
   * @param eta0 Linear predictors when y == 0
   * @param eta1 Linear predictors when y == 1
   * @param successes Integer array with the number of successes in group j
   * @param failures Integer array with the number of failures in group j
   * @param observations Integer array with the number of observations in group j
   * @return lp__
   */
  real ll_clogit_lp(vector eta0, vector eta1, array[] int successes,
                    array[] int failures, array[] int observations) {
    int J = num_elements(observations);
    int pos0 = 1;
    int pos1 = 1;
    vector[J] summands;
    for (j in 1 : J) {
      int D_g = successes[j];
      int N_g = observations[j];
      int F_g = failures[j];
      vector[N_g] eta_g = append_row(segment(eta1, pos1, D_g),
                                     segment(eta0, pos0, F_g));
      summands[j] = log_clogit_denom(N_g, D_g, eta_g);
      pos0 += F_g;
      pos1 += D_g;
    }
    target += sum(eta1) - sum(summands);
    return target();
  }
}
data {
  // dimensions
  int<lower=0> K; // number of predictors
  array[2] int<lower=1> N; // number of observations where y = 0 and y = 1 respectively
  vector[K] xbar; // vector of column-means of rbind(X0, X1)
  int<lower=0, upper=1> dense_X; // flag for dense vs. sparse
  array[dense_X] matrix[N[1], K] X0; // centered (by xbar) predictor matrix | y = 0
  array[dense_X] matrix[N[2], K] X1; // centered (by xbar) predictor matrix | y = 1
  
  int<lower=0, upper=1> clogit; // 1 iff the number of successes is fixed in each stratum
  int<lower=0> J; // number of strata (possibly zero)
  array[clogit == 1 ? N[1] + N[2] : 0] int<lower=1, upper=J> strata;
  
  // stuff for the sparse case
  int<lower=0> nnz_X0; // number of non-zero elements in the implicit X0 matrix
  vector[nnz_X0] w_X0; // non-zero elements in the implicit X0 matrix
  array[nnz_X0] int<lower=0, upper=K - 1> v_X0; // column indices for w_X0
  // where the non-zeros start in each row of X0
  array[dense_X ? 0 : N[1] + 1] int<lower=0, upper=rows(w_X0) + 1> u_X0;
  int<lower=0> nnz_X1; // number of non-zero elements in the implicit X1 matrix
  vector[nnz_X1] w_X1; // non-zero elements in the implicit X1 matrix
  array[nnz_X1] int<lower=0, upper=K - 1> v_X1; // column indices for w_X1
  // where the non-zeros start in each row of X1
  array[dense_X ? 0 : N[2] + 1] int<lower=0, upper=rows(w_X1) + 1> u_X1;
  // declares prior_PD, has_intercept, link, prior_dist, prior_dist_for_intercept
  
  // flag indicating whether to draw from the prior
  int<lower=0, upper=1> prior_PD; // 1 = yes
  int<lower=0, upper=1> compute_mean_PPD; // 1 = yes
  
  // intercept
  int<lower=0, upper=1> has_intercept; // 1 = yes
  
  // link function from location to linear predictor 
  int<lower=1> link; // interpretation varies by .stan file
  
  // prior family: 0 = none, 1 = normal, 2 = student_t, 3 = hs, 4 = hs_plus, 
  //   5 = laplace, 6 = lasso, 7 = product_normal
  int<lower=0, upper=7> prior_dist;
  int<lower=0, upper=2> prior_dist_for_intercept;
  
  // prior family: 0 = none, 1 = normal, 2 = student_t, 3 = exponential
  int<lower=0, upper=3> prior_dist_for_aux;
  
  // prior family: 0 = none, 1 = normal, 2 = student_t, 3 = exponential
  int<lower=0, upper=3> prior_dist_for_smooth;
  
  int<lower=0> K_smooth;
  matrix[N[1], K_smooth] S0;
  matrix[N[2], K_smooth] S1;
  array[K_smooth] int<lower=1> smooth_map;
  
  int<lower=5, upper=5> family;
  
  // weights
  int<lower=0, upper=1> has_weights; // 0 = No, 1 = Yes
  vector[has_weights ? N[1] : 0] weights0;
  vector[has_weights ? N[2] : 0] weights1;
  
  // offset_
  int<lower=0, upper=1> has_offset; // 0 = No, 1 = Yes
  vector[has_offset ? N[1] : 0] offset0;
  vector[has_offset ? N[2] : 0] offset1;
  
  // declares prior_{mean, scale, df}, prior_{mean, scale, df}_for_intercept, prior_{mean, scale, df}_for_aux
  
  // hyperparameter values are set to 0 if there is no prior
  vector<lower=0>[K] prior_scale;
  real<lower=0> prior_scale_for_intercept;
  real<lower=0> prior_scale_for_aux;
  vector<lower=0>[K_smooth > 0 ? max(smooth_map) : 0] prior_scale_for_smooth;
  vector[K] prior_mean;
  real prior_mean_for_intercept;
  real<lower=0> prior_mean_for_aux;
  vector<lower=0>[K_smooth > 0 ? max(smooth_map) : 0] prior_mean_for_smooth;
  vector<lower=0>[K] prior_df;
  real<lower=0> prior_df_for_intercept;
  real<lower=0> prior_df_for_aux;
  vector<lower=0>[K_smooth > 0 ? max(smooth_map) : 0] prior_df_for_smooth;
  real<lower=0> global_prior_df; // for hs priors only
  real<lower=0> global_prior_scale; // for hs priors only
  real<lower=0> slab_df; // for hs prior only
  real<lower=0> slab_scale; // for hs prior only
  array[prior_dist == 7 ? K : 0] int<lower=2> num_normals;
  
  // declares t, p[t], l[t], q, len_theta_L, shape, scale, {len_}concentration, {len_}regularization
  
  // glmer stuff, see table 3 of
  // https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
  int<lower=0> t; // num. terms (maybe 0) with a | in the glmer formula
  array[t] int<lower=1> p; // num. variables on the LHS of each |
  array[t] int<lower=1> l; // num. levels for the factor(s) on the RHS of each |
  int<lower=0> q; // conceptually equals \sum_{i=1}^t p_i \times l_i
  int<lower=0> len_theta_L; // length of the theta_L vector
  
  // hyperparameters for glmer stuff; if t > 0 priors are mandatory
  vector<lower=0>[t] shape;
  vector<lower=0>[t] scale;
  int<lower=0> len_concentration;
  array[len_concentration] real<lower=0> concentration;
  int<lower=0> len_regularization;
  array[len_regularization] real<lower=0> regularization;
  
  // more glmer stuff
  array[2] int<lower=0> num_non_zero; // number of non-zero elements in the Z matrices
  vector[num_non_zero[1]] w0; // non-zero elements in the implicit Z0 matrix
  vector[num_non_zero[2]] w1; // non-zero elements in the implicit Z1 matrix
  array[num_non_zero[1]] int<lower=0, upper=q - 1> v0; // column indices for w0
  array[num_non_zero[2]] int<lower=0, upper=q - 1> v1; // column indices for w1
  // where the non-zeros start in each row of Z0
  array[t > 0 ? N[1] + 1 : 0] int<lower=0, upper=rows(w0) + 1> u0;
  // where the non-zeros start in each row of Z1
  array[t > 0 ? N[2] + 1 : 0] int<lower=0, upper=rows(w1) + 1> u1;
  int<lower=0, upper=1> special_case; // whether we only have to deal with (1|group)
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
  // defines hs, len_z_T, len_var_group, delta, pos
  
  int<lower=0> len_z_T = 0;
  int<lower=0> len_var_group = sum(p) * (t > 0);
  int<lower=0> len_rho = sum(p) - t;
  int<lower=0, upper=1> is_continuous = 0; // changed in continuous.stan
  int<lower=1> pos = 1;
  array[len_concentration] real<lower=0> delta;
  int<lower=0> hs;
  if (prior_dist <= 2) 
    hs = 0;
  else if (prior_dist == 3) 
    hs = 2;
  else if (prior_dist == 4) 
    hs = 4;
  else 
    hs = 0;
  
  for (i in 1 : t) {
    if (p[i] > 1) {
      for (j in 1 : p[i]) {
        delta[pos] = concentration[j];
        pos += 1;
      }
    }
    for (j in 3 : p[i]) 
      len_z_T += p[i] - 1;
  }
  
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
  // declares z_beta, global, local, z_b, z_T, rho, zeta, tau
  
  vector[prior_dist == 7 ? sum(num_normals) : K] z_beta;
  vector[K_smooth] z_beta_smooth;
  vector<lower=0>[K_smooth > 0 ? smooth_map[K_smooth] : 0] smooth_sd_raw;
  array[hs] real<lower=0> global;
  array[hs] vector<lower=0>[K] local;
  array[hs > 0] real<lower=0> caux;
  array[prior_dist == 5 || prior_dist == 6] vector<lower=0>[K] mix;
  array[prior_dist == 6] real<lower=0> one_over_lambda;
  vector[q] z_b;
  vector[len_z_T] z_T;
  vector<lower=0, upper=1>[len_rho] rho;
  vector<lower=0>[len_concentration] zeta;
  vector<lower=0>[t] tau;
}
transformed parameters {
  // defines beta, b, theta_L
  
  vector[K] beta;
  vector[K_smooth] beta_smooth;
  vector[K_smooth > 0 ? smooth_map[K_smooth] : 0] smooth_sd;
  vector[q] b;
  vector[len_theta_L] theta_L;
  if (prior_dist == 0) 
    beta = z_beta;
  else if (prior_dist == 1) 
    beta = z_beta .* prior_scale + prior_mean;
  else if (prior_dist == 2) 
    for (k in 1 : K) {
      beta[k] = CFt(z_beta[k], prior_df[k]) * prior_scale[k] + prior_mean[k];
    }
  else if (prior_dist == 3) {
    real c2 = square(slab_scale) * caux[1];
    if (is_continuous == 1 && family == 1) 
      beta = hs_prior(z_beta, global, local, global_prior_scale, aux, c2);
    else 
      beta = hs_prior(z_beta, global, local, global_prior_scale, 1, c2);
  } else if (prior_dist == 4) {
    real c2 = square(slab_scale) * caux[1];
    if (is_continuous == 1 && family == 1) 
      beta = hsplus_prior(z_beta, global, local, global_prior_scale, aux, c2);
    else 
      beta = hsplus_prior(z_beta, global, local, global_prior_scale, 1, c2);
  } else if (prior_dist == 5)  // laplace
    beta = prior_mean + prior_scale .* sqrt(2 * mix[1]) .* z_beta;
  else if (prior_dist == 6)  // lasso
    beta = prior_mean
           + one_over_lambda[1] * prior_scale .* sqrt(2 * mix[1]) .* z_beta;
  else if (prior_dist == 7) {
    // product_normal
    int z_pos = 1;
    for (k in 1 : K) {
      beta[k] = z_beta[z_pos];
      z_pos += 1;
      for (n in 2 : num_normals[k]) {
        beta[k] *= z_beta[z_pos];
        z_pos += 1;
      }
      beta[k] *= prior_scale[k] ^ num_normals[k];
      beta[k] += prior_mean[k];
    }
  }
  
  if (K_smooth) {
    smooth_sd = prior_mean_for_smooth
                + prior_scale_for_smooth .* smooth_sd_raw;
    if (is_continuous && family == 1) 
      smooth_sd *= aux;
    beta_smooth = z_beta_smooth .* smooth_sd[smooth_map];
  }
  
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
  // defines eta0, eta1
  
  vector[N[1]] eta0;
  vector[N[2]] eta1;
  if (K > 0) {
    if (dense_X) {
      eta0 = X0[1] * beta;
      eta1 = X1[1] * beta;
    } else {
      eta0 = csr_matrix_times_vector2(N[1], K, w_X0, v_X0, u_X0, beta);
      eta1 = csr_matrix_times_vector2(N[2], K, w_X1, v_X1, u_X1, beta);
    }
  } else {
    eta0 = rep_vector(0.0, N[1]);
    eta1 = rep_vector(0.0, N[2]);
  }
  if (has_intercept == 0 && dense_X) {
    real tmp = dot_product(xbar, beta);
    eta0 += tmp;
    eta1 += tmp;
  }
  if (has_offset == 1) {
    eta0 += offset0;
    eta1 += offset1;
  }
  if (K_smooth) {
    eta0 += S0 * beta_smooth;
    eta1 += S1 * beta_smooth;
  }
  if (special_case) 
    for (i in 1 : t) {
      eta0 += b[V0[i]];
      eta1 += b[V1[i]];
    }
  else if (t > 0) {
    eta0 += csr_matrix_times_vector2(N[1], q, w0, v0, u0, b);
    eta1 += csr_matrix_times_vector2(N[2], q, w1, v1, u1, b);
  }
  
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
  // Log-likelihood
  if (clogit && prior_PD == 0) {
    real dummy = ll_clogit_lp(eta0, eta1, successes, failures, observations);
  } else if (has_weights == 0 && prior_PD == 0) {
    real dummy = ll_bern_lp(eta0, eta1, link, N);
  } else if (prior_PD == 0) {
    // weighted log-likelihoods
    target += dot_product(weights0, pw_bern(0, eta0, link));
    target += dot_product(weights1, pw_bern(1, eta1, link));
  }
  
  // Log-priors for coefficients
  if (prior_dist == 1) 
    target += normal_lpdf(z_beta | 0, 1);
  else if (prior_dist == 2) 
    target += normal_lpdf(z_beta | 0, 1); // Student t via Cornish-Fisher expansion
  else if (prior_dist == 3) {
    // hs
    real log_half = -0.693147180559945286;
    target += normal_lpdf(z_beta | 0, 1);
    target += normal_lpdf(local[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(local[2] | 0.5 * prior_df, 0.5 * prior_df);
    target += normal_lpdf(global[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(global[2] | 0.5 * global_prior_df,
                             0.5 * global_prior_df);
    target += inv_gamma_lpdf(caux | 0.5 * slab_df, 0.5 * slab_df);
  } else if (prior_dist == 4) {
    // hs+
    real log_half = -0.693147180559945286;
    target += normal_lpdf(z_beta | 0, 1);
    target += normal_lpdf(local[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(local[2] | 0.5 * prior_df, 0.5 * prior_df);
    target += normal_lpdf(local[3] | 0, 1) - log_half;
    // unorthodox useage of prior_scale as another df hyperparameter
    target += inv_gamma_lpdf(local[4] | 0.5 * prior_scale, 0.5 * prior_scale);
    target += normal_lpdf(global[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(global[2] | 0.5 * global_prior_df,
                             0.5 * global_prior_df);
    target += inv_gamma_lpdf(caux | 0.5 * slab_df, 0.5 * slab_df);
  } else if (prior_dist == 5) {
    // laplace
    target += normal_lpdf(z_beta | 0, 1);
    target += exponential_lpdf(mix[1] | 1);
  } else if (prior_dist == 6) {
    // lasso
    target += normal_lpdf(z_beta | 0, 1);
    target += exponential_lpdf(mix[1] | 1);
    target += chi_square_lpdf(one_over_lambda[1] | prior_df[1]);
  } else if (prior_dist == 7) {
    // product_normal
    target += normal_lpdf(z_beta | 0, 1);
  }
  /* else prior_dist is 0 and nothing is added */
  
  // Log-prior for intercept  
  if (has_intercept == 1) {
    if (prior_dist_for_intercept == 1)  // normal
      target += normal_lpdf(gamma | prior_mean_for_intercept,
                            prior_scale_for_intercept);
    else if (prior_dist_for_intercept == 2)  // student_t
      target += student_t_lpdf(gamma | prior_df_for_intercept,
                               prior_mean_for_intercept,
                               prior_scale_for_intercept);
    /* else prior_dist is 0 and nothing is added */
  }
  
  if (K_smooth) {
    target += normal_lpdf(z_beta_smooth | 0, 1);
    if (prior_dist_for_smooth > 0) {
      real log_half = -0.693147180559945286;
      if (prior_dist_for_smooth == 1) 
        target += normal_lpdf(smooth_sd_raw | 0, 1) - log_half;
      else if (prior_dist_for_smooth == 2) 
        target += student_t_lpdf(smooth_sd_raw | prior_df_for_smooth, 0, 1)
                  - log_half;
      else if (prior_dist_for_smooth == 3) 
        target += exponential_lpdf(smooth_sd_raw | 1);
    }
  }
  
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
    // defines eta0, eta1
    
    vector[N[1]] eta0;
    vector[N[2]] eta1;
    if (K > 0) {
      if (dense_X) {
        eta0 = X0[1] * beta;
        eta1 = X1[1] * beta;
      } else {
        eta0 = csr_matrix_times_vector2(N[1], K, w_X0, v_X0, u_X0, beta);
        eta1 = csr_matrix_times_vector2(N[2], K, w_X1, v_X1, u_X1, beta);
      }
    } else {
      eta0 = rep_vector(0.0, N[1]);
      eta1 = rep_vector(0.0, N[2]);
    }
    if (has_intercept == 0 && dense_X) {
      real tmp = dot_product(xbar, beta);
      eta0 += tmp;
      eta1 += tmp;
    }
    if (has_offset == 1) {
      eta0 += offset0;
      eta1 += offset1;
    }
    if (K_smooth) {
      eta0 += S0 * beta_smooth;
      eta1 += S1 * beta_smooth;
    }
    if (special_case) 
      for (i in 1 : t) {
        eta0 += b[V0[i]];
        eta1 += b[V1[i]];
      }
    else if (t > 0) {
      eta0 += csr_matrix_times_vector2(N[1], q, w0, v0, u0, b);
      eta1 += csr_matrix_times_vector2(N[2], q, w1, v1, u1, b);
    }
    
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
        mean_PPD += successes[j]; // fixed by design
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
