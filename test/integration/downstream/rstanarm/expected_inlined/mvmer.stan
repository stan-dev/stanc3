/* $ stanc --include-paths=.. --auto-format --allow-undefined --canonicalize=includes ../mvmer.stan : [exit 0] */
//    This file is part of rstanarm.
//    Copyright (C) 2015, 2016 2017 Trustees of Columbia University

//    Copyright (C) 2016, 2017 Sam Brilleman

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

// Multivariate GLM with correlated group-specific terms
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
  
  /**
   * Apply inverse link function to linear predictor
   *
   * @param eta Linear predictor vector
   * @param link An integer indicating the link function
   * @return A vector, i.e. inverse-link(eta)
   */
  vector linkinv_binom(vector eta, int link) {
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
  * @param y An integer array indicating the number of successes
  * @param trials An integer array indicating the number of trials
  * @param eta A vector of linear predictors
  * @param link An integer indicating the link function
  * @return lp__
  */
  real ll_binom_lp(array[] int y, array[] int trials, vector eta, int link) {
    if (link == 1) 
      target += binomial_logit_lpmf(y | trials, eta);
    else if (link < 4) 
      target += binomial_lpmf(y | trials, linkinv_binom(eta, link));
    else if (link == 4) {
      // log
      for (n in 1 : num_elements(y)) {
        target += y[n] * eta[n];
        target += (trials[n] - y[n]) * log1m_exp(eta[n]);
        target += lchoose(trials[n], y[n]);
      }
    } else if (link == 5) {
      // cloglog
      for (n in 1 : num_elements(y)) {
        real neg_exp_eta = -exp(eta[n]);
        target += y[n] * log1m_exp(neg_exp_eta);
        target += (trials[n] - y[n]) * neg_exp_eta;
        target += lchoose(trials[n], y[n]);
      }
    } else 
      reject("Invalid link");
    return target();
  }
  
  /**
  * Pointwise (pw) log-likelihood vector
  *
  * @param y The integer array corresponding to the outcome variable.
  * @param link An integer indicating the link function
  * @return A vector
  */
  vector pw_binom(array[] int y, array[] int trials, vector eta, int link) {
    int N = rows(eta);
    vector[N] ll;
    if (link == 1) {
      // logit
      for (n in 1 : N) 
        ll[n] = binomial_logit_lpmf(y[n] | trials[n], eta[n]);
    } else if (link <= 5) {
      // link = probit, cauchit, log, or cloglog
      vector[N] pi = linkinv_binom(eta, link); // may be unstable
      for (n in 1 : N) 
        ll[n] = binomial_lpmf(y[n] | trials[n], pi[n]);
    } else 
      reject("Invalid link");
    return ll;
  }
  
  /** 
   * Apply inverse link function to linear predictor
   *
   * @param eta Linear predictor vector
   * @param link An integer indicating the link function
   * @return A vector, i.e. inverse-link(eta)
   */
  vector linkinv_gauss(vector eta, int link) {
    if (link == 1) 
      return eta;
    else if (link == 2) 
      return exp(eta);
    else if (link == 3) 
      return inv(eta);
    else 
      reject("Invalid link");
    return eta; // never reached
  }
  
  /** 
  * Pointwise (pw) log-likelihood vector
  *
  * @param y A vector corresponding to the outcome variable.
  * @param link An integer indicating the link function
  * @return A vector
  */
  vector pw_gauss(vector y, vector eta, real sigma, int link) {
    return -0.5 * log(6.283185307179586232 * sigma)
           - 0.5 * square((y - linkinv_gauss(eta, link)) / sigma);
  }
  
  /** 
  * Apply inverse link function to linear predictor
  *
  * @param eta Linear predictor vector
  * @param link An integer indicating the link function
  * @return A vector, i.e. inverse-link(eta)
  */
  vector linkinv_gamma(vector eta, int link) {
    if (link == 1) 
      return eta;
    else if (link == 2) 
      return exp(eta);
    else if (link == 3) 
      return inv(eta);
    else 
      reject("Invalid link");
    return eta; // never reached
  }
  
  /** 
  * Pointwise (pw) log-likelihood vector
  *
  * @param y A vector corresponding to the outcome variable.
  * @param eta A vector of linear predictors
  * @param shape A real number for the shape parameter
  * @param link An integer indicating the link function
  * @param sum_log_y A scalar equal to the sum of log(y)
  * @return A scalar log-likelihood
  */
  real GammaReg(vector y, vector eta, real shape, int link, real sum_log_y) {
    real ret = rows(y) * (shape * log(shape) - lgamma(shape))
               + (shape - 1) * sum_log_y;
    if (link == 2)  // link is log
      ret -= shape * sum(eta) + shape * sum(y ./ exp(eta));
    else if (link == 1)  // link is identity
      ret -= shape * sum(log(eta)) + shape * sum(y ./ eta);
    else if (link == 3)  // link is inverse
      ret += shape * sum(log(eta)) - shape * dot_product(eta, y);
    else 
      reject("Invalid link");
    return ret;
  }
  
  /** 
  * Pointwise (pw) log-likelihood vector
  *
  * @param y A vector corresponding to the outcome variable.
  * @param shape A real number for the shape parameter
  * @param link An integer indicating the link function
  * @return A vector
  */
  vector pw_gamma(vector y, vector eta, real shape, int link) {
    int N = rows(eta);
    vector[N] ll;
    if (link == 3) {
      // link = inverse
      for (n in 1 : N) {
        ll[n] = gamma_lpdf(y[n] | shape, shape * eta[n]);
      }
    } else if (link == 2) {
      // link = log
      for (n in 1 : N) {
        ll[n] = gamma_lpdf(y[n] | shape, shape / exp(eta[n]));
      }
    } else if (link == 1) {
      // link = identity
      for (n in 1 : N) {
        ll[n] = gamma_lpdf(y[n] | shape, shape / eta[n]);
      }
    } else 
      reject("Invalid link");
    return ll;
  }
  
  /** 
  * Apply inverse link function to linear predictor
  *
  * @param eta Linear predictor vector
  * @param link An integer indicating the link function
  * @return A vector, i.e. inverse-link(eta)
  */
  vector linkinv_inv_gaussian(vector eta, int link) {
    if (link == 1) 
      return eta;
    else if (link == 2) 
      return exp(eta);
    else if (link == 3) 
      return inv(eta);
    else if (link == 4) 
      return inv_sqrt(eta);
    else 
      reject("Invalid link");
    return eta; // never reached
  }
  
  /** 
  * inverse Gaussian log-PDF
  *
  * @param y The vector of outcomes
  * @param mu The vector of conditional means
  * @param lambda A positive scalar dispersion parameter
  * @param sum_log_y A scalar equal to the sum of log(y)
  * @param sqrt_y A vector equal to sqrt(y)
  * @return A scalar
  */
  real inv_gaussian(vector y, vector mu, real lambda, real sum_log_y,
                    vector sqrt_y) {
    return 0.5 * rows(y) * log(lambda / 6.283185307179586232)
           - 1.5 * sum_log_y
           - 0.5 * lambda * dot_self((y - mu) ./ (mu .* sqrt_y));
  }
  
  /** 
  * Pointwise (pw) log-likelihood vector
  *
  * @param y A vector corresponding to the outcome variable.
  * @param eta The linear predictors
  * @param lamba A positive scalar dispersion parameter
  * @param link An integer indicating the link function
  * @param log_y A precalculated vector of the log of y
  * @param sqrt_y A precalculated vector of the square root of y
  * @return A vector of log-likelihoods
  */
  vector pw_inv_gaussian(vector y, vector eta, real lambda, int link,
                         vector log_y, vector sqrt_y) {
    vector[rows(y)] mu = linkinv_inv_gaussian(eta, link); // link checked
    return -0.5 * lambda * square((y - mu) ./ (mu .* sqrt_y))
           + 0.5 * log(lambda / 6.283185307179586232) - 1.5 * log_y;
  }
  
  /** 
  * PRNG for the inverse Gaussian distribution
  *
  * Algorithm from wikipedia 
  *
  * @param mu The expectation
  * @param lambda The dispersion
  * @return A draw from the inverse Gaussian distribution
  */
  real inv_gaussian_rng(real mu, real lambda) {
    real mu2 = square(mu);
    real z = uniform_rng(0, 1);
    real y = square(normal_rng(0, 1));
    real x = mu
             + (mu2 * y - mu * sqrt(4 * mu * lambda * y + mu2 * square(y)))
               / (2 * lambda);
    if (z <= (mu / (mu + x))) 
      return x;
    else 
      return mu2 / x;
  }
  
  /** 
  * Apply inverse link function to linear predictor for beta models
  *
  * @param eta Linear predictor vector
  * @param link An integer indicating the link function
  * @return A vector, i.e. inverse-link(eta)
  */
  vector linkinv_beta(vector eta, int link) {
    if (link == 1) 
      return inv_logit(eta); // logit
    else if (link == 2) 
      return Phi(eta); // probit
    else if (link == 3) 
      return inv_cloglog(eta); // cloglog
    else if (link == 4) 
      return 0.5 + atan(eta) / pi(); // cauchy
    else if (link == 5) 
      return exp(eta); // log 
    else if (link == 6) 
      return 1 - inv_cloglog(-eta); // loglog
    else 
      reject("invalid link");
    return eta; // never reached
  }
  
  /** 
  * Apply inverse link function to linear predictor for dispersion for beta models
  *
  * @param eta Linear predictor vector
  * @param link An integer indicating the link function
  * @return A vector, i.e. inverse-link(eta)
  */
  vector linkinv_beta_z(vector eta, int link) {
    if (link == 1) 
      return exp(eta); // log
    else if (link == 2) 
      return eta; // identity
    else if (link == 3) 
      return square(eta); // sqrt
    else 
      reject("Invalid link");
    return eta; // never reached
  }
  
  /** 
  * Pointwise (pw) log-likelihood vector for beta models
  *
  * @param y The vector of outcomes
  * @param eta The linear predictors
  * @param dispersion Positive dispersion parameter
  * @param link An integer indicating the link function
  * @return A vector of log-likelihoods
  */
  vector pw_beta(vector y, vector eta, real dispersion, int link) {
    vector[rows(y)] ll;
    vector[rows(y)] mu = linkinv_beta(eta, link); // link checked
    for (n in 1 : rows(y)) {
      ll[n] = beta_lpdf(y[n] | mu[n] * dispersion, (1 - mu[n]) * dispersion);
    }
    return ll;
  }
  
  /** 
  * Pointwise (pw) log-likelihood vector for beta models with z variables
  *
  * @param y The vector of outcomes
  * @param eta The linear predictors (for y)
  * @param eta_z The linear predictors (for dispersion)
  * @param link An integer indicating the link function passed to linkinv_beta
  * @param link_phi An integer indicating the link function passed to linkinv_beta_z
  * @return A vector of log-likelihoods
  */
  vector pw_beta_z(vector y, vector eta, vector eta_z, int link,
                   int link_phi) {
    vector[rows(y)] ll;
    vector[rows(y)] mu = linkinv_beta(eta, link); // link checked
    vector[rows(y)] mu_z = linkinv_beta_z(eta_z, link_phi); // link checked
    for (n in 1 : rows(y)) {
      ll[n] = beta_lpdf(y[n] | mu[n] * mu_z[n], (1 - mu[n]) * mu_z[n]);
    }
    return ll;
  }
  
  /**
   * Apply inverse link function to linear predictor
   * see help(poisson) in R
   *
   * @param eta Linear predictor vector
   * @param link An integer indicating the link function
   * @return A vector, i.e. inverse-link(eta)
   */
  vector linkinv_count(vector eta, int link) {
    if (link == 1) 
      return exp(eta); // log
    else if (link == 2) 
      return eta; // identity
    else if (link == 3) 
      return (square(eta)); // sqrt
    else 
      reject("Invalid link");
    return eta; // never reached
  }
  
  /**
  * Pointwise (pw) log-likelihood vector for the Poisson distribution
  *
  * @param y The integer array corresponding to the outcome variable.
  * @param eta The vector of linear predictors
  * @param link An integer indicating the link function
  * @return A vector
  */
  vector pw_pois(array[] int y, vector eta, int link) {
    int N = rows(eta);
    vector[N] ll;
    if (link == 1)  // log
      for (n in 1 : N) 
        ll[n] = poisson_log_lpmf(y[n] | eta[n]);
    else if (link <= 3) {
      // link = identity or sqrt
      vector[N] phi = linkinv_count(eta, link);
      for (n in 1 : N) 
        ll[n] = poisson_lpmf(y[n] | phi[n]);
    } else 
      reject("Invalid link");
    return ll;
  }
  
  /**
  * Pointwise (pw) log-likelihood vector for the negative binomial distribution
  *
  * @param y The integer array corresponding to the outcome variable.
  * @param eta The vector of linear predictors
  * @param theta The reciprocal_dispersion parameter
  * @param link An integer indicating the link function
  * @return A vector
  */
  vector pw_nb(array[] int y, vector eta, real theta, int link) {
    int N = rows(eta);
    vector[N] rho = linkinv_count(eta, link); // link checked
    vector[N] ll;
    for (n in 1 : N) 
      ll[n] = neg_binomial_2_lpmf(y[n] | rho[n], theta);
    return ll;
  }
  
  /**
  * Return the required number of local hs parameters
  *
  * @param prior_dist An integer indicating the prior distribution
  * @return An integer
  */
  int get_nvars_for_hs(int prior_dist) {
    int hs = 0;
    if (prior_dist == 3) 
      hs = 2;
    else if (prior_dist == 4) 
      hs = 4;
    return hs;
  }
  
  /**
  * Return the lower/upper bound for the specified intercept type
  *
  * @param intercept_type An integer specifying the type of intercept;
  *   0=no intercept, 1=unbounded, 2=lower bounded, 3=upper bounded
  * @return A real, corresponding to the lower bound
  */
  real lb(int intercept_type) {
    real lb_;
    if (intercept_type == 2) 
      lb_ = 0;
    else 
      lb_ = negative_infinity();
    return lb_;
  }
  real ub(int intercept_type) {
    real ub_;
    if (intercept_type == 3) 
      ub_ = 0;
    else 
      ub_ = positive_infinity();
    return ub_;
  }
  
  /**
  * Get the indices corresponding to the lower tri of a square matrix
  *
  * @param dim The number of rows in the square matrix
  * @return A vector of indices
  */
  array[] int lower_tri_indices(int dim) {
    array[dim + choose(dim, 2)] int indices;
    int mark = 1;
    for (r in 1 : dim) {
      for (c in r : dim) {
        indices[mark] = (r - 1) * dim + c;
        mark += 1;
      }
    }
    return indices;
  }
  
  /**
  * Scale the auxiliary parameter based on prior information
  *
  * @param aux_unscaled A real, the unscaled auxiliary parameter
  * @param prior_dist Integer, the type of prior distribution
  * @param prior_mean,prior_scale Real scalars, the mean_ and scale
  *   of the prior distribution
  * @return A real, corresponding to the scaled auxiliary parameter
  */
  real make_aux(real aux_unscaled, int prior_dist, real prior_mean,
                real prior_scale) {
    real aux;
    if (prior_dist == 0)  // none
      aux = aux_unscaled;
    else {
      aux = prior_scale * aux_unscaled;
      if (prior_dist <= 2)  // normal or student_t
        aux += prior_mean;
    }
    return aux;
  }
  
  /**
  * Scale the primitive population level parameters based on prior information
  *
  * @param z_beta A vector of primitive parameters
  * @param prior_dist Integer, the type of prior distribution
  * @param prior_mean,prior_scale Vectors of mean_ and scale parameters
  *   for the prior distributions
  * @return A vector containing the population level parameters (coefficients)
  */
  vector make_beta(vector z_beta,
                   int prior_dist,
                   vector prior_mean,
                   vector prior_scale,
                   vector prior_df,
                   real global_prior_scale,
                   array[] real global,
                   array[] vector local,
                   array[] real ool,
                   array[] vector mix,
                   array[] real aux,
                   int family,
                   real slab_scale,
                   array[] real caux) {
    vector[rows(z_beta)] beta;
    if (prior_dist == 0) 
      beta = z_beta;
    else if (prior_dist == 1) 
      beta = z_beta .* prior_scale + prior_mean;
    else if (prior_dist == 2) 
      for (k in 1 : rows(prior_mean)) {
        beta[k] = CFt(z_beta[k], prior_df[k]) * prior_scale[k]
                  + prior_mean[k];
      }
    else if (prior_dist == 3) {
      real c2 = square(slab_scale) * caux[1];
      if (family == 1)  // don't need is_continuous since family == 1 is gaussian in mvmer
        beta = hs_prior(z_beta, global, local, global_prior_scale, aux[1],
                        c2);
      else 
        beta = hs_prior(z_beta, global, local, global_prior_scale, 1, c2);
    } else if (prior_dist == 4) {
      real c2 = square(slab_scale) * caux[1];
      if (family == 1)  // don't need is_continuous since family == 1 is gaussian in mvmer
        beta = hsplus_prior(z_beta, global, local, global_prior_scale,
                            aux[1], c2);
      else 
        beta = hsplus_prior(z_beta, global, local, global_prior_scale, 1, c2);
    } else if (prior_dist == 5)  // laplace
      beta = prior_mean + prior_scale .* sqrt(2 * mix[1]) .* z_beta;
    else if (prior_dist == 6)  // lasso
      beta = prior_mean + ool[1] * prior_scale .* sqrt(2 * mix[1]) .* z_beta;
    return beta;
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
  * @param i The index of the grouping factor for which you want to return
  *   the group-specific coefficients for
  * @return An array of group-specific coefficients for grouping factor i
  */
  matrix make_b_matrix(vector z_b, vector theta_L, array[] int p,
                       array[] int l, int i) {
    matrix[p[i], l[i]] b_matrix;
    int nc = p[i];
    int b_mark = 1;
    int theta_L_mark = 1;
    if (i > 1) {
      for (j in 1 : (i - 1)) {
        theta_L_mark += p[j] + choose(p[j], 2);
        b_mark += p[j] * l[j];
      }
    }
    if (nc == 1) {
      real theta_L_start = theta_L[theta_L_mark];
      for (s in b_mark : (b_mark + l[i] - 1)) 
        b_matrix[nc, s] = theta_L_start * z_b[s];
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
        b_matrix[ : , j] = temp;
        b_mark += nc;
      }
    }
    return b_matrix';
  }
  
  /**
  * Evaluate the linear predictor for the glmer submodel
  *
  * @param X Design matrix for fe
  * @param Z1 Design matrix for re, for first grouping factor
  * @param Z2 Design matrix for re, for second grouping factor
  * @param Z1_id Group indexing for Z1
  * @param Z2_id Group indexing for Z2
  * @param gamma The intercept parameter
  * @param beta Vector of population level parameters
  * @param b1Mat Matrix of group level params for first grouping factor
  * @param b2Mat Matrix of group level params for second grouping factor
  * @param b1Mat_colshift,b2Mat_colshift Number of columns in b1Mat/b2Mat
  *   that correpond to group level params from prior glmer submodels
  * @param intercept_type The type of intercept parameter (0 = none,
  *   1 = unbounded, 2 = lower bound, 3 = upper bound)
  * @return A vector containing the linear predictor for the glmer submodel
  */
  vector evaluate_eta(matrix X,
                      array[] vector Z1,
                      array[] vector Z2,
                      array[] int Z1_id,
                      array[] int Z2_id,
                      array[] real gamma,
                      vector beta,
                      matrix b1Mat,
                      matrix b2Mat,
                      int b1Mat_colshift,
                      int b2Mat_colshift,
                      int intercept_type) {
    int N = rows(X); // num rows in design matrix
    int K = rows(beta); // num predictors
    int p1 = size(Z1); // num group level params for group factor 1
    int p2 = size(Z2); // num group level params for group factor 2
    vector[N] eta;
    
    if (K > 0) 
      eta = X * beta;
    else 
      eta = rep_vector(0.0, N);
    
    if (intercept_type > 0) {
      // submodel has an intercept
      if (intercept_type == 1) 
        eta += gamma[1];
      else if (intercept_type == 2) 
        eta += gamma[1] - max(eta);
      else if (intercept_type == 3) 
        eta += gamma[1] - min(eta);
    }
    
    if (p1 > 0) {
      // submodel includes group factor 1
      for (k in 1 : p1) 
        for (n in 1 : N) 
          eta[n] += (b1Mat[Z1_id[n], k + b1Mat_colshift]) * Z1[k, n];
    }
    if (p2 > 0) {
      // submodel includes group factor 2
      for (k in 1 : p2) 
        for (n in 1 : N) 
          eta[n] += (b2Mat[Z2_id[n], k + b2Mat_colshift]) * Z2[k, n];
    }
    
    return eta;
  }
  
  /**
  * Evaluate mu based on eta, family and link
  *
  * @param eta Vector of linear predictors
  * @param family An integer indicating the family
  * @param link An integer indicating the link function (differs by family)
  * @return A vector
  */
  vector evaluate_mu(vector eta, int family, int link) {
    vector[rows(eta)] mu;
    if (family == 1) 
      mu = linkinv_gauss(eta, link);
    else if (family == 2) 
      mu = linkinv_gamma(eta, link);
    else if (family == 3) 
      mu = linkinv_inv_gaussian(eta, link);
    else if (family == 4) 
      mu = linkinv_bern(eta, link);
    else if (family == 5) 
      mu = linkinv_binom(eta, link);
    else if (family == 6 || family == 7 || family == 8) 
      mu = linkinv_count(eta, link);
    return mu;
  }
  
  /**
  * Increment the target with the log-likelihood for the glmer submodel
  *
  * @param z_beta A vector of primitive parameters
  * @param prior_dist Integer, the type of prior distribution
  * @param prior_mean,prior_scale Vectors of mean_ and scale parameters
  *   for the prior distributions
  * @return A vector containing the population level parameters (coefficients)
  */
  void glm_lp(vector y_real,
              array[] int y_integer,
              vector eta,
              array[] real aux,
              int family,
              int link,
              real sum_log_y,
              vector sqrt_y,
              vector log_y) {
    if (family == 1) {
      // gaussian
      if (link == 1) 
        target += normal_lpdf(y_real | eta, aux[1]);
      else if (link == 2) 
        target += lognormal_lpdf(y_real | eta, aux[1]);
      else 
        target += normal_lpdf(y_real | inv(eta), aux[1]);
    } else if (family == 2) {
      // gamma
      target += GammaReg(y_real, eta, aux[1], link, sum_log_y);
    } else if (family == 3) {
      // inverse gaussian
      target += inv_gaussian(y_real, linkinv_inv_gaussian(eta, link), aux[1],
                             sum_log_y, sqrt_y);
    } else if (family == 4) {
      // bernoulli
      if (link == 1) 
        target += bernoulli_logit_lpmf(y_integer | eta);
      else 
        target += bernoulli_lpmf(y_integer | linkinv_bern(eta, link));
    } else if (family == 5) {
      // binomial
      reject("Binomial with >1 trials not allowed.");
    } else if (family == 6 || family == 8) {
      // poisson or poisson-gamma
      if (link == 1) 
        target += poisson_log_lpmf(y_integer | eta);
      else 
        target += poisson_lpmf(y_integer | linkinv_count(eta, link));
    } else if (family == 7) {
      // negative binomial
      if (link == 1) 
        target += neg_binomial_2_log_lpmf(y_integer | eta, aux[1]);
      else 
        target += neg_binomial_2_lpmf(y_integer | linkinv_count(eta, link),
                    aux[1]);
    } else 
      reject("Invalid family.");
  }
  
  /**
  * Log-prior for coefficients
  *
  * @param z_beta Vector of primative coefficients
  * @param prior_dist Integer, the type of prior distribution
  * @param prior_scale Real, scale for the prior distribution
  * @param prior_df Real, df for the prior distribution
  * @param global_prior_df Real, df for the prior for the global hs parameter
  * @param local Vector of hs local parameters
  * @param global Real, the global parameter
  * @param mix Vector of shrinkage parameters
  * @param one_over_lambda Real
  * @return nothing
  */
  void beta_lp(vector z_beta,
               int prior_dist,
               vector prior_scale,
               vector prior_df,
               real global_prior_df,
               array[] vector local,
               array[] real global,
               array[] vector mix,
               array[] real one_over_lambda,
               real slab_df,
               array[] real caux) {
    if (prior_dist == 1) 
      target += normal_lpdf(z_beta | 0, 1);
    else if (prior_dist == 2) 
      target += normal_lpdf(z_beta | 0, 1); // Student t
    else if (prior_dist == 3) {
      // hs
      target += normal_lpdf(z_beta | 0, 1);
      target += normal_lpdf(local[1] | 0, 1);
      target += inv_gamma_lpdf(local[2] | 0.5 * prior_df, 0.5 * prior_df);
      target += normal_lpdf(global[1] | 0, 1);
      target += inv_gamma_lpdf(global[2] | 0.5 * global_prior_df,
                               0.5 * global_prior_df);
      target += inv_gamma_lpdf(caux | 0.5 * slab_df, 0.5 * slab_df);
    } else if (prior_dist == 4) {
      // hs+
      target += normal_lpdf(z_beta | 0, 1);
      target += normal_lpdf(local[1] | 0, 1);
      target += inv_gamma_lpdf(local[2] | 0.5 * prior_df, 0.5 * prior_df);
      target += normal_lpdf(local[3] | 0, 1);
      // unorthodox useage of prior_scale as another df hyperparameter
      target += inv_gamma_lpdf(local[4] | 0.5 * prior_scale,
                               0.5 * prior_scale);
      target += normal_lpdf(global[1] | 0, 1);
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
  }
  
  /**
  * Log-prior for intercept parameters
  *
  * @param gamma Real, the intercept parameter
  * @param dist Integer, the type of prior distribution
  * @param mean_ Real, mean_ of prior distribution
  * @param scale Real, scale for the prior distribution
  * @param df Real, df for the prior distribution
  * @return nothing
  */
  void gamma_lp(real gamma, int dist, real mean_, real scale, real df) {
    if (dist == 1)  // normal
      target += normal_lpdf(gamma | mean_, scale);
    else if (dist == 2)  // student_t
      target += student_t_lpdf(gamma | df, mean_, scale);
    /* else dist is 0 and nothing is added */
  }
  
  /**
  * Log-prior for auxiliary parameters
  *
  * @param aux_unscaled Vector (potentially of length 1) of unscaled
  *   auxiliary parameter(s)
  * @param dist Integer specifying the type of prior distribution
  * @param scale Real specifying the scale for the prior distribution
  * @param df Real specifying the df for the prior distribution
  * @return nothing
  */
  void aux_lp(real aux_unscaled, int dist, real scale, real df) {
    if (dist > 0 && scale > 0) {
      if (dist == 1) 
        target += normal_lpdf(aux_unscaled | 0, 1);
      else if (dist == 2) 
        target += student_t_lpdf(aux_unscaled | df, 0, 1);
      else 
        target += exponential_lpdf(aux_unscaled | 1);
    }
  }
  
  /**
  * Evaluate the mean_ of the posterior predictive distribution
  *
  * @param mu Vector containing the mean_ of the posterior predictive
  *   distribution for each observation (ie. the linear predictor after
  *   applying the inverse link function).
  * @param real The auxiliary parameter for the glmer submodel. This will be
  *   an empty array if the submodel does not have an auxiliary parameter
  * @param family An integer specifying the family
  * @return A real, the mean_ of the posterior predictive distribution
  */
  real mean_PPD_rng(vector mu, array[] real aux, int family) {
    int N = rows(mu);
    real mean_PPD = 0;
    if (family == 1) {
      // gaussian
      for (n in 1 : N) 
        mean_PPD += normal_rng(mu[n], aux[1]);
    } else if (family == 2) {
      // gamma
      for (n in 1 : N) 
        mean_PPD += gamma_rng(aux[1], aux[1] / mu[n]);
    } else if (family == 3) {
      // inverse gaussian
      for (n in 1 : N) 
        mean_PPD += inv_gaussian_rng(mu[n], aux[1]);
    } else if (family == 4) {
      // bernoulli
      for (n in 1 : N) 
        mean_PPD += bernoulli_rng(mu[n]);
    } else if (family == 5) {
      // binomial
      reject("Binomial with >1 trials not allowed.");
    } else if (family == 6 || family == 8) {
      real poisson_max = pow(2.0, 30.0);
      for (n in 1 : N) {
        // poisson or poisson-gamma
        if (mu[n] < poisson_max) 
          mean_PPD += poisson_rng(mu[n]);
        else 
          mean_PPD += normal_rng(mu[n], sqrt(mu[n]));
      }
    } else if (family == 7) {
      real poisson_max = pow(2.0, 30.0);
      for (n in 1 : N) {
        // negative binomial
        real gamma_temp;
        if (is_inf(aux[1])) 
          gamma_temp = mu[n];
        else 
          gamma_temp = gamma_rng(aux[1], aux[1] / mu[n]);
        if (gamma_temp < poisson_max) 
          mean_PPD += poisson_rng(gamma_temp);
        else 
          mean_PPD += normal_rng(gamma_temp, sqrt(gamma_temp));
      }
    }
    return mean_PPD / N;
  }
}
data {
  // declares: M, has_aux, has_weights, resp_type, intercept_type,
  //   yNobs, yNeta, yK, t, p, l, q, len_theta_L, bN1, bK1, bK1_len
  //   bK1_idx, bN2, bK2, bK2_len, bK2_idx
  
  // population level dimensions
  int<lower=1, upper=3> M; // num submodels with data (limit of 3)
  array[3] int<lower=0, upper=1> has_aux; // has auxiliary param
  int<lower=0, upper=1> has_weights; // has observation weights
  array[3] int<lower=0, upper=2> resp_type; // 1=real,2=integer,0=none
  array[3] int<lower=0, upper=3> intercept_type; // 1=unbounded,2=lob,3=upb,0=none
  array[3] int<lower=0> yNobs; // num observations
  array[3] int<lower=0> yNeta; // required length of eta
  array[3] int<lower=0> yK; // num predictors
  
  // group level dimensions, for decov prior
  int<lower=0> t; // num. terms (maybe 0) with a | in the glmer formula
  array[t] int<lower=1> p; // num. variables on the LHS of each |
  array[t] int<lower=1> l; // num. levels for the factor(s) on the RHS of each |
  int<lower=0> q; // conceptually equals \sum_{i=1}^t p_i \times l_i
  int<lower=0> len_theta_L; // length of the theta_L vector
  
  // group level dimensions, for lkj prior
  
  // group factor 1
  int<lower=0> bN1; // num groups
  int<lower=0> bK1; // total num params
  array[3] int<lower=0> bK1_len; // num params in each submodel
  array[3, 2] int<lower=0> bK1_idx; // beg/end index for group params
  
  // group factor 2
  int<lower=0> bN2; // num groups
  int<lower=0> bK2; // total num params
  array[3] int<lower=0> bK2_len; // num params in each submodel
  array[3, 2] int<lower=0> bK2_idx; // beg/end index for group params
  
  // declares: yInt{1,2,3}, yReal{1,2,3}, yX{1,2,3}, yXbar{1,2,3},
  //   family, link, y{1,2,3}_Z{1,2}, y{1,2,3}_Z{1,2}_id,
  //   y_prior_dist{_for_intercept,_for_aux,_for_cov}, prior_PD
  
  // population level data
  array[resp_type[1] == 2 ? yNobs[1] : 0] int<lower=0> yInt1; // integer responses
  array[resp_type[2] == 2 ? yNobs[2] : 0] int<lower=0> yInt2;
  array[resp_type[3] == 2 ? yNobs[3] : 0] int<lower=0> yInt3;
  vector[resp_type[1] == 1 ? yNobs[1] : 0] yReal1; // real responses
  vector[resp_type[2] == 1 ? yNobs[2] : 0] yReal2;
  vector[resp_type[3] == 1 ? yNobs[3] : 0] yReal3;
  matrix[yNeta[1], yK[1]] yX1; // fe design matrix
  matrix[yNeta[2], yK[2]] yX2;
  matrix[yNeta[3], yK[3]] yX3;
  vector[yK[1]] yXbar1; // predictor means
  vector[yK[2]] yXbar2;
  vector[yK[3]] yXbar3;
  
  // family and link (determined by 'append_mvmer_famlink' R function)
  // 1 = gaussian
  // 2 = gamma
  // 3 = inverse gaussian
  // 4 = bernoulli
  // 5 = binomial (n>1)
  // 6 = poisson
  // 7 = negative binomial
  array[M] int<lower=0> family;
  array[M] int<lower=0> link; // varies by family
  
  // group level data, group factor 1
  array[bK1_len[1]] vector[bK1_len[1] > 0 ? yNeta[1] : 0] y1_Z1; // re design matrix
  array[bK1_len[2]] vector[bK1_len[2] > 0 ? yNeta[2] : 0] y2_Z1;
  array[bK1_len[3]] vector[bK1_len[3] > 0 ? yNeta[3] : 0] y3_Z1;
  array[bK1_len[1] > 0 ? yNeta[1] : 0] int<lower=0> y1_Z1_id; // group indexing for y1_Z1
  array[bK1_len[2] > 0 ? yNeta[2] : 0] int<lower=0> y2_Z1_id; // group indexing for y2_Z1
  array[bK1_len[3] > 0 ? yNeta[3] : 0] int<lower=0> y3_Z1_id; // group indexing for y3_Z1
  
  // group level data, group factor 2
  array[bK2_len[1]] vector[bK2_len[1] > 0 ? yNeta[1] : 0] y1_Z2; // re design matrix
  array[bK2_len[2]] vector[bK2_len[2] > 0 ? yNeta[2] : 0] y2_Z2;
  array[bK2_len[3]] vector[bK2_len[3] > 0 ? yNeta[3] : 0] y3_Z2;
  array[bK2_len[1] > 0 ? yNeta[1] : 0] int<lower=0> y1_Z2_id; // group indexing for y1_Z2
  array[bK2_len[2] > 0 ? yNeta[2] : 0] int<lower=0> y2_Z2_id; // group indexing for y2_Z2
  array[bK2_len[3] > 0 ? yNeta[3] : 0] int<lower=0> y3_Z2_id; // group indexing for y3_Z2
  
  // prior family: 0 = none, 1 = normal, 2 = student_t, 3 = hs, 4 = hs_plus,
  //   5 = laplace, 6 = lasso, 7 = product_normal
  array[3] int<lower=0, upper=7> y_prior_dist;
  array[M] int<lower=0, upper=2> y_prior_dist_for_intercept;
  
  // prior family: 0 = none, 1 = normal, 2 = student_t, 3 = exponential
  array[M] int<lower=0, upper=3> y_prior_dist_for_aux;
  
  // prior family: 1 = decov, 2 = lkj
  int<lower=1, upper=2> prior_dist_for_cov;
  
  // flag indicating whether to draw from the prior
  int<lower=0, upper=1> prior_PD; // 1 = yes
  
  // declares: y_prior_{mean,scale,df}{1,2,3,_for_intercept,_for_aux}, 
  //   y_global_prior_{df,scale}, len_{concentration,regularization},
  //   b_prior_{shape,scale,concentration,regularization},
  //   b{1,2}_prior_{scale,df,regularization}
  
  // hyperparameter values are set to 0 if there is no prior
  
  // coefficients
  vector[yK[1]] y_prior_mean1;
  vector[yK[2]] y_prior_mean2;
  vector[yK[3]] y_prior_mean3;
  vector<lower=0>[yK[1]] y_prior_scale1;
  vector<lower=0>[yK[2]] y_prior_scale2;
  vector<lower=0>[yK[3]] y_prior_scale3;
  vector<lower=0>[yK[1]] y_prior_df1;
  vector<lower=0>[yK[2]] y_prior_df2;
  vector<lower=0>[yK[3]] y_prior_df3;
  vector<lower=0>[M] y_global_prior_df; // for hs priors only
  vector<lower=0>[M] y_global_prior_scale; // for hs priors only
  vector<lower=0>[M] y_slab_df; // for hs priors only
  vector<lower=0>[M] y_slab_scale; // for hs priors only
  
  // intercepts
  vector[M] y_prior_mean_for_intercept;
  vector<lower=0>[M] y_prior_scale_for_intercept;
  vector<lower=0>[M] y_prior_df_for_intercept;
  
  // auxiliary params
  vector<lower=0>[M] y_prior_mean_for_aux;
  vector<lower=0>[M] y_prior_scale_for_aux;
  vector<lower=0>[M] y_prior_df_for_aux;
  
  // decov prior stuff
  int<lower=0> len_concentration;
  int<lower=0> len_regularization;
  vector<lower=0>[t] b_prior_shape;
  vector<lower=0>[t] b_prior_scale;
  array[len_concentration] real<lower=0> b_prior_concentration;
  array[len_regularization] real<lower=0> b_prior_regularization;
  
  // lkj prior stuff
  vector<lower=0>[bK1] b1_prior_scale;
  vector<lower=0>[bK2] b2_prior_scale;
  vector<lower=0>[bK1] b1_prior_df;
  vector<lower=0>[bK2] b2_prior_df;
  real<lower=0> b1_prior_regularization;
  real<lower=0> b2_prior_regularization;
}
transformed data {
  // declares: yHs{1,2,3}, len_{z_T,var_group,rho}, pos, delta,
  //   bCov{1,2}_idx, {sqrt,log,sum_log}_y{1,2,3},
  
  // dimensions for hs priors
  int<lower=0> yHs1 = get_nvars_for_hs(M > 0 ? y_prior_dist[1] : 0);
  int<lower=0> yHs2 = get_nvars_for_hs(M > 1 ? y_prior_dist[2] : 0);
  int<lower=0> yHs3 = get_nvars_for_hs(M > 2 ? y_prior_dist[3] : 0);
  
  // data for decov prior
  int<lower=0> len_z_T = 0;
  int<lower=0> len_var_group = sum(p) * (t > 0);
  int<lower=0> len_rho = sum(p) - t;
  int<lower=1> pos = 1;
  array[len_concentration] real<lower=0> delta;
  
  // data for lkj prior
  array[prior_dist_for_cov == 2 ? (bK1 + choose(bK1, 2)) : 0] int bCov1_idx;
  array[prior_dist_for_cov == 2 ? (bK2 + choose(bK2, 2)) : 0] int bCov2_idx;
  
  // transformations of data
  real sum_log_y1 = M > 0 && (family[1] == 2 || family[1] == 3)
                    ? sum(log(yReal1)) : not_a_number();
  real sum_log_y2 = M > 1 && (family[2] == 2 || family[2] == 3)
                    ? sum(log(yReal2)) : not_a_number();
  real sum_log_y3 = M > 2 && (family[3] == 2 || family[3] == 3)
                    ? sum(log(yReal3)) : not_a_number();
  vector[M > 0 && family[1] == 3 ? yNobs[1] : 0] sqrt_y1;
  vector[M > 1 && family[2] == 3 ? yNobs[2] : 0] sqrt_y2;
  vector[M > 2 && family[3] == 3 ? yNobs[3] : 0] sqrt_y3;
  vector[M > 0 && family[1] == 3 ? yNobs[1] : 0] log_y1;
  vector[M > 1 && family[2] == 3 ? yNobs[2] : 0] log_y2;
  vector[M > 2 && family[3] == 3 ? yNobs[3] : 0] log_y3;
  if (M > 0 && family[1] == 3) {
    sqrt_y1 = sqrt(yReal1);
    log_y1 = log(yReal1);
  }
  if (M > 1 && family[2] == 3) {
    sqrt_y2 = sqrt(yReal2);
    log_y2 = log(yReal2);
  }
  if (M > 2 && family[3] == 3) {
    sqrt_y3 = sqrt(yReal3);
    log_y3 = log(yReal3);
  }
  
  // data for decov prior
  if (prior_dist_for_cov == 1) {
    for (i in 1 : t) {
      if (p[i] > 1) {
        for (j in 1 : p[i]) {
          delta[pos] = b_prior_concentration[j];
          pos += 1;
        }
      }
      for (j in 3 : p[i]) 
        len_z_T += p[i] - 1;
    }
  }
  
  // data for lkj prior
  if (prior_dist_for_cov == 2) {
    if (bK1 > 0) 
      bCov1_idx = lower_tri_indices(bK1);
    if (bK2 > 0) 
      bCov2_idx = lower_tri_indices(bK2);
  }
}
parameters {
  // declares: yGamma{1,2,3}, z_yBeta{1,2,3}, z_b, z_T, rho,
  //   zeta, tau, bSd{1,2}, z_bMat{1,2}, bCholesky{1,2},
  //   yAux{1,2,3}_unscaled, yGlobal{1,2,3}, yLocal{1,2,3}, 
  //   yOol{1,2,3}, yMix{1,2,3}
  
  // intercepts
  array[intercept_type[1] > 0] real<lower=lb(intercept_type[1]),
                                    upper=ub(intercept_type[1])> yGamma1;
  array[intercept_type[2] > 0] real<lower=lb(intercept_type[2]),
                                    upper=ub(intercept_type[2])> yGamma2;
  array[intercept_type[3] > 0] real<lower=lb(intercept_type[3]),
                                    upper=ub(intercept_type[3])> yGamma3;
  
  // population level primitive params
  vector[yK[1]] z_yBeta1;
  vector[yK[2]] z_yBeta2;
  vector[yK[3]] z_yBeta3;
  
  // group level params, decov prior
  vector[prior_dist_for_cov == 1 ? q : 0] z_b;
  vector[prior_dist_for_cov == 1 ? len_z_T : 0] z_T;
  vector<lower=0, upper=1>[prior_dist_for_cov == 1 ? len_rho : 0] rho;
  vector<lower=0>[prior_dist_for_cov == 1 ? len_concentration : 0] zeta;
  vector<lower=0>[prior_dist_for_cov == 1 ? t : 0] tau;
  
  // group level params for first grouping factor
  // group-level sds
  vector<lower=0>[prior_dist_for_cov == 2 ? bK1 : 0] bSd1;
  // unscaled group-level params
  matrix[prior_dist_for_cov == 2 && bK1 > 0 ? bK1 : 0, bK1 > 0 ? bN1 : 0] z_bMat1;
  // cholesky factor of corr matrix (if > 1 random effect)
  cholesky_factor_corr[prior_dist_for_cov == 2 && bK1 > 1 ? bK1 : 0] bCholesky1;
  
  // group level params for second grouping factor
  // group-level sds
  vector<lower=0>[prior_dist_for_cov == 2 ? bK2 : 0] bSd2;
  // unscaled group-level params
  matrix[prior_dist_for_cov == 2 && bK2 > 0 ? bK2 : 0, bK2 > 0 ? bN2 : 0] z_bMat2;
  // cholesky factor of corr matrix (if > 1 random effect)
  cholesky_factor_corr[prior_dist_for_cov == 2 && bK2 > 1 ? bK2 : 0] bCholesky2;
  
  // auxiliary params, interpretation depends on family
  array[has_aux[1]] real<lower=0> yAux1_unscaled;
  array[has_aux[2]] real<lower=0> yAux2_unscaled;
  array[has_aux[3]] real<lower=0> yAux3_unscaled;
  
  // params for priors
  array[yHs1] real<lower=0> yGlobal1;
  array[yHs2] real<lower=0> yGlobal2;
  array[yHs3] real<lower=0> yGlobal3;
  array[yHs1] vector<lower=0>[yK[1]] yLocal1;
  array[yHs2] vector<lower=0>[yK[2]] yLocal2;
  array[yHs3] vector<lower=0>[yK[3]] yLocal3;
  array[yHs1 > 0] real<lower=0> y_caux1;
  array[yHs2 > 0] real<lower=0> y_caux2;
  array[yHs3 > 0] real<lower=0> y_caux3;
  array[y_prior_dist[1] == 6] real<lower=0> yOol1; // one_over_lambda
  array[y_prior_dist[2] == 6] real<lower=0> yOol2;
  array[y_prior_dist[3] == 6] real<lower=0> yOol3;
  array[y_prior_dist[1] == 5 || y_prior_dist[1] == 6] vector<lower=0>[yK[1]] yMix1;
  array[y_prior_dist[2] == 5 || y_prior_dist[2] == 6] vector<lower=0>[yK[2]] yMix2;
  array[y_prior_dist[3] == 5 || y_prior_dist[3] == 6] vector<lower=0>[yK[3]] yMix3;
}
transformed parameters {
  // declares and defines: yBeta{1,2,3}, yAux{1,2,3}, yAuxMaximum, 
  //   theta_L, bMat{1,2}
  
  vector[yK[1]] yBeta1; // population level params
  vector[yK[2]] yBeta2;
  vector[yK[3]] yBeta3;
  array[has_aux[1]] real yAux1; // auxiliary params
  array[has_aux[2]] real yAux2;
  array[has_aux[3]] real yAux3;
  vector[len_theta_L] theta_L; // cov matrix for decov prior
  real yAuxMaximum = 1.0; // used for scaling in theta_L
  
  // group level params
  matrix[bK1 > 0 ? bN1 : 0, bK1] bMat1; // for grouping factor 1
  matrix[bK2 > 0 ? bN2 : 0, bK2] bMat2; // for grouping factor 2
  
  // population level params, auxiliary params
  if (has_aux[1] == 1) {
    yAux1[1] = make_aux(yAux1_unscaled[1], y_prior_dist_for_aux[1],
                        y_prior_mean_for_aux[1], y_prior_scale_for_aux[1]);
    if (yAux1[1] > yAuxMaximum) 
      yAuxMaximum = yAux1[1];
  }
  
  if (yK[1] > 0) 
    yBeta1 = make_beta(z_yBeta1, y_prior_dist[1], y_prior_mean1,
                       y_prior_scale1, y_prior_df1, y_global_prior_scale[1],
                       yGlobal1, yLocal1, yOol1, yMix1, yAux1, family[1],
                       y_slab_scale[1], y_caux1);
  if (M > 1) {
    if (has_aux[2] == 1) {
      yAux2[1] = make_aux(yAux2_unscaled[1], y_prior_dist_for_aux[2],
                          y_prior_mean_for_aux[2], y_prior_scale_for_aux[2]);
      if (yAux2[1] > yAuxMaximum) 
        yAuxMaximum = yAux2[1];
    }
    if (yK[2] > 0) 
      yBeta2 = make_beta(z_yBeta2, y_prior_dist[2], y_prior_mean2,
                         y_prior_scale2, y_prior_df2,
                         y_global_prior_scale[2], yGlobal2, yLocal2, yOol2,
                         yMix2, yAux2, family[2], y_slab_scale[2], y_caux2);
  }
  if (M > 2) {
    if (has_aux[3] == 1) {
      yAux3[1] = make_aux(yAux3_unscaled[1], y_prior_dist_for_aux[3],
                          y_prior_mean_for_aux[3], y_prior_scale_for_aux[3]);
      if (yAux3[1] > yAuxMaximum) 
        yAuxMaximum = yAux3[1];
    }
    if (yK[3] > 0) 
      yBeta3 = make_beta(z_yBeta3, y_prior_dist[3], y_prior_mean3,
                         y_prior_scale3, y_prior_df3,
                         y_global_prior_scale[3], yGlobal3, yLocal3, yOol3,
                         yMix3, yAux3, family[3], y_slab_scale[3], y_caux3);
  }
  
  // group level params, under decov prior
  if (prior_dist_for_cov == 1) {
    int mark = 1;
    // cov matrix
    theta_L = make_theta_L(len_theta_L, p, yAuxMaximum, tau, b_prior_scale,
                           zeta, rho, z_T);
    // group-level params for first grouping factor
    if (bK1 > 0) 
      bMat1 = make_b_matrix(z_b, theta_L, p, l, 1);
    // group level params for second grouping factor
    if (bK2 > 0) 
      bMat2 = make_b_matrix(z_b, theta_L, p, l, 2);
  }
  
  // group-level params, under lkj prior
  else if (prior_dist_for_cov == 2) {
    // group-level params for first grouping factor
    if (bK1 == 1) 
      bMat1 = (bSd1[1] * z_bMat1)';
    else if (bK1 > 1) 
      bMat1 = (diag_pre_multiply(bSd1, bCholesky1) * z_bMat1)';
    // group level params for second grouping factor
    if (bK2 == 1) 
      bMat2 = (bSd2[1] * z_bMat2)';
    else if (bK2 > 1) 
      bMat2 = (diag_pre_multiply(bSd2, bCholesky2) * z_bMat2)';
  }
}
model {
  // Log likelihoods
  // increments target with mvmer log liks
  
  vector[yNeta[1]] yEta1; // linear predictor
  vector[yNeta[2]] yEta2;
  vector[yNeta[3]] yEta3;
  
  // Linear predictor for submodel 1
  if (M > 0) {
    int bMat1_colshift = 0; // column shift in bMat1
    int bMat2_colshift = 0; // column shift in bMat2
    yEta1 = evaluate_eta(yX1, y1_Z1, y1_Z2, y1_Z1_id, y1_Z2_id, yGamma1,
                         yBeta1, bMat1, bMat2, bMat1_colshift,
                         bMat2_colshift, intercept_type[1]);
  }
  
  // Linear predictor for submodel 2
  if (M > 1) {
    int bMat1_colshift = bK1_len[1]; // column shift in bMat1
    int bMat2_colshift = bK2_len[1]; // column shift in bMat2
    yEta2 = evaluate_eta(yX2, y2_Z1, y2_Z2, y2_Z1_id, y2_Z2_id, yGamma2,
                         yBeta2, bMat1, bMat2, bMat1_colshift,
                         bMat2_colshift, intercept_type[2]);
  }
  
  // Linear predictor for submodel 3
  if (M > 2) {
    int bMat1_colshift = sum(bK1_len[1 : 2]); // column shift in bMat1
    int bMat2_colshift = sum(bK2_len[1 : 2]); // column shift in bMat2
    yEta3 = evaluate_eta(yX3, y3_Z1, y3_Z2, y3_Z1_id, y3_Z2_id, yGamma3,
                         yBeta3, bMat1, bMat2, bMat1_colshift,
                         bMat2_colshift, intercept_type[3]);
  }
  
  // Log-likelihoods
  if (prior_PD == 0) {
    glm_lp(yReal1, yInt1, yEta1, yAux1, family[1], link[1], sum_log_y1,
           sqrt_y1, log_y1);
    if (M > 1) 
      glm_lp(yReal2, yInt2, yEta2, yAux2, family[2], link[2], sum_log_y2,
             sqrt_y2, log_y2);
    if (M > 2) 
      glm_lp(yReal3, yInt3, yEta3, yAux3, family[3], link[3], sum_log_y3,
             sqrt_y3, log_y3);
  }
  
  // Log priors
  // increments target with mvmer priors
  
  // Log-priors, auxiliary params
  if (has_aux[1] == 1) 
    aux_lp(yAux1_unscaled[1], y_prior_dist_for_aux[1],
           y_prior_scale_for_aux[1], y_prior_df_for_aux[1]);
  if (M > 1 && has_aux[2] == 1) 
    aux_lp(yAux2_unscaled[1], y_prior_dist_for_aux[2],
           y_prior_scale_for_aux[2], y_prior_df_for_aux[2]);
  if (M > 2 && has_aux[3] == 1) 
    aux_lp(yAux3_unscaled[1], y_prior_dist_for_aux[3],
           y_prior_scale_for_aux[3], y_prior_df_for_aux[3]);
  
  // Log priors, intercepts
  if (intercept_type[1] > 0) 
    gamma_lp(yGamma1[1], y_prior_dist_for_intercept[1],
             y_prior_mean_for_intercept[1], y_prior_scale_for_intercept[1],
             y_prior_df_for_intercept[1]);
  if (M > 1 && intercept_type[2] > 0) 
    gamma_lp(yGamma2[1], y_prior_dist_for_intercept[2],
             y_prior_mean_for_intercept[2], y_prior_scale_for_intercept[2],
             y_prior_df_for_intercept[2]);
  if (M > 2 && intercept_type[3] > 0) 
    gamma_lp(yGamma3[1], y_prior_dist_for_intercept[3],
             y_prior_mean_for_intercept[3], y_prior_scale_for_intercept[3],
             y_prior_df_for_intercept[3]);
  
  // Log priors, population level params
  if (yK[1] > 0) 
    beta_lp(z_yBeta1, y_prior_dist[1], y_prior_scale1, y_prior_df1,
            y_global_prior_df[1], yLocal1, yGlobal1, yMix1, yOol1,
            y_slab_df[1], y_caux1);
  if (M > 1 && yK[2] > 0) 
    beta_lp(z_yBeta2, y_prior_dist[2], y_prior_scale2, y_prior_df2,
            y_global_prior_df[2], yLocal2, yGlobal2, yMix2, yOol2,
            y_slab_df[2], y_caux2);
  if (M > 2 && yK[3] > 0) 
    beta_lp(z_yBeta3, y_prior_dist[3], y_prior_scale3, y_prior_df3,
            y_global_prior_df[3], yLocal3, yGlobal3, yMix3, yOol3,
            y_slab_df[3], y_caux3);
  
  // Log priors, group level terms
  if (prior_dist_for_cov == 1) {
    // decov
    real dummy = decov_lp(z_b, z_T, rho, zeta, tau, b_prior_regularization,
                          delta, b_prior_shape, t, p);
  } else if (prior_dist_for_cov == 2) {
    // lkj
    if (bK1 > 0) {
      // sds for group factor 1
      target += student_t_lpdf(bSd1 | b1_prior_df, 0, b1_prior_scale);
      // primitive coefs for group factor 1
      target += normal_lpdf(to_vector(z_bMat1) | 0, 1);
      // corr matrix for group factor 1
      if (bK1 > 1) 
        target += lkj_corr_cholesky_lpdf(bCholesky1 |
                    b1_prior_regularization);
    }
    if (bK2 > 0) {
      // sds for group factor 2
      target += student_t_lpdf(bSd2 | b2_prior_df, 0, b2_prior_scale);
      // primitive coefs for group factor 2
      target += normal_lpdf(to_vector(z_bMat2) | 0, 1);
      // corr matrix for group factor 2
      if (bK2 > 1) 
        target += lkj_corr_cholesky_lpdf(bCholesky2 |
                    b2_prior_regularization);
    }
  }
}
generated quantities {
  // declares and defines: mean_PPD, yAlpha{1,2,3}, b{1,2}, bCov{1,2}
  
  array[M] real mean_PPD;
  array[intercept_type[1] > 0] real yAlpha1;
  array[intercept_type[2] > 0] real yAlpha2;
  array[intercept_type[3] > 0] real yAlpha3;
  vector[prior_dist_for_cov == 2 && bK1 > 0 ? size(bCov1_idx) : 0] bCov1;
  vector[prior_dist_for_cov == 2 && bK2 > 0 ? size(bCov2_idx) : 0] bCov2;
  vector[bN1 * bK1] b1 = to_vector(bMat1'); // ensures same order as stan_glmer (make_b)
  vector[bN2 * bK2] b2 = to_vector(bMat2');
  
  // Evaluate mean_PPD
  {
    int bMat1_colshift = 0; // column shift in bMat1
    int bMat2_colshift = 0; // column shift in bMat2
    
    // Linear predictor for submodel 1
    if (M > 0) {
      vector[yNeta[1]] yEta1 = evaluate_mu(
                                           // linear predictor
                                           evaluate_eta(yX1, y1_Z1, y1_Z2,
                                                        y1_Z1_id, y1_Z2_id,
                                                        yGamma1, yBeta1,
                                                        bMat1, bMat2,
                                                        bMat1_colshift,
                                                        bMat2_colshift,
                                                        intercept_type[1]),
                                           family[1], link[1]);
      mean_PPD[1] = mean_PPD_rng(yEta1, yAux1, family[1]);
    }
    
    // Linear predictor for submodel 2
    if (M > 1) {
      vector[yNeta[2]] yEta2;
      bMat1_colshift += bK1_len[1];
      bMat2_colshift += bK2_len[1];
      yEta2 = evaluate_mu(
                          evaluate_eta(yX2, y2_Z1, y2_Z2, y2_Z1_id, y2_Z2_id,
                                       yGamma2, yBeta2, bMat1, bMat2,
                                       bMat1_colshift, bMat2_colshift,
                                       intercept_type[2]),
                          family[2], link[2]);
      mean_PPD[2] = mean_PPD_rng(yEta2, yAux2, family[2]);
    }
    
    // Linear predictor for submodel 3
    if (M > 2) {
      vector[yNeta[3]] yEta3;
      bMat1_colshift += bK1_len[2];
      bMat2_colshift += bK2_len[2];
      yEta3 = evaluate_mu(
                          evaluate_eta(yX3, y3_Z1, y3_Z2, y3_Z1_id, y3_Z2_id,
                                       yGamma3, yBeta3, bMat1, bMat2,
                                       bMat1_colshift, bMat2_colshift,
                                       intercept_type[3]),
                          family[3], link[3]);
      mean_PPD[3] = mean_PPD_rng(yEta3, yAux3, family[3]);
    }
  }
  
  // Transform intercept parameters
  if (intercept_type[1] > 0) 
    yAlpha1[1] = yGamma1[1] - dot_product(yXbar1, yBeta1);
  if (M > 1 && intercept_type[2] > 0) 
    yAlpha2[1] = yGamma2[1] - dot_product(yXbar2, yBeta2);
  if (M > 2 && intercept_type[3] > 0) 
    yAlpha3[1] = yGamma3[1] - dot_product(yXbar3, yBeta3);
  
  // Transform variance-covariance matrices
  
  // Grouping factor 1
  if (prior_dist_for_cov == 2 && bK1 == 1) {
    bCov1[1] = bSd1[1] * bSd1[1];
  } else if (prior_dist_for_cov == 2 && bK1 > 1) {
    bCov1 = to_vector(
                      quad_form_diag(
                                     multiply_lower_tri_self_transpose(
                                       bCholesky1),
                                     bSd1))[bCov1_idx];
  }
  
  // Grouping factor 2
  if (prior_dist_for_cov == 2 && bK2 == 1) {
    bCov2[1] = bSd2[1] * bSd2[1];
  } else if (prior_dist_for_cov == 2 && bK2 > 1) {
    bCov2 = to_vector(
                      quad_form_diag(
                                     multiply_lower_tri_self_transpose(
                                       bCholesky2),
                                     bSd2))[bCov2_idx];
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
