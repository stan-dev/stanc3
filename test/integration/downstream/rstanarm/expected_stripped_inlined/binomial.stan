/* $ stanc --include-paths=.. --auto-format --allow-undefined --canonicalize=strip-comments,includes ../binomial.stan : [exit 0] */
functions {
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
    
    for (i in 1 : size(p)) {
      int nc = p[i];
      if (nc == 1) {
        theta_L[theta_L_mark] = tau[i] * scale[i] * dispersion;
        
        theta_L_mark += 1;
      } else {
        matrix[nc, nc] T_i;
        real std_dev;
        real T21;
        real trace_T_i = square(tau[i] * scale[i] * dispersion) * nc;
        vector[nc] pi = segment(zeta, zeta_mark, nc);
        pi /= sum(pi);
        
        zeta_mark += nc;
        std_dev = sqrt(pi[1] * trace_T_i);
        T_i[1, 1] = std_dev;
        
        std_dev = sqrt(pi[2] * trace_T_i);
        T21 = 2.0 * rho[rho_mark] - 1.0;
        rho_mark += 1;
        T_i[2, 2] = std_dev * sqrt(1.0 - square(T21));
        T_i[2, 1] = std_dev * T21;
        
        for (r in 2 : (nc - 1)) {
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
        
        for (c in 1 : nc) 
          for (r in c : nc) {
            theta_L[theta_L_mark] = T_i[r, c];
            theta_L_mark += 1;
          }
      }
    }
    return theta_L;
  }
  
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
  
  vector csr_matrix_times_vector2(int m, int n, vector w, array[] int v,
                                  array[] int u, vector b);
  
  real make_lower(int family, int link) {
    if (family == 1) 
      return negative_infinity();
    if (family <= 3) {
      if (link == 2) 
        return negative_infinity();
      return 0;
    }
    return negative_infinity();
  }
  
  real make_upper(int family, int link) {
    if (family == 4 && link == 5) 
      return 0;
    return positive_infinity();
  }
  
  vector linkinv_binom(vector eta, int link) {
    if (link == 1) 
      return (inv_logit(eta));
    else if (link == 2) 
      return (Phi(eta));
    else if (link == 3) 
      return (atan(eta) / pi() + 0.5);
    else if (link == 4) 
      return (exp(eta));
    else if (link == 5) 
      return (inv_cloglog(eta));
    else 
      reject("Invalid link");
    return eta;
  }
  
  real ll_binom_lp(array[] int y, array[] int trials, vector eta, int link) {
    if (link == 1) 
      target += binomial_logit_lpmf(y | trials, eta);
    else if (link < 4) 
      target += binomial_lpmf(y | trials, linkinv_binom(eta, link));
    else if (link == 4) {
      for (n in 1 : num_elements(y)) {
        target += y[n] * eta[n];
        target += (trials[n] - y[n]) * log1m_exp(eta[n]);
        target += lchoose(trials[n], y[n]);
      }
    } else if (link == 5) {
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
  
  vector pw_binom(array[] int y, array[] int trials, vector eta, int link) {
    int N = rows(eta);
    vector[N] ll;
    if (link == 1) {
      for (n in 1 : N) 
        ll[n] = binomial_logit_lpmf(y[n] | trials[n], eta[n]);
    } else if (link <= 5) {
      vector[N] pi = linkinv_binom(eta, link);
      for (n in 1 : N) 
        ll[n] = binomial_lpmf(y[n] | trials[n], pi[n]);
    } else 
      reject("Invalid link");
    return ll;
  }
}
data {
  int<lower=0> N;
  int<lower=0> K;
  
  vector[K] xbar;
  int<lower=0, upper=1> dense_X;
  array[dense_X] matrix[N, K] X;
  
  int<lower=0> nnz_X;
  vector[nnz_X] w_X;
  array[nnz_X] int<lower=0, upper=K - 1> v_X;
  
  array[dense_X ? 0 : N + 1] int<lower=0, upper=rows(w_X) + 1> u_X;
  
  int<lower=0> K_smooth;
  matrix[N, K_smooth] S;
  array[K_smooth] int<lower=1> smooth_map;
  
  array[N] int<lower=0> y;
  array[N] int<lower=0> trials;
  
  int<lower=0, upper=1> prior_PD;
  int<lower=0, upper=1> compute_mean_PPD;
  
  int<lower=0, upper=1> has_intercept;
  
  int<lower=1> link;
  
  int<lower=0, upper=7> prior_dist;
  int<lower=0, upper=2> prior_dist_for_intercept;
  
  int<lower=0, upper=3> prior_dist_for_aux;
  
  int<lower=0, upper=3> prior_dist_for_smooth;
  
  int<lower=0, upper=1> has_weights;
  vector[has_weights ? N : 0] weights;
  
  int<lower=0, upper=1> has_offset;
  vector[has_offset ? N : 0] offset_;
  
  int<lower=5, upper=5> family;
  
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
  real<lower=0> global_prior_df;
  real<lower=0> global_prior_scale;
  real<lower=0> slab_df;
  real<lower=0> slab_scale;
  array[prior_dist == 7 ? K : 0] int<lower=2> num_normals;
  
  int<lower=0> t;
  array[t] int<lower=1> p;
  array[t] int<lower=1> l;
  int<lower=0> q;
  int<lower=0> len_theta_L;
  
  vector<lower=0>[t] shape;
  vector<lower=0>[t] scale;
  int<lower=0> len_concentration;
  array[len_concentration] real<lower=0> concentration;
  int<lower=0> len_regularization;
  array[len_regularization] real<lower=0> regularization;
  
  int<lower=0> num_non_zero;
  vector[num_non_zero] w;
  array[num_non_zero] int<lower=0, upper=q - 1> v;
  array[t > 0 ? N + 1 : 0] int<lower=0, upper=rows(w) + 1> u;
  int<lower=0, upper=1> special_case;
}
transformed data {
  real aux = not_a_number();
  array[special_case ? t : 0, N] int<lower=1> V = make_V(N,
                                                         special_case ? t : 0,
                                                         v);
  
  int<lower=0> len_z_T = 0;
  int<lower=0> len_var_group = sum(p) * (t > 0);
  int<lower=0> len_rho = sum(p) - t;
  int<lower=0, upper=1> is_continuous = 0;
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
}
parameters {
  array[has_intercept] real<upper=(link == 4 ? 0.0 : positive_infinity())> gamma;
  
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
  } else if (prior_dist == 5) 
    beta = prior_mean + prior_scale .* sqrt(2 * mix[1]) .* z_beta;
  else if (prior_dist == 6) 
    beta = prior_mean
           + one_over_lambda[1] * prior_scale .* sqrt(2 * mix[1]) .* z_beta;
  else if (prior_dist == 7) {
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
  vector[N] eta;
  if (K > 0) {
    if (dense_X) 
      eta = X[1] * beta;
    else 
      eta = csr_matrix_times_vector2(N, K, w_X, v_X, u_X, beta);
  } else 
    eta = rep_vector(0.0, N);
  if (has_offset == 1) 
    eta += offset_;
  if (K_smooth) 
    eta += S * beta_smooth;
  
  if (t > 0) {
    if (special_case) 
      for (i in 1 : t) 
        eta += b[V[i]];
    else 
      eta += csr_matrix_times_vector2(N, q, w, v, u, b);
  }
  if (has_intercept == 1) {
    if (link != 4) 
      eta += gamma[1];
    else 
      eta += gamma[1] - max(eta);
  } else {
    eta += dot_product(xbar, beta);
  }
  
  if (has_weights == 0 && prior_PD == 0) {
    real dummy = ll_binom_lp(y, trials, eta, link);
  } else if (prior_PD == 0) 
    target += dot_product(weights, pw_binom(y, trials, eta, link));
  
  if (prior_dist == 1) 
    target += normal_lpdf(z_beta | 0, 1);
  else if (prior_dist == 2) 
    target += normal_lpdf(z_beta | 0, 1);
  else if (prior_dist == 3) {
    real log_half = -0.693147180559945286;
    target += normal_lpdf(z_beta | 0, 1);
    target += normal_lpdf(local[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(local[2] | 0.5 * prior_df, 0.5 * prior_df);
    target += normal_lpdf(global[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(global[2] | 0.5 * global_prior_df,
                             0.5 * global_prior_df);
    target += inv_gamma_lpdf(caux | 0.5 * slab_df, 0.5 * slab_df);
  } else if (prior_dist == 4) {
    real log_half = -0.693147180559945286;
    target += normal_lpdf(z_beta | 0, 1);
    target += normal_lpdf(local[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(local[2] | 0.5 * prior_df, 0.5 * prior_df);
    target += normal_lpdf(local[3] | 0, 1) - log_half;
    
    target += inv_gamma_lpdf(local[4] | 0.5 * prior_scale, 0.5 * prior_scale);
    target += normal_lpdf(global[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(global[2] | 0.5 * global_prior_df,
                             0.5 * global_prior_df);
    target += inv_gamma_lpdf(caux | 0.5 * slab_df, 0.5 * slab_df);
  } else if (prior_dist == 5) {
    target += normal_lpdf(z_beta | 0, 1);
    target += exponential_lpdf(mix[1] | 1);
  } else if (prior_dist == 6) {
    target += normal_lpdf(z_beta | 0, 1);
    target += exponential_lpdf(mix[1] | 1);
    target += chi_square_lpdf(one_over_lambda[1] | prior_df[1]);
  } else if (prior_dist == 7) {
    target += normal_lpdf(z_beta | 0, 1);
  }
  
  if (has_intercept == 1) {
    if (prior_dist_for_intercept == 1) 
      target += normal_lpdf(gamma | prior_mean_for_intercept,
                            prior_scale_for_intercept);
    else if (prior_dist_for_intercept == 2) 
      target += student_t_lpdf(gamma | prior_df_for_intercept,
                               prior_mean_for_intercept,
                               prior_scale_for_intercept);
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
    vector[N] pi;
    
    vector[N] eta;
    if (K > 0) {
      if (dense_X) 
        eta = X[1] * beta;
      else 
        eta = csr_matrix_times_vector2(N, K, w_X, v_X, u_X, beta);
    } else 
      eta = rep_vector(0.0, N);
    if (has_offset == 1) 
      eta += offset_;
    if (K_smooth) 
      eta += S * beta_smooth;
    
    if (t > 0) {
      if (special_case) 
        for (i in 1 : t) 
          eta += b[V[i]];
      else 
        eta += csr_matrix_times_vector2(N, q, w, v, u, b);
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
      eta += dot_product(xbar, beta);
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
