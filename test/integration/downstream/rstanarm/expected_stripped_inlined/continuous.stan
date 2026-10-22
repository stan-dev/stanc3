/* $ stanc --include-paths=.. --auto-format --allow-undefined --canonicalize=strip-comments,includes ../continuous.stan : [exit 0] */
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
  
  vector linkinv_gauss(vector eta, int link) {
    if (link == 1) 
      return eta;
    else if (link == 2) 
      return exp(eta);
    else if (link == 3) 
      return inv(eta);
    else 
      reject("Invalid link");
    return eta;
  }
  
  vector pw_gauss(vector y, vector eta, real sigma, int link) {
    return -0.5 * log(6.283185307179586232 * sigma)
           - 0.5 * square((y - linkinv_gauss(eta, link)) / sigma);
  }
  
  vector linkinv_gamma(vector eta, int link) {
    if (link == 1) 
      return eta;
    else if (link == 2) 
      return exp(eta);
    else if (link == 3) 
      return inv(eta);
    else 
      reject("Invalid link");
    return eta;
  }
  
  real GammaReg(vector y, vector eta, real shape, int link, real sum_log_y) {
    real ret = rows(y) * (shape * log(shape) - lgamma(shape))
               + (shape - 1) * sum_log_y;
    if (link == 2) 
      ret -= shape * sum(eta) + shape * sum(y ./ exp(eta));
    else if (link == 1) 
      ret -= shape * sum(log(eta)) + shape * sum(y ./ eta);
    else if (link == 3) 
      ret += shape * sum(log(eta)) - shape * dot_product(eta, y);
    else 
      reject("Invalid link");
    return ret;
  }
  
  vector pw_gamma(vector y, vector eta, real shape, int link) {
    int N = rows(eta);
    vector[N] ll;
    if (link == 3) {
      for (n in 1 : N) {
        ll[n] = gamma_lpdf(y[n] | shape, shape * eta[n]);
      }
    } else if (link == 2) {
      for (n in 1 : N) {
        ll[n] = gamma_lpdf(y[n] | shape, shape / exp(eta[n]));
      }
    } else if (link == 1) {
      for (n in 1 : N) {
        ll[n] = gamma_lpdf(y[n] | shape, shape / eta[n]);
      }
    } else 
      reject("Invalid link");
    return ll;
  }
  
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
    return eta;
  }
  
  real inv_gaussian(vector y, vector mu, real lambda, real sum_log_y,
                    vector sqrt_y) {
    return 0.5 * rows(y) * log(lambda / 6.283185307179586232)
           - 1.5 * sum_log_y
           - 0.5 * lambda * dot_self((y - mu) ./ (mu .* sqrt_y));
  }
  
  vector pw_inv_gaussian(vector y, vector eta, real lambda, int link,
                         vector log_y, vector sqrt_y) {
    vector[rows(y)] mu = linkinv_inv_gaussian(eta, link);
    return -0.5 * lambda * square((y - mu) ./ (mu .* sqrt_y))
           + 0.5 * log(lambda / 6.283185307179586232) - 1.5 * log_y;
  }
  
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
  
  vector linkinv_beta(vector eta, int link) {
    if (link == 1) 
      return inv_logit(eta);
    else if (link == 2) 
      return Phi(eta);
    else if (link == 3) 
      return inv_cloglog(eta);
    else if (link == 4) 
      return 0.5 + atan(eta) / pi();
    else if (link == 5) 
      return exp(eta);
    else if (link == 6) 
      return 1 - inv_cloglog(-eta);
    else 
      reject("invalid link");
    return eta;
  }
  
  vector linkinv_beta_z(vector eta, int link) {
    if (link == 1) 
      return exp(eta);
    else if (link == 2) 
      return eta;
    else if (link == 3) 
      return square(eta);
    else 
      reject("Invalid link");
    return eta;
  }
  
  vector pw_beta(vector y, vector eta, real dispersion, int link) {
    vector[rows(y)] ll;
    vector[rows(y)] mu = linkinv_beta(eta, link);
    for (n in 1 : rows(y)) {
      ll[n] = beta_lpdf(y[n] | mu[n] * dispersion, (1 - mu[n]) * dispersion);
    }
    return ll;
  }
  
  vector pw_beta_z(vector y, vector eta, vector eta_z, int link,
                   int link_phi) {
    vector[rows(y)] ll;
    vector[rows(y)] mu = linkinv_beta(eta, link);
    vector[rows(y)] mu_z = linkinv_beta_z(eta_z, link_phi);
    for (n in 1 : rows(y)) {
      ll[n] = beta_lpdf(y[n] | mu[n] * mu_z[n], (1 - mu[n]) * mu_z[n]);
    }
    return ll;
  }
  
  vector SS_asymp(vector input, matrix Phi_);
  vector SS_asympOff(vector input, matrix Phi_);
  vector SS_asympOrig(vector input, matrix Phi_);
  vector SS_biexp(vector input, matrix Phi_);
  vector SS_fol(vector Dose, vector input, matrix Phi_);
  vector SS_fpl(vector input, matrix Phi_);
  vector SS_gompertz(vector x, matrix Phi_);
  vector SS_logis(vector input, matrix Phi_);
  vector SS_micmen(vector input, matrix Phi_);
  vector SS_weibull(vector x, matrix Phi_);
  
  vector SS_asymp(vector input, matrix Phi_) {
    if (rows(Phi_) > 1) {
      vector[rows(Phi_)] Asym = Phi_[ : , 1];
      return Asym + (Phi_[ : , 2] - Asym) .* exp(-exp(Phi_[ : , 3]) .* input);
    } else {
      real Asym = Phi_[1, 1];
      return Asym + (Phi_[1, 2] - Asym) * exp(-exp(Phi_[1, 3]) * input);
    }
  }
  
  vector SS_asympOff(vector input, matrix Phi_) {
    if (rows(Phi_) > 1) 
      return Phi_[ : , 1]
             .* (1 - exp(-exp(Phi_[ : , 2]) .* (input - Phi_[ : , 3])));
    else 
      return Phi_[1, 1] * (1 - exp(-exp(Phi_[1, 2]) * (input - Phi_[1, 3])));
  }
  
  vector SS_asympOrig(vector input, matrix Phi_) {
    if (rows(Phi_) > 1) 
      return Phi_[ : , 1] .* (1 - exp(-exp(Phi_[ : , 2]) .* input));
    else 
      return Phi_[1, 1] * (1 - exp(-exp(Phi_[1, 2]) * input));
  }
  
  vector SS_biexp(vector input, matrix Phi_) {
    if (rows(Phi_) > 1) 
      return Phi_[ : , 1] .* exp(-exp(Phi_[ : , 2]) .* input)
             + Phi_[ : , 3] .* exp(-exp(Phi_[ : , 4]) .* input);
    else 
      return Phi_[1, 1] * exp(-exp(Phi_[1, 2]) * input)
             + Phi_[1, 3] * exp(-exp(Phi_[1, 4]) * input);
  }
  
  vector SS_fol(vector Dose, vector input, matrix Phi_) {
    int Phi__rows = rows(Phi_);
    if (Phi__rows > 1) {
      vector[Phi__rows] lKe = Phi_[ : , 1];
      vector[Phi__rows] lKa = Phi_[ : , 2];
      vector[Phi__rows] exp_lKe = exp(lKe);
      vector[Phi__rows] exp_lKa = exp(lKa);
      return Dose .* exp(lKe + lKa - Phi_[ : , 3])
             .* (exp(-exp_lKe .* input) - exp(-exp_lKa .* input))
             ./ (exp_lKa - exp_lKe);
    } else {
      real lKe = Phi_[1, 1];
      real lKa = Phi_[1, 2];
      real exp_lKe = exp(lKe);
      real exp_lKa = exp(lKa);
      return Dose * exp(lKe + lKa - Phi_[1, 3])
             .* (exp(-exp_lKe * input) - exp(-exp_lKa * input))
             / (exp_lKa - exp_lKe);
    }
  }
  
  vector SS_fpl(vector input, matrix Phi_) {
    if (rows(Phi_) > 1) {
      vector[rows(Phi_)] A = Phi_[ : , 1];
      return A
             + (Phi_[ : , 2] - A)
               ./ (1 + exp((Phi_[ : , 3] - input) ./ exp(Phi_[ : , 4])));
    } else {
      real A = Phi_[1, 1];
      return A
             + rep_vector(Phi_[1, 2] - A, rows(input))
               ./ (1 + exp((Phi_[1, 3] - input) / exp(Phi_[1, 4])));
    }
  }
  
  vector SS_gompertz(vector x, matrix Phi_) {
    vector[rows(x)] out;
    if (rows(Phi_) > 1) 
      for (i in 1 : rows(x)) 
        out[i] = Phi_[i, 1] * exp(-Phi_[i, 2] * Phi_[i, 3] ^ x[i]);
    else {
      real Asym = Phi_[1, 1];
      real b2 = Phi_[1, 2];
      real b3 = Phi_[1, 3];
      for (i in 1 : rows(x)) 
        out[i] = Asym * exp(-b2 * b3 ^ x[i]);
    }
    return out;
  }
  
  vector SS_logis(vector input, matrix Phi_) {
    if (rows(Phi_) > 1) 
      return Phi_[ : , 1]
             ./ (1 + exp((Phi_[ : , 2] - input) ./ exp(Phi_[ : , 3])));
    else 
      return rep_vector(Phi_[1, 1], rows(input))
             ./ (1 + exp((Phi_[1, 2] - input) / exp(Phi_[1, 3])));
  }
  
  vector SS_micmen(vector input, matrix Phi_) {
    if (rows(Phi_) > 1) 
      return Phi_[ : , 1] .* input ./ (Phi_[ : , 2] + input);
    else 
      return Phi_[1, 1] * input ./ (Phi_[1, 2] + input);
  }
  
  vector SS_weibull(vector x, matrix Phi_) {
    vector[rows(x)] out;
    if (rows(Phi_) > 1) 
      for (i in 1 : rows(x)) 
        out[i] = Phi_[i, 1]
                 - Phi_[i, 2] * exp(-exp(Phi_[i, 3]) * x[i] ^ Phi_[i, 4]);
    else {
      real Asym = Phi_[1, 1];
      real Drop = Phi_[1, 2];
      real lrc = Phi_[1, 3];
      real pwr = Phi_[1, 4];
      for (i in 1 : rows(x)) 
        out[i] = Asym - Drop * exp(-exp(lrc) * x[i] ^ pwr);
    }
    return out;
  }
  
  matrix reshape_vec(vector x, int Rows, int Cols) {
    matrix[Rows, Cols] out;
    int pos = 1;
    if (rows(x) != Rows * Cols) 
      reject("x is the wrong length");
    for (c in 1 : Cols) 
      for (r in 1 : Rows) {
        out[r, c] = x[pos];
        pos += 1;
      }
    return out;
  }
  
  vector test_csr_matrix_times_vector(
           int m,
           int n,
           vector w,
           array[] int v,
           array[] int u,
           vector b
         ) {
    return csr_matrix_times_vector(m, n, w, v, u, b);
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
  
  int<lower=0> len_y;
  real lb_y;
  real<lower=lb_y> ub_y;
  vector<lower=lb_y, upper=ub_y>[len_y] y;
  int<lower=1, upper=4> family;
  
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
  
  int<lower=0, upper=1> has_intercept_z;
  int<lower=0> link_phi;
  int<lower=0> z_dim;
  matrix[N, z_dim] betareg_z;
  row_vector[z_dim] zbar;
  
  int<lower=0, upper=7> prior_dist_z;
  int<lower=0, upper=2> prior_dist_for_intercept_z;
  vector<lower=0>[z_dim] prior_scale_z;
  real<lower=0> prior_scale_for_intercept_z;
  vector[z_dim] prior_mean_z;
  real prior_mean_for_intercept_z;
  vector<lower=0>[z_dim] prior_df_z;
  real<lower=0> prior_df_for_intercept_z;
  real<lower=0> global_prior_scale_z;
  real<lower=0> global_prior_df_z;
  real<lower=0> slab_df_z;
  real<lower=0> slab_scale_z;
  array[prior_dist_z == 7 ? z_dim : 0] int<lower=2> num_normals_z;
  
  int<lower=0, upper=10> SSfun;
  vector[SSfun > 0 ? len_y : 0] input;
  vector[SSfun == 5 ? len_y : 0] Dose;
}
transformed data {
  vector[family == 3 ? len_y : 0] sqrt_y;
  vector[family == 3 ? len_y : 0] log_y;
  real sum_log_y = family == 1 ? not_a_number() : sum(log(y));
  array[special_case ? t : 0, len_y] int<lower=1> V = make_V(len_y,
                                                             special_case ? t
                                                             : 0, v);
  int<lower=0> hs_z;
  
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
  
  if (prior_dist_z <= 2) 
    hs_z = 0;
  else if (prior_dist_z == 3) 
    hs_z = 2;
  else if (prior_dist_z == 4) 
    hs_z = 4;
  else 
    hs_z = 0;
  
  is_continuous = 1;
  
  if (family == 3) {
    sqrt_y = sqrt(y);
    log_y = log(y);
  }
}
parameters {
  array[has_intercept] real<lower=make_lower(family, link),
                            upper=make_upper(family, link)> gamma;
  
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
  
  real<lower=0> aux_unscaled;
  
  vector[prior_dist_z == 7 ? sum(num_normals_z) : z_dim] z_omega;
  array[has_intercept_z] real<lower=(link_phi <= 1 ? negative_infinity() : 0)> gamma_z;
  array[hs_z] real<lower=0> global_z;
  array[hs_z] vector<lower=0>[z_dim] local_z;
  array[hs_z > 0] real<lower=0> caux_z;
  array[prior_dist_z == 5 || prior_dist_z == 6] vector<lower=0>[z_dim] S_z;
  array[prior_dist_z == 6] real<lower=0> one_over_lambda_z;
}
transformed parameters {
  real aux = prior_dist_for_aux == 0 ? aux_unscaled
             : (prior_dist_for_aux <= 2
                ? prior_scale_for_aux * aux_unscaled + prior_mean_for_aux
                : prior_scale_for_aux * aux_unscaled);
  
  vector[z_dim] omega;
  
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
  
  if (prior_dist_z == 0) 
    omega = z_omega;
  else if (prior_dist_z == 1) 
    omega = z_omega .* prior_scale_z + prior_mean_z;
  else if (prior_dist_z == 2) 
    for (k in 1 : z_dim) {
      real left = CFt(omega[k], prior_df_z[k]);
      omega[k] = left * prior_scale_z[k] + prior_mean_z[k];
    }
  else if (prior_dist_z == 3) 
    omega = hs_prior(z_omega, global_z, local_z, global_prior_scale, 1,
                     square(slab_scale_z) * caux_z[1]);
  else if (prior_dist_z == 4) 
    omega = hsplus_prior(z_omega, global_z, local_z, global_prior_scale, 1,
                         square(slab_scale_z) * caux_z[1]);
  else if (prior_dist_z == 5) 
    omega = prior_mean_z + prior_scale_z .* sqrt(2 * S_z[1]) .* z_omega;
  else if (prior_dist_z == 6) 
    omega = prior_mean_z
            + one_over_lambda_z[1] * prior_scale_z .* sqrt(2 * S_z[1])
              .* z_omega;
  else if (prior_dist_z == 7) {
    int z_pos = 1;
    for (k in 1 : z_dim) {
      omega[k] = z_omega[z_pos];
      z_pos += 1;
      for (n in 2 : num_normals_z[k]) {
        omega[k] *= z_omega[z_pos];
        z_pos += 1;
      }
      omega[k] *= prior_scale_z[k] ^ num_normals_z[k];
      omega[k] += prior_mean_z[k];
    }
  }
  
  if (prior_dist_for_aux == 0) 
    aux = aux_unscaled;
  else {
    aux = prior_scale_for_aux * aux_unscaled;
    if (prior_dist_for_aux <= 2) 
      aux += prior_mean_for_aux;
  }
  
  if (t > 0) {
    if (special_case == 1) {
      int start = 1;
      theta_L = scale .* tau * aux;
      if (t == 1) 
        b = theta_L[1] * z_b;
      else 
        for (i in 1 : t) {
          int end = start + l[i] - 1;
          b[start : end] = theta_L[i] * z_b[start : end];
          start = end + 1;
        }
    } else {
      theta_L = make_theta_L(len_theta_L, p, aux, tau, scale, zeta, rho, z_T);
      b = make_b(z_b, theta_L, p, l);
    }
  }
}
model {
  vector[N] eta_z;
  
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
    if ((family == 1 || link == 2) || (family == 4 && link != 5)) 
      eta += gamma[1];
    else if (family == 4 && link == 5) 
      eta += gamma[1] - max(eta);
    else 
      eta += gamma[1] - min(eta);
  } else {
    eta += dot_product(xbar, beta);
  }
  
  if (SSfun > 0) {
    matrix[len_y, K] P = reshape_vec(eta, len_y, K);
    if (SSfun < 5) {
      if (SSfun <= 2) {
        if (SSfun == 1) 
          target += normal_lpdf(y | SS_asymp(input, P), aux);
        else 
          target += normal_lpdf(y | SS_asympOff(input, P), aux);
      } else if (SSfun == 3) 
        target += normal_lpdf(y | SS_asympOrig(input, P), aux);
      else {
        for (i in 1 : len_y) 
          P[i, 1] += exp(P[i, 3]);
        target += normal_lpdf(y | SS_biexp(input, P), aux);
      }
    } else {
      if (SSfun <= 7) {
        if (SSfun == 5) 
          target += normal_lpdf(y | SS_fol(Dose, input, P), aux);
        else if (SSfun == 6) 
          target += normal_lpdf(y | SS_fpl(input, P), aux);
        else 
          target += normal_lpdf(y | SS_gompertz(input, P), aux);
      } else {
        if (SSfun == 8) 
          target += normal_lpdf(y | SS_logis(input, P), aux);
        else if (SSfun == 9) 
          target += normal_lpdf(y | SS_micmen(input, P), aux);
        else 
          target += normal_lpdf(y | SS_weibull(input, P), aux);
      }
    }
  } else if (has_weights == 0 && prior_PD == 0) {
    if (family == 4 && z_dim > 0 && link_phi > 0) {
      eta_z = betareg_z * omega;
    } else if (family == 4 && z_dim == 0 && has_intercept_z == 1) {
      eta_z = rep_vector(0.0, N);
    }
    
    if (has_intercept_z == 1) {
      if (link_phi > 1) {
        eta_z += gamma_z[1] - min(eta_z);
      } else {
        eta_z += gamma_z[1];
      }
    } else {
      if (link_phi > 1) {
        eta_z += dot_product(zbar, omega) - min(eta_z);
      } else {
        eta_z += dot_product(zbar, omega);
      }
    }
    if (family == 1) {
      if (link == 1) 
        target += normal_lpdf(y | eta, aux);
      else if (link == 2) 
        target += normal_lpdf(y | exp(eta), aux);
      else 
        target += normal_lpdf(y | inv(eta), aux);
    } else if (family == 2) {
      target += GammaReg(y, eta, aux, link, sum_log_y);
    } else if (family == 3) {
      target += inv_gaussian(y, linkinv_inv_gaussian(eta, link), aux,
                             sum_log_y, sqrt_y);
    } else if (family == 4 && link_phi == 0) {
      vector[N] mu;
      mu = linkinv_beta(eta, link);
      target += beta_lpdf(y | mu * aux, (1 - mu) * aux);
    } else if (family == 4 && link_phi > 0) {
      vector[N] mu;
      vector[N] mu_z;
      mu = linkinv_beta(eta, link);
      mu_z = linkinv_beta_z(eta_z, link_phi);
      target += beta_lpdf(y | rows_dot_product(mu, mu_z),
                          rows_dot_product((1 - mu), mu_z));
    }
  } else if (prior_PD == 0) {
    vector[N] summands;
    if (family == 1) 
      summands = pw_gauss(y, eta, aux, link);
    else if (family == 2) 
      summands = pw_gamma(y, eta, aux, link);
    else if (family == 3) 
      summands = pw_inv_gaussian(y, eta, aux, link, log_y, sqrt_y);
    else if (family == 4 && link_phi == 0) 
      summands = pw_beta(y, eta, aux, link);
    else if (family == 4 && link_phi > 0) 
      summands = pw_beta_z(y, eta, eta_z, link, link_phi);
    target += dot_product(weights, summands);
  }
  
  if (prior_dist_for_aux > 0 && prior_scale_for_aux > 0) {
    real log_half = -0.693147180559945286;
    if (prior_dist_for_aux == 1) 
      target += normal_lpdf(aux_unscaled | 0, 1) - log_half;
    else if (prior_dist_for_aux == 2) 
      target += student_t_lpdf(aux_unscaled | prior_df_for_aux, 0, 1)
                - log_half;
    else 
      target += exponential_lpdf(aux_unscaled | 1);
  }
  
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
  
  if (prior_dist_z == 1) 
    target += normal_lpdf(z_omega | 0, 1);
  else if (prior_dist_z == 2) 
    target += normal_lpdf(z_omega | 0, 1);
  else if (prior_dist_z == 3) {
    real log_half = -0.693147180559945286;
    target += normal_lpdf(z_omega | 0, 1);
    target += normal_lpdf(local_z[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(local_z[2] | 0.5 * prior_df_z, 0.5 * prior_df_z);
    target += normal_lpdf(global_z[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(global_z[2] | 0.5 * global_prior_df_z,
                             0.5 * global_prior_df_z);
    target += inv_gamma_lpdf(caux_z | 0.5 * slab_df_z, 0.5 * slab_df_z);
  } else if (prior_dist_z == 4) {
    real log_half = -0.693147180559945286;
    target += normal_lpdf(z_omega | 0, 1);
    target += normal_lpdf(local_z[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(local_z[2] | 0.5 * prior_df_z, 0.5 * prior_df_z);
    target += normal_lpdf(local_z[3] | 0, 1) - log_half;
    
    target += inv_gamma_lpdf(local_z[4] | 0.5 * prior_scale_z,
                             0.5 * prior_scale_z);
    target += normal_lpdf(global_z[1] | 0, 1) - log_half;
    target += inv_gamma_lpdf(global_z[2] | 0.5, 0.5);
    target += inv_gamma_lpdf(caux_z | 0.5 * slab_df_z, 0.5 * slab_df_z);
  } else if (prior_dist_z == 5) {
    target += normal_lpdf(z_omega | 0, 1);
    target += exponential_lpdf(S_z[1] | 1);
  } else if (prior_dist_z == 6) {
    target += normal_lpdf(z_omega | 0, 1);
    target += exponential_lpdf(S_z[1] | 1);
    target += chi_square_lpdf(one_over_lambda_z[1] | prior_df_z[1]);
  } else if (prior_dist_z == 7) {
    target += normal_lpdf(z_omega | 0, 1);
  }
  
  if (has_intercept_z == 1) {
    if (prior_dist_for_intercept_z == 1) 
      target += normal_lpdf(gamma_z | prior_mean_for_intercept_z,
                            prior_scale_for_intercept_z);
    else if (prior_dist_for_intercept_z == 2) 
      target += student_t_lpdf(gamma_z | prior_df_for_intercept_z,
                               prior_mean_for_intercept_z,
                               prior_scale_for_intercept_z);
  }
  
  if (t > 0) {
    real dummy = decov_lp(z_b, z_T, rho, zeta, tau, regularization, delta,
                          shape, t, p);
  }
}
generated quantities {
  real mean_PPD = compute_mean_PPD ? 0 : negative_infinity();
  array[has_intercept] real alpha;
  array[has_intercept_z] real omega_int;
  
  if (has_intercept == 1) {
    if (dense_X) 
      alpha[1] = gamma[1] - dot_product(xbar, beta);
    else 
      alpha[1] = gamma[1];
  }
  if (has_intercept_z == 1) {
    omega_int[1] = gamma_z[1] - dot_product(zbar, omega);
  }
  
  if (compute_mean_PPD) {
    vector[N] eta_z;
    
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
      if (make_lower(family, link) == negative_infinity()
          && make_upper(family, link) == positive_infinity()) 
        eta += gamma[1];
      else if (family == 4 && link == 5) {
        real max_eta = max(eta);
        alpha[1] -= max_eta;
        eta += gamma[1] - max_eta;
      } else {
        real min_eta = min(eta);
        alpha[1] -= min_eta;
        eta += gamma[1] - min_eta;
      }
    } else {
      eta += dot_product(xbar, beta);
    }
    
    if (family == 4 && z_dim > 0 && link_phi > 0) {
      eta_z = betareg_z * omega;
    } else if (family == 4 && z_dim == 0 && has_intercept_z == 1) {
      eta_z = rep_vector(0.0, N);
    }
    
    if (has_intercept_z == 1) {
      if (link_phi > 1) {
        omega_int[1] -= min(eta_z);
        eta_z += gamma_z[1] - min(eta_z);
      } else {
        eta_z += gamma_z[1];
      }
    } else {
      if (link_phi > 1) {
        eta_z += dot_product(zbar, omega) - min(eta_z);
      } else {
        eta_z += dot_product(zbar, omega);
      }
    }
    
    if (SSfun > 0) {
      vector[len_y] eta_nlmer;
      matrix[len_y, K] P;
      P = reshape_vec(eta, len_y, K);
      if (SSfun < 5) {
        if (SSfun <= 2) {
          if (SSfun == 1) 
            eta_nlmer = SS_asymp(input, P);
          else 
            eta_nlmer = SS_asympOff(input, P);
        } else if (SSfun == 3) 
          eta_nlmer = SS_asympOrig(input, P);
        else 
          eta_nlmer = SS_biexp(input, P);
      } else {
        if (SSfun <= 7) {
          if (SSfun == 5) 
            eta_nlmer = SS_fol(Dose, input, P);
          else if (SSfun == 6) 
            eta_nlmer = SS_fpl(input, P);
          else 
            eta_nlmer = SS_gompertz(input, P);
        } else {
          if (SSfun == 8) 
            eta_nlmer = SS_logis(input, P);
          else if (SSfun == 9) 
            eta_nlmer = SS_micmen(input, P);
          else 
            eta_nlmer = SS_weibull(input, P);
        }
      }
      for (n in 1 : len_y) 
        mean_PPD += normal_rng(eta_nlmer[n], aux);
    } else if (family == 1) {
      vector[N] mu = link > 1 ? linkinv_gauss(eta, link) : eta;
      for (n in 1 : len_y) 
        mean_PPD += normal_rng(mu[n], aux);
    } else if (family == 2) {
      vector[N] mu = link > 1 ? linkinv_gamma(eta, link) : eta;
      for (n in 1 : len_y) 
        mean_PPD += gamma_rng(aux, aux / mu[n]);
    } else if (family == 3) {
      vector[N] mu = link > 1 ? linkinv_inv_gaussian(eta, link) : eta;
      for (n in 1 : len_y) 
        mean_PPD += inv_gaussian_rng(mu[n], aux);
    } else if (family == 4 && link_phi == 0) {
      vector[N] mu = linkinv_beta(eta, link);
      for (n in 1 : N) {
        real mu_n = mu[n];
        if (aux <= 0) 
          mean_PPD += bernoulli_rng(0.5);
        else if (mu_n >= 1) 
          mean_PPD += 1;
        else if (mu_n > 0) 
          mean_PPD += beta_rng(mu_n * aux, (1 - mu_n) * aux);
      }
    } else if (family == 4 && link_phi > 0) {
      vector[N] mu = linkinv_beta(eta, link);
      vector[N] phi = linkinv_beta_z(eta_z, link_phi);
      for (n in 1 : N) {
        real mu_n = mu[n];
        real aux_n = phi[n];
        if (aux_n <= 0) 
          mean_PPD += bernoulli_rng(0.5);
        else if (mu_n >= 1) 
          mean_PPD += 1;
        else if (mu_n > 0) 
          mean_PPD += beta_rng(mu_n * aux_n, (1 - mu_n) * aux_n);
      }
    }
    mean_PPD /= len_y;
  }
}

/*
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
    ┌─ ../continuous.stan:7:3
  6 │    #include </functions/common_functions.stan>
  7 │    #include </functions/continuous_likelihoods.stan>
    │    -------- file '..//functions/continuous_likelihoods.stan' included here
  8 │    
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
    ┌─ ../continuous.stan:7:3
  6 │    #include </functions/common_functions.stan>
  7 │    #include </functions/continuous_likelihoods.stan>
    │    -------- file '..//functions/continuous_likelihoods.stan' included here
  8 │    
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
    ┌─ ../continuous.stan:7:3
  6 │    #include </functions/common_functions.stan>
  7 │    #include </functions/continuous_likelihoods.stan>
    │    -------- file '..//functions/continuous_likelihoods.stan' included here
  8 │    
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
    ┌─ ../continuous.stan:7:3
  6 │    #include </functions/common_functions.stan>
  7 │    #include </functions/continuous_likelihoods.stan>
    │    -------- file '..//functions/continuous_likelihoods.stan' included here
  8 │    
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
    ┌─ ../continuous.stan:7:3
  6 │    #include </functions/common_functions.stan>
  7 │    #include </functions/continuous_likelihoods.stan>
    │    -------- file '..//functions/continuous_likelihoods.stan' included here
  8 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:1:8
  1 │  vector SS_asymp(vector input, matrix Phi_);
    │         ^^^^^^^^ Forward declaration.
  2 │  vector SS_asympOff(vector input, matrix Phi_);
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:2:8
  1 │  vector SS_asymp(vector input, matrix Phi_);
  2 │  vector SS_asympOff(vector input, matrix Phi_);
    │         ^^^^^^^^^^^ Forward declaration.
  3 │  vector SS_asympOrig(vector input, matrix Phi_);
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:3:8
  2 │  vector SS_asympOff(vector input, matrix Phi_);
  3 │  vector SS_asympOrig(vector input, matrix Phi_);
    │         ^^^^^^^^^^^^ Forward declaration.
  4 │  vector SS_biexp(vector input, matrix Phi_);
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:4:8
  3 │  vector SS_asympOrig(vector input, matrix Phi_);
  4 │  vector SS_biexp(vector input, matrix Phi_);
    │         ^^^^^^^^ Forward declaration.
  5 │  vector SS_fol(vector Dose, vector input, matrix Phi_);
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:5:8
  4 │  vector SS_biexp(vector input, matrix Phi_);
  5 │  vector SS_fol(vector Dose, vector input, matrix Phi_);
    │         ^^^^^^ Forward declaration.
  6 │  vector SS_fpl(vector input, matrix Phi_);
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:6:8
  5 │  vector SS_fol(vector Dose, vector input, matrix Phi_);
  6 │  vector SS_fpl(vector input, matrix Phi_);
    │         ^^^^^^ Forward declaration.
  7 │  vector SS_gompertz(vector x, matrix Phi_);
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:7:8
  6 │  vector SS_fpl(vector input, matrix Phi_);
  7 │  vector SS_gompertz(vector x, matrix Phi_);
    │         ^^^^^^^^^^^ Forward declaration.
  8 │  vector SS_logis(vector input, matrix Phi_);
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:8:8
  7 │  vector SS_gompertz(vector x, matrix Phi_);
  8 │  vector SS_logis(vector input, matrix Phi_);
    │         ^^^^^^^^ Forward declaration.
  9 │  vector SS_micmen(vector input, matrix Phi_);
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:9:8
  8 │  vector SS_logis(vector input, matrix Phi_);
  9 │  vector SS_micmen(vector input, matrix Phi_);
    │         ^^^^^^^^^ Forward declaration.
 10 │  vector SS_weibull(vector x, matrix Phi_);
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
warning: Functions do not need to be declared before definition; all user defined
function names are always in scope regardless of definition order.
    ┌─ ..//functions/SSfunctions.stan:10:8
  9 │  vector SS_micmen(vector input, matrix Phi_);
 10 │  vector SS_weibull(vector x, matrix Phi_);
    │         ^^^^^^^^^^ Forward declaration.
 11 │  
    ┌─ ../continuous.stan:9:3
  8 │    
  9 │    #include </functions/SSfunctions.stan>
    │    -------- file '..//functions/SSfunctions.stan' included here
 10 │    
*/
