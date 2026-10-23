/* $ stanc --include-paths=.. --auto-format --allow-undefined --canonicalize=strip-comments,includes ../polr.stan : [exit 0] */
functions {
  real CDF_polr(real x, int link) {
    if (link == 1) 
      return (inv_logit(x));
    else if (link == 2) 
      return (Phi(x));
    else if (link == 3) 
      return (gumbel_cdf(x | 0, 1));
    else if (link == 4) 
      return (inv_cloglog(x));
    else if (link == 5) 
      return (cauchy_cdf(x | 0, 1));
    else 
      reject("Invalid link");
    return x;
  }
  
  vector pw_polr(array[] int y, vector eta, vector cutpoints, int link,
                 real alpha) {
    int N = rows(eta);
    int J = rows(cutpoints) + 1;
    vector[N] ll;
    if (link < 1 || link > 5) 
      reject("Invalid link");
    
    if (alpha == 1) 
      for (n in 1 : N) {
        if (y[n] == 1) 
          ll[n] = CDF_polr(cutpoints[1] - eta[n], link);
        else if (y[n] == J) 
          ll[n] = 1 - CDF_polr(cutpoints[J - 1] - eta[n], link);
        else 
          ll[n] = CDF_polr(cutpoints[y[n]] - eta[n], link)
                  - CDF_polr(cutpoints[y[n] - 1] - eta[n], link);
      }
    else 
      for (n in 1 : N) {
        if (y[n] == 1) 
          ll[n] = CDF_polr(cutpoints[1] - eta[n], link) ^ alpha;
        else if (y[n] == J) 
          ll[n] = 1 - CDF_polr(cutpoints[J - 1] - eta[n], link) ^ alpha;
        else 
          reject("alpha not allowed with more than 2 outcome categories");
      }
    return log(ll);
  }
  
  vector make_cutpoints(vector probabilities, real scale, int link) {
    int C = rows(probabilities) - 1;
    vector[C] cutpoints;
    real running_sum = 0;
    
    if (link == 1) 
      for (c in 1 : C) {
        running_sum += probabilities[c];
        cutpoints[c] = logit(running_sum);
      }
    else if (link == 2) 
      for (c in 1 : C) {
        running_sum += probabilities[c];
        cutpoints[c] = inv_Phi(running_sum);
      }
    else if (link == 3) 
      for (c in 1 : C) {
        running_sum += probabilities[c];
        cutpoints[c] = -log(-log(running_sum));
      }
    else if (link == 4) 
      for (c in 1 : C) {
        running_sum += probabilities[c];
        cutpoints[c] = log(-log1m(running_sum));
      }
    else if (link == 5) 
      for (c in 1 : C) {
        running_sum += probabilities[c];
        cutpoints[c] = tan(pi() * (running_sum - 0.5));
      }
    else 
      reject("invalid link");
    return scale * cutpoints;
  }
  
  real draw_ystar_rng(real lower_, real upper_, real eta, int link) {
    int iter = 0;
    real ystar = not_a_number();
    if (lower_ >= upper_) 
      reject("lower_ must be less than upper_");
    
    if (link == 1) 
      while (!(ystar > lower_ && ystar < upper_)) ystar = logistic_rng(eta,
                                                                    1);
    else if (link == 2) 
      while (!(ystar > lower_ && ystar < upper_)) ystar = normal_rng(eta, 1);
    else if (link == 3) 
      while (!(ystar > lower_ && ystar < upper_)) ystar = gumbel_rng(eta, 1);
    else if (link == 4) 
      while (!(ystar > lower_ && ystar < upper_)) ystar = log(
                                                              -log1m(
                                                                    uniform_rng
                                                                    (0, 1)));
    else if (link == 5) 
      while (!(ystar > lower_ && ystar < upper_)) ystar = cauchy_rng(eta, 1);
    else 
      reject("invalid link");
    return ystar;
  }
  
  vector csr_matrix_times_vector2(int m, int n, vector w, array[] int v,
                                  array[] int u, vector b);
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
  
  int<lower=2> J;
  array[N] int<lower=1, upper=J> y;
  
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
  
  real<lower=0> regularization;
  vector<lower=0>[J] prior_counts;
  int<lower=0, upper=1> is_skewed;
  real<lower=0> shape;
  real<lower=0> rate;
  int<lower=0, upper=1> do_residuals;
}
transformed data {
  real<lower=0> half_K = 0.5 * K;
  real<lower=0> sqrt_Nm1 = sqrt(N - 1.0);
  int<lower=0, upper=1> is_constant = 1;
  vector[0] beta_smooth;
  for (j in 1 : J) 
    if (prior_counts[j] != 1) 
      is_constant = 0;
}
parameters {
  simplex[J] pi;
  array[K > 1] unit_vector[K] u;
  real<lower=(K > 1 ? 0 : -1), upper=1> R2;
  array[is_skewed] real<lower=0> alpha;
}
transformed parameters {
  vector[K] beta;
  vector[J - 1] cutpoints;
  {
    real Delta_y;
    if (K > 1) {
      Delta_y = inv_sqrt(1 - R2);
      beta = u[1] * sqrt(R2) * Delta_y * sqrt_Nm1;
    } else {
      Delta_y = inv_sqrt(1 - square(R2));
      beta[1] = R2 * Delta_y * sqrt_Nm1;
    }
    cutpoints = make_cutpoints(pi, Delta_y, link);
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
  
  if (has_weights == 0 && prior_PD == 0) {
    if (is_skewed == 0) 
      target += pw_polr(y, eta, cutpoints, link, 1.0);
    else 
      target += pw_polr(y, eta, cutpoints, link, alpha[1]);
  } else if (prior_PD == 0) {
    if (is_skewed == 0) 
      target += dot_product(weights, pw_polr(y, eta, cutpoints, link, 1.0));
    else 
      target += dot_product(weights,
                            pw_polr(y, eta, cutpoints, link, alpha[1]));
  }
  
  if (is_constant == 0) 
    target += dirichlet_lpdf(pi | prior_counts);
  
  if (prior_dist == 1) {
    if (K > 1) 
      target += beta_lpdf(R2 | half_K, regularization);
    else 
      target += beta_lpdf(square(R2) | half_K, regularization) + log(abs(R2));
  }
  if (is_skewed == 1) 
    target += gamma_lpdf(alpha | shape, rate);
}
generated quantities {
  vector[J > 2 ? J : 1] mean_PPD = rep_vector(0, J > 2 ? J : 1);
  vector[do_residuals ? N : 0] residuals;
  vector[J - 1] zeta;
  
  if (dense_X) 
    zeta = cutpoints + dot_product(xbar, beta);
  else 
    zeta = cutpoints;
  if (J == 2) 
    zeta *= -1.0;
  {
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
    
    for (n in 1 : N) {
      int y_tilde;
      vector[J] theta;
      real previous;
      real first = CDF_polr(cutpoints[1] - eta[n], link);
      previous = first;
      if (is_skewed) 
        theta[1] = first ^ alpha[1];
      else 
        theta[1] = first;
      for (j in 2 : (J - 1)) {
        real current = CDF_polr(cutpoints[j] - eta[n], link);
        theta[j] = current - previous;
        previous = current;
      }
      if (is_skewed == 0) 
        theta[J] = 1 - previous;
      else 
        theta[J] = 1 - previous ^ alpha[1];
      if (previous <= 0 || previous >= 1) {
        
      } else if (J == 2) {
        mean_PPD[1] += bernoulli_rng(theta[J]);
      } else {
        y_tilde = categorical_rng(theta);
        mean_PPD[y_tilde] += 1;
      }
      
      if (do_residuals) {
        real ystar;
        if (y[n] == 1) 
          ystar = draw_ystar_rng(negative_infinity(), cutpoints[1], eta[n],
                                 link);
        else if (y[n] == J) 
          ystar = draw_ystar_rng(cutpoints[J - 1], positive_infinity(),
                                 eta[n], link);
        else 
          ystar = draw_ystar_rng(cutpoints[y[n] - 1], cutpoints[y[n]],
                                 eta[n], link);
        residuals[n] = ystar - eta[n];
      }
    }
    mean_PPD /= (N + 0.0);
  }
}

/*
warning: Unreachable statement found, is this intended?
    ┌─ ../polr.stan:28:5
 16 │      if (link == 1)
 17 │        return (inv_logit(x));
    │        ---------------------- exits the function
 18 │      else if (link == 2)
 19 │        return (Phi(x));
    │        ---------------- exits the function
 20 │      else if (link == 3)
 21 │        return (gumbel_cdf(x | 0, 1));
    │        ------------------------------ exits the function
 22 │      else if (link == 4)
 23 │        return (inv_cloglog(x));
    │        ------------------------ exits the function
 24 │      else if (link == 5)
 25 │        return (cauchy_cdf(x | 0, 1));
    │        ------------------------------ exits the function
 26 │      else
 27 │        reject("Invalid link");
    │        ----------------------- stops this model evaluation
 28 │      return x; // never reached
    │      ^ Never reached
 29 │    }
*/
