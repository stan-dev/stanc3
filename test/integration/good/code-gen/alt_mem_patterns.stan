data {
  int N;
  int Nr;
  int K;
  matrix[N, K] X;
  vector[N] low_high_diff;
  matrix[2, N] low_high_real;
}


parameters {
  vector[N] Ys_p;
  vector[K] bQ_model;  // regression coefficients at QR scale
  vector[K] bQ_Y_mean;
  vector[K] bQ_Y_sigma;
  vector[Nr] h;
  vector[2] ar;
  real ma;
  real<lower=0,upper=1> phi_beta;
  real<lower=0> sigma2;
  real Intercept;
}

transformed parameters {
  vector[N] mu_Ys = X * bQ_Y_mean;
  vector[N] sigma_Ys =  exp(X * bQ_Y_sigma);
  vector[N] Y_tmp = Ys_p .* sigma_Ys + mu_Ys;
  vector[N] Ys = (low_high_diff) .* inv_logit(Y_tmp) + low_high_real[, 1] ;
  vector[Nr] Ys_diff = (Ys[2:N] - Ys[1:(N - 1)]) ./ Ys[1:(N - 1)];
  real<lower=-1,upper=1> phi = fma(phi_beta, 2.0, -1.0);
  real<lower=0> sigma = sqrt(sigma2);
  vector[Nr] mu = Intercept + X * bQ_model;

  vector[Nr] err;  // actual residuals
  // include ARMA terms
  err[1] = Ys_diff[1] - mu[1];
  mu[2] += err[1] * ma + Ys_diff[1] * ar[1];
  err[2] = Ys_diff[2] - mu[2];
  for (n in 3:Nr) {
    mu[n] += err[n - 1] * ma + dot_product(Ys_diff[(n - 2):(n - 1)], ar);
    err[n] = Ys_diff[n] - mu[n];
  }


  vector[Nr] h_i_mean;
  vector<lower=0>[Nr] h_i_sigma;
  h_i_mean[1] = 0;
  h_i_sigma[1] = pow(1 - square(phi), -0.5);
  h_i_sigma[2:Nr] = rep_vector(1, (Nr - 1));
  h_i_mean[2:Nr] = phi * (h[1:(Nr-1)] - mu[2:Nr]);
  vector[Nr] h_sigma = exp(h * (0.5 * sigma) + (0.5 * mu));
}

model {
  Intercept ~ normal(0, 1);
  phi_beta ~ beta(0, 1);
  sigma2 ~ gamma(0, 1);
  ar ~ normal(0, .2);
  ma ~ normal(0, .2);
  h ~ normal(h_i_mean, h_i_sigma);
  sigma_Ys ~ student_t(10, 0, 5);
  Ys_p ~ std_normal();
  Ys_diff ~ normal(mu, h_sigma);
}
