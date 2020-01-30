

functions {
  // runs reduce over the index range start to end. Mapping from
  // data-index set to group indices pre-stored in gidx
  real hierarchical_reduce(int start, int end, int[] y_slice, real[] log_lambda_group, int[] gidx) {
    return poisson_log_lpmf(y_slice| log_lambda_group[gidx[start:end]]);   
  }
}

data {
  int<lower=0> N;
  int<lower=0> G;
  int<lower=0> grainsize;
} 
transformed data {
  real true_log_lambda = log(5.0); 
  real true_tau = log(10)/1.96;
  int y[N*G];
  int yg[G,N];
  real xr[G,0];
  int gidx[N*G]; 
  int gsidx[G+1]; 
  int start[G];
  int end[G];
  int group[G];
  vector[0] theta_dummy;

  print("Simulating problem size: G = ", G, "; N = ", N);
  print("Using parallel reduce TBB with grainsize = ", grainsize);

  for(g in 1:G) {
    real lambda_group = lognormal_rng(true_log_lambda, true_tau);
    int elem = 1;
    group[g] = g;
    start[g] = (g-1) * N + 1;
    end[g] = g * N;
    gsidx[g] = start[g];
    gsidx[g+1] = end[g]+1;
    for(i in start[g]:end[g]) {
      y[i] = poisson_rng(lambda_group);
      yg[g,elem] = y[i];
      gidx[i] = g;
      elem += 1; 
    }
  }
}

parameters { 
  real log_lambda;
  real<lower=0> tau; 
  vector[G] eta;
}
model {
  real log_lambda_group[G] = to_array_1d(log_lambda + eta * tau);

  real lpmf = reduce_sum(hierarchical_reduce, y, grainsize, log_lambda_group, gidx);

  target += lpmf;
  target += normal_lpdf(log_lambda|0,1);
  target += normal_lpdf(tau|0,1);
  target += normal_lpdf(eta|0,1);
}

generated quantities {
  real msq_log_lambda = square(true_log_lambda - log_lambda);
  real msq_tau = square(true_tau - tau);
}
