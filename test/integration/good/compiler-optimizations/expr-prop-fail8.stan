data {
  int<lower=0> N;
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=N> node1; // node1[i] adjacent to node2[i]
  array[N_edges] int<lower=1, upper=N> node2; // and node1[i] < node2[i]
  
  array[N] int<lower=0> y; // count outcomes
  vector[N] x; // predictor
}
parameters {
  real beta0; // intercept
  real beta1; // slope
  
  real<lower=0> tau_theta; // precision of heterogeneous effects
  real<lower=0> tau_phi; // precision of spatial effects
  
  vector[N] theta_std; // standardized heterogeneous effects
  vector[N] phi_std_raw; // raw, standardized spatial effects
}
transformed parameters {
  real<lower=0> sigma_phi = inv(sqrt(tau_phi));
  vector[N] phi;
  phi[1 : N] = phi_std_raw;
  phi = phi * sigma_phi;
}
model {
  target += dot_self(phi[node1]);
}

