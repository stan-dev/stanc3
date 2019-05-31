functions {
  real udf1(real a, real b) { return a + b;}
}
data {
  int N;
  int K;
  vector<lower=0>[N] datavar[K];
}
transformed data {
  real<lower=0> tdatavar[1, 1] = {{2.0}};
}
parameters {
  vector<lower=0>[N] param[K];
  matrix<lower=0>[N,N] matparam[K,K+1];
}
transformed parameters {
  vector<lower=0>[N] tparam[K];
  for (n in 1:N) tparam[n] = param[n] * 2;
}
model {
  vector[N] modellocal[K];
  for (n in 1:N) modellocal[n] = tparam[n] * 2;
  target += normal_lpdf(0 | 0, 1); // check <propto_>
}
generated quantities {
  vector<lower=0>[N] gq[K];
  for (n in 1:N) gq[n] = tparam[n] + param[n];
}
