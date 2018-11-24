transformed data {
  int M =5;
}
parameters {
  ordered[2] mu[N];
}
model {
  mu ~ normal(-[0.5,0.5]', 1.0);
}