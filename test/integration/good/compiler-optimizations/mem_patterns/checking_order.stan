data {
  int N;
}
parameters {
  real alpha;
  vector[N] beta;
}
model{
  vector[N] mu = alpha + rep_vector(0.0, N) + beta;
  target += sum(mu);
}
