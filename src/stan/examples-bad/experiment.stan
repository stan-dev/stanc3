transformed data {
  int N = 5;
}
parameters {
  ordered[2] mu[N];
}
model {
   f(mu, [-0.5,0.5]', 1.0);
}