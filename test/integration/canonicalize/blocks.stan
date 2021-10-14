data {
  int N;
  vector[N] y;
}
parameters {
   real mu;
   real<lower=0> sigma;
}
model {
    int j = 0;

    if (N>5) y ~ normal(mu, sigma);
    else y ~ student_t(5, mu, sigma);

    if (N>5) y ~ normal(mu, sigma);

    for (i in 1:N) target += i;
    
    while(j<N) j = j+1;
}