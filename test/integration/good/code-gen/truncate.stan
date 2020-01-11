data {
    int n;
    real x;
}
parameters {
    real m;
    real<lower=0> y;
}
model {
    x ~ normal(m,1) T[,];
    x ~ normal(m,1) T[0.0,];
    x ~ normal(m,1) T[,10.0];
    x ~ normal(m,1) T[0.0,10.0];
    n ~ poisson(y) T[,];
    n ~ poisson(y) T[10,];
    n ~ poisson(y) T[,20];
    n ~ poisson(y) T[10,20];
}