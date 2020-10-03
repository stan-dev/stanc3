data {
    int d;
}
parameters {
    real y;
}
model {
    y ~ normal(0, 1);
}
generated quantities {
    real x = poisson_lupmf(d | y);
}