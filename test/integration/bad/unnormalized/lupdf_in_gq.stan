parameters {
    real y;
}
model {
    y ~ normal(0, 1);
}
generated quantities {
    real x = normal_lupdf(y| 0, 1);
}