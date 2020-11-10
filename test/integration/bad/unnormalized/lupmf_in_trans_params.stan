data {
    int d;
}
transformed parameters {
    real y;
    real x = poisson_lupmf(d | y);
}