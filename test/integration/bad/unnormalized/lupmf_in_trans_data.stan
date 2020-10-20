
data {
    int d;
}
transformed data {
    real y;
    real x = poisson_lupmf(d | y);
}