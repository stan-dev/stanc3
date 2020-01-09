data {
    real x;
    real y;
}
parameters {
    real s;
}
model {
    real z = (y < x) ? x : (1 < y ? y : 1);
    real v = z*(z+x^2);
    real u = (y < 1 && x < 1) * -x;
    s ~ normal(u,v);
}
