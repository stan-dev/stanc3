functions {
    void foo_lp(int i1, real r1) {
        target += poisson_lupmf(i1| r1);
    }
}
data {
    int i1;
}
parameters {
    real r1;
}
transformed parameters {
    foo_lp(i1, r1, m1, v1, ai1, rv1);
}