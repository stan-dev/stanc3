transformed data {
    functions
    real foo_rng(real(real) bar_lpdf) {
        return bar_lpdf(1);
    }
    functions
    real foo_lpdf(real zz, real(real) bar_lpdf) {
        return bar_lupdf(1);
    }
    functions
    real foo_lp(real(real) bar_lp) {
        return bar_lp(1);
    }
    functions
    real f2(real x) {
        return x;
    }
    functions
    real f3_lpdf(real x) {
        return x;
    }
    functions
    real f4_rng(real x) {
        return x;
    }
    functions
    real f5_lp(real x) {
        return x;
    }
    real z = foo_rng(f3_lpdf);
}
transformed parameters {
    real s = foo_lp(f5_lp);
}
model {
    target += foo_lupdf(1|f3_lupdf);
}