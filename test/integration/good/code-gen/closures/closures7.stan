functions {
    real ff_lpdf(real x) {
        real s = std_normal_lupdf(0|);
        return x;
    }
    real ff_rng(real x) {
        real s = std_normal_rng();
        return x;
    }
    real ff_lp(real x) {
        real s = target();
        return x;
    }
}
transformed data {
    functions
    void hof(real(real) s_rng,real(real) s_lpdf, real(real) s_lp) {}
    hof(ff_rng,ff_lpdf,ff_lp);

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