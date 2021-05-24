parameters {
    real s;
    real k;
}
transformed parameters {
    functions
    real foo_rng(real x) {
        return normal_rng(x,1);
    }
    functions
    real foo_lpdf(real y, real x) {
        return normal_lupdf(y|x,1);
    }
}
model {
    target += foo_lpdf(s|k);
    target += foo_lupdf(s|k);
}
generated quantities {
    real m = foo_rng(k);
}