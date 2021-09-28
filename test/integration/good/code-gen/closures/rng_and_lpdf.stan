parameters {
    real s;
    real k;
}
transformed parameters {
    function
    real closure_dist_rng(real x) {
        return normal_rng(x,1);
    }
    function
    real closure_dist_lpdf(real y, real x) {
        return normal_lupdf(y|x,1);
    }
}
model {
    target += closure_dist_lpdf(s|k);
    target += closure_dist_lupdf(s|k);
}
generated quantities {
    real m = closure_dist_rng(k);
}