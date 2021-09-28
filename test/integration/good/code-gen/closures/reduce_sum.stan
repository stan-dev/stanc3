data {
    int N;
}
parameters {
    real mu;
    real x[N];
}
model {
    function
    real partial_sum_closure(real[] slice, int start, int end) {
        return normal_lpdf(slice|mu,1);
    }
    function
    real closure_dist_lpdf(real[] slice, int start, int end) {
        return normal_lupdf(slice|mu, 1);
    }
    target += reduce_sum(partial_sum_closure, x, 1);
    target += reduce_sum(closure_dist_lpdf, x, 1);
    target += reduce_sum(closure_dist_lupdf, x, 1);
}