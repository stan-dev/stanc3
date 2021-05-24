data {
    int N;
}
parameters {
    real mu;
    real x[N];
}
model {
    functions
    real foo(real[] slice, int start, int end) {
        return normal_lpdf(slice|mu,1);
    }
    functions
    real bar_lpdf(real[] slice, int start, int end) {
        return normal_lupdf(slice|mu, 1);
    }
    target += reduce_sum(foo, x, 1);
    target += reduce_sum(bar_lpdf, x, 1);
    target += reduce_sum(bar_lupdf, x, 1);
}