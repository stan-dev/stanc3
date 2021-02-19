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
    target += reduce_sum(foo, x, 1);
}