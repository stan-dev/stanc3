functions {
    real foo_lpdf(real x) {
        return -square(x);
    }
}
parameters {
    real x;
}
model {
    target += std_normal_lpdf(x);
    target += std_normal_lpdf(x|);
    target += foo_lpdf(x);
    target += foo_lpdf(x|);
}