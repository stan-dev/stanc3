transformed parameters {
    functions
    real foo_lpdf(real x, real y) {
        return normal_lpdf(x|y,1);
    }
}