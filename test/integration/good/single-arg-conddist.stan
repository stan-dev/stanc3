functions {
    real foo_lpdf(real x) {
        return -square(x);
    }
    real goo_lpmf(int x) {
        return -square(x);
    }
}
data {
    int y;
}
parameters {
    real x;
}
model {
    target += std_normal_lpdf(x);
    target += std_normal_lpdf(x|);
    target += std_normal_lupdf(x);
    target += std_normal_lupdf(x|);
    target += foo_lpdf(x);
    target += foo_lpdf(x|);
    target += goo_lpmf(y);
    target += goo_lpmf(y|);
    target += goo_lupmf(y);
    target += goo_lupmf(y|);
}
