functions {
    real foo_lpdf(real x, real mu) {
        return normal_lupdf(x|mu,1);
    }
    real bar_lpmf(int n, real mu) {
        return poisson_lupmf(n|mu);
    }
    real baz_lpdf(real x) {
        return foo_lupdf(x|0.5);
    }
}
data {
    int n;
}
parameters {
    real mu;
}
transformed parameters {
    real tp = foo_lpdf(mu| 1.0);
}
model {
    target += baz_lupdf(mu|);
    target += bar_lupmf(n| mu);
}
generated quantities {
    real lbaz = baz_lpdf(mu|);
    real lbar = bar_lpmf(n|mu);
}