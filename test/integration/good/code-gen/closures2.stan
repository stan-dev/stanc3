transformed data {
    functions
    real foo(real x) {
        return x;
    }
    functions
    real bar(real y) {
        return foo(y);
    }
}
parameters {
    real alpha;
}
transformed parameters {
    functions
    real baz(real y) {
        return foo(y);
    }
    functions
    real goo(real s) {
        functions
        real gar(real b) {
            return b;
        }
        return gar(s);
    }
    real s1 = bar(1.0);
    real s2 = baz(1.0);
    real s3 = goo(1.0);
}