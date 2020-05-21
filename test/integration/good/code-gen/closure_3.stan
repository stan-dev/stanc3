functions {
    real foo(real(real) g, real x) {
        functions
        real g2(real y) {
            return g(y) * x;
        }
        return g2(1.0);
    }
}
transformed data {
    real x = 2.0;
    functions
    real bar(real y) {
        return x*y;
    }
    real z = foo(bar, 1.0);
}
parameters {
    real p;
}
transformed parameters {
    functions
    real baz(real y) {
        return p*y;
    }
    real w = foo(baz, 1.0);
}