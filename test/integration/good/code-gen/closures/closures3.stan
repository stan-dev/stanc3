functions {
    real foo(real(real) g, real x) {
        function
        real g2(real y) {
            return g(y) * x;
        }
        return g2(1.0);
    }
    real foo2(real x) {
        return x;
    }
}
transformed data {
    real x = foo(foo2, 2.0);
    function
    real bar(real y) {
        return x*y;
    }
    real z = foo(bar, 1.0);
}
parameters {
    real p;
}
transformed parameters {
    function
    real baz(real y) {
        return p*y;
    }
    real w = foo(baz, 1.0);
}