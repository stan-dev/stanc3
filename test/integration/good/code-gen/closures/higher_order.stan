functions {
    real higher_order_func(real(real) g, real x) {
        function
        real g2(real y) {
            return g(y) * x;
        }
        return g2(1.0);
    }
    real func(real x) {
        return x;
    }
}
transformed data {
    real x = higher_order_func(func, 2.0);
    function
    real data_closure(real y) {
        return x*y;
    }
    real z = higher_order_func(data_closure, 1.0);
}
parameters {
    real p;
}
transformed parameters {
    function
    real closure(real y) {
        return p*y;
    }
    real w = higher_order_func(closure, 1.0);
}