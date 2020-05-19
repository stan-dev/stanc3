functions {
    real foo(real(real) g) {
        functions
        real f(real x) {
            return g(x)^2;
        }
        return f(1.0) + f(2.0);
    }
}