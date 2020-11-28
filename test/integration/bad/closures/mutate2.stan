model {
    real x = 0.0;
    functions
    real foo(real y) {
        return x + y;
    }
    x = 2.0;
}