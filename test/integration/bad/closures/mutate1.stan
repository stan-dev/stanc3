model {
    real x = 0.0;
    functions
    real foo(real y) {
        x = 2.0;
        return x + y;
    }
}