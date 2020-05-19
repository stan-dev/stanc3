functions {
    real foo(real x) {
        return 2*x;
    }
    real(real) bar(real x) {
        return foo;
    }
}