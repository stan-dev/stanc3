model {
    real x = 0.0;
    function
    real foo(real y) {
        return x + y;
    }
    x = 2.0;
}