model {
    real x = 0.0;
    function
    real foo(real y) {
        x = 2.0;
        return x + y;
    }
}